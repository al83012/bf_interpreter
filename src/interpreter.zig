pub const std = @import("std");
pub const BoundedArray = std.BoundedArray;
pub const ArrayList = std.ArrayList;
pub const Allocator = std.mem.Allocator;
// pub const Channel = @import("channel.zig").Channel;
pub const BufferedChannel = @import("channel.zig").BufferedChannel;
pub const Writer = std.io.Writer;
pub const Reader = std.io.Reader;
pub const HashMap = std.AutoHashMap;
pub const SizeConstraint = union(enum) {
    sized: usize,
    dynamic: Allocator,
};

var GPA = std.heap.GeneralPurposeAllocator(.{}){};
const alloc = GPA.allocator();

var _arr_list = ArrayList(u8).init(std.testing.allocator);
const DefaultIoWriter = @TypeOf(_arr_list.writer());

pub const InterpreterConfig = struct {
    const Self = @This();

    max_program_buffer_size: SizeConstraint = .{ .dynamic = alloc },
    reader_buf_size: usize = 4096,
    instr_alloc: Allocator = alloc,
    jump_alloc: Allocator = alloc,
    channel_alloc: Allocator = alloc,
    IoWriter: type = DefaultIoWriter,
    IoReader: type = @TypeOf(std.io.getStdIn().reader()),
};

pub const InstructionExecutionError = error{
    UnexpectedJumpBack,
    MovedPointerOutOfRange,
    UnmatchedOpenBracket,
    FurthestMatchedInstructionCorrupted,
};

pub const InstructionParserError = error{
    UnmatchedOpenBracket,
    UnmatchedClosedBracket,
    UnsupportedChar,
};

pub const ReducedInstruction = union(enum) {
    MovRight: usize,
    MovLeft: usize,
    JumpPoint,
    JumpBackTo,
    Incr: u8,
    Decr: u8,
    Zero,
    EOP,
    ReadIo,
    WriteIo,

    const Self = @This();

    pub fn isEOP(self: Self) bool {
        return switch (self) {
            ReducedInstruction.EOP => true,
            else => false,
        };
    }

    pub fn deepEqual(a: ReducedInstruction, b: ReducedInstruction) bool {
        return switch (a) {
            .MovRight => |val_a| switch (b) {
                .MovRight => |val_b| val_a == val_b,
                else => false,
            },
            .MovLeft => |val_a| switch (b) {
                .MovLeft => |val_b| val_a == val_b,
                else => false,
            },
            .Incr => |val_a| switch (b) {
                .Incr => |val_b| val_a == val_b,
                else => false,
            },
            .Decr => |val_a| switch (b) {
                .Decr => |val_b| val_a == val_b,
                else => false,
            },
            .JumpPoint => switch (b) {
                .JumpPoint => true,
                else => false,
            },
            .JumpBackTo => switch (b) {
                .JumpBackTo => true,
                else => false,
            },
            .Zero => switch (b) {
                .Zero => true,
                else => false,
            },
            .EOP => switch (b) {
                .EOP => true,
                else => false,
            },
            .ReadIo => switch (b) {
                .ReadIo => true,
                else => false,
            },
            .WriteIo => switch (b) {
                .WriteIo => true,
                else => false,
            },
        };
    }

    fn format(
        self: *Self,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;

        switch (self) {
            .MovRight => |count| {
                try writer.print("MovRight(", .{});
                try writer.print("{})", .{count});
            },

            .MovLeft => |count| {
                try writer.print("MovLeft(", .{});
                try writer.print("{})", .{count});
            },

            .Incr => |count| {
                try writer.print("Incr(", .{});
                try writer.print("{})", .{count});
            },

            .Decr => |count| {
                try writer.print("Decr(", .{});
                try writer.print("{})", .{count});
            },
            .JumpPoint => {
                try writer.print("JumpPoint", .{});
            },
            .JumpBackTo => |to| {
                try writer.print("JumpBackTo(");
                try writer.print("{})", .{to});
            },
            else => @panic("todo"),
        }

        try writer.writeAll(")");
    }
};

pub fn CharReader(comptime buf_size: usize) type {
    return struct {
        reader: std.io.BufferedReader(buf_size, std.fs.File.Reader),
        peeked: ?u8 = null,
        const Self = @This();

        pub fn new(file: std.fs.File) Self {
            return Self{
                .reader = std.io.bufferedReader(file.reader()),
                .peeked = null,
            };
        }

        pub fn readChar(self: *Self) !?u8 {
            if (self.peeked) |ch| {
                self.peeked = null;
                return ch;
            } else {
                return try self.readIgnoreWhitespace();
            }
        }

        pub fn peekChar(self: *Self) !?u8 {
            if (self.peeked) |ch| {
                return ch;
            } else {
                const ch = try self.readIgnoreWhitespace();
                self.peeked = ch;
                return ch;
            }
        }

        pub fn markPeekRead(self: *Self) void {
            self.peeked = null;
        }

        fn readIgnoreWhitespace(self: *Self) !?u8 {
            while (true) {
                const c = self.reader.reader().readByte() catch |err| switch (err) {
                    error.EndOfStream => return null,
                    else => |e| return e,
                };
                if (!std.ascii.isWhitespace(c)) {
                    return c;
                }
            }
            return null;
        }
    };
}

pub const InstructionChannel: type = BufferedChannel(ReducedInstruction, 1024);

pub fn InstructionWriter(comptime file_reader_buf_size: usize) type {
    return struct {
        channel: *InstructionChannel,
        // The peek buffer size is 2 as the longest strictly structured instruction would be the zero instruction with 3 characters
        // --> Read, Peek1, Peek2
        char_reader: Self.Reader,
        indent_counter: usize,

        const Self = @This();
        const Reader = PeekableFileReader(file_reader_buf_size, 2);

        pub fn init(file: std.fs.File, channel: *InstructionChannel) Self {
            return .{
                .channel = channel,
                .char_reader = Self.Reader.init(file),
                .indent_counter = 0,
            };
        }

        fn genNextInstruction(self: *Self) !ReducedInstruction {
            const next_char = try self.char_reader.read() orelse if (self.indent_counter == 0) return .EOP else return InstructionParserError.UnmatchedOpenBracket;
            switch (next_char) {
                '<' => {
                    var count: usize = 1;
                    while (try self.char_reader.peek()) |peek| {
                        if (peek == '<') {
                            count += 1;
                            self.char_reader.confirmPeeks();
                        } else {
                            break;
                        }
                    }
                    return ReducedInstruction{ .MovLeft = count };
                },

                '>' => {
                    var count: usize = 1;
                    while (try self.char_reader.peek()) |peek| {
                        if (peek == '>') {
                            count += 1;
                            self.char_reader.confirmPeeks();
                        } else {
                            break;
                        }
                    }
                    return ReducedInstruction{ .MovRight = count };
                },

                '+' => {
                    var count: u8 = 1;
                    while (try self.char_reader.peek()) |peek| {
                        if (peek == '+') {
                            // Keeping it as a u8 is fine since it will wrap in the program code anyways
                            count +%= 1;
                            self.char_reader.confirmPeeks();
                        } else {
                            break;
                        }
                    }
                    return ReducedInstruction{ .Incr = count };
                },

                '-' => {
                    var count: u8 = 1;
                    while (try self.char_reader.peek()) |peek| {
                        if (peek == '-') {
                            count +%= 1;
                            self.char_reader.confirmPeeks();
                        } else {
                            break;
                        }
                    }
                    return ReducedInstruction{ .Decr = count };
                },
                '[' => {
                    const peek1 = try self.char_reader.peek();
                    if ((peek1 == '-' or peek1 == '+') and try self.char_reader.peek() == ']') {
                        self.char_reader.confirmPeeks();
                        return ReducedInstruction.Zero;
                    } else {
                        self.indent_counter += 1;
                        return ReducedInstruction.JumpPoint;
                    }
                },
                ']' => {
                    if (self.indent_counter > 0) {
                        self.indent_counter -= 1;
                    } else {
                        return InstructionParserError.UnmatchedClosedBracket;
                    }
                    return ReducedInstruction.JumpBackTo;
                },
                ',' => {
                    return ReducedInstruction.ReadIo;
                },
                '.' => {
                    return ReducedInstruction.WriteIo;
                },
                else => return InstructionParserError.UnsupportedChar,
            }
        }

        fn sendAllInstructions(self: *Self) !void {
            while (true) {
                const next_instruction = try self.genNextInstruction();
                // std.debug.print("[WRITE] next_instruction = {any}", .{next_instruction});
                try self.channel.send(next_instruction);
                if (next_instruction.isEOP()) {
                    break;
                }
            }
        }

        pub fn sendAllPanic(self: *Self) void {
            self.sendAllInstructions() catch |err| {
                std.debug.panic("Sending panicked with error {any}", .{err});
            };
        }
    };
}

pub fn InstructionReader(buffer_size: SizeConstraint, OutWriter: type, InReader: type) type {
    return struct {
        channel: *InstructionChannel,
        local_instructions: ArrayList(ReducedInstruction),
        instruction_pos: usize,
        read_from_channel: bool,
        pointer_pos: usize,
        jump_points: HashMap(usize, usize), // Jump from bracket pos to bracket pos
        unmatched_open: ArrayList(usize),
        buffer: ArrayList(u8),
        out: OutWriter,
        in: InReader,
        furthest_matched_instruction: usize,

        const Self = @This();

        pub fn init(channel: *InstructionChannel, instruction_alloc: Allocator, jump_alloc: Allocator, out_writer: OutWriter, in_reader: InReader) Self {
            const local_instructions = ArrayList(ReducedInstruction).init(instruction_alloc);
            // local_instructions.append(ReducedInstruction.Zero) catch |err| std.debug.panic("Append failed: {}", .{err});

            const instruction_pos = 0;
            const read_from_channel = true;
            const pointer_pos = 0;
            var buffer = comptime switch (buffer_size) {
                .dynamic => |a| ArrayList(u8).init(a),
                .sized => |s| blk: {
                    const buffer = [_]u8{0} ** s;
                    break :blk ArrayList(u8).init(std.heap.FixedBufferAllocator.init(buffer));
                },
            };

            buffer.append(0) catch |err| {
                std.debug.panic("Append failed: {any}", .{err});
            };

            const jump_points = HashMap(usize, usize).init(jump_alloc);
            const unmatched_open = ArrayList(usize).init(jump_alloc);

            return .{
                .channel = channel,
                .local_instructions = local_instructions,
                .instruction_pos = instruction_pos,
                .read_from_channel = read_from_channel,
                .pointer_pos = pointer_pos,
                .buffer = buffer,
                .jump_points = jump_points,
                .unmatched_open = unmatched_open,
                .out = out_writer,
                .in = in_reader,
                .furthest_matched_instruction = 0,
            };
        }

        pub fn deinit(self: *Self) void {
            self.local_instructions.deinit();
            self.buffer.deinit();
            self.jump_points.deinit();
            self.unmatched_open.deinit();
        }

        pub fn readNext(self: *Self) !ReducedInstruction {
            if (self.read_from_channel) {
                return try self.readNextFromChannel();
            } else {
                return self.local_instructions.items[self.instruction_pos];
            }
        }

        fn searchClosingBracket(self: *Self) !void {
            var indent_counter: usize = 1;
            self.read_from_channel = self.instruction_pos >= self.local_instructions.items.len;

            while (indent_counter != 0) {
                const next_instruction = try self.readNext();
                if (next_instruction.isEOP()) {
                    return InstructionExecutionError.UnmatchedOpenBracket;
                }
                switch (next_instruction) {
                    ReducedInstruction.JumpPoint => {
                        indent_counter += 1;
                        try self.unmatched_open.append(self.instruction_pos);
                    },
                    ReducedInstruction.JumpBackTo => {
                        indent_counter -= 1;
                        _ = try self.matchOpenBracket(self.instruction_pos);
                    },
                    else => {},
                }
                self.instruction_pos += 1;
            }
        }

        fn matchOpenBracket(self: *Self, close: usize) !usize {
            const matching_open = self.unmatched_open.popOrNull() orelse return InstructionExecutionError.UnexpectedJumpBack;
            try self.jump_points.put(matching_open, close);
            try self.jump_points.put(close, matching_open);
            self.furthest_matched_instruction = close;
            return matching_open;
        }

        fn processInstruction(self: *Self, instruction: ReducedInstruction) !void {
            switch (instruction) {
                .Incr => |x| {
                    self.selectedValue().* +%= x;
                    std.debug.print("\nSelected Value now is {d}", .{self.selectedValue().*});
                },
                .Decr => |x| {
                    self.selectedValue().* -%= x;
                    std.debug.print("\nSelected Value now is {d}", .{self.selectedValue().*});
                },
                .Zero => self.selectedValue().* = 0,
                .JumpPoint => {
                    if (self.furthest_matched_instruction < self.instruction_pos) {
                        try self.unmatched_open.append(self.instruction_pos);
                        if (self.selectedValue().* == 0) {
                            std.debug.print("searching forward", .{});
                            try self.searchClosingBracket();
                        }
                    } else {
                        if (self.selectedValue().* == 0) {
                            self.instruction_pos = self.jump_points.get(self.instruction_pos) orelse return InstructionExecutionError.FurthestMatchedInstructionCorrupted;
                        }
                    }
                },
                .JumpBackTo => {
                    if (self.furthest_matched_instruction < self.instruction_pos) {
                        std.debug.print("Found ] during normal process", .{});
                        const open = try self.matchOpenBracket(self.instruction_pos);
                        if (self.selectedValue().* != 0) {
                            self.instruction_pos = open;
                        }
                    } else {
                        if (self.selectedValue().* != 0) {
                            const open = self.jump_points.get(self.instruction_pos) orelse return InstructionExecutionError.FurthestMatchedInstructionCorrupted;
                            self.instruction_pos = open;
                        }
                    }
                },
                .MovLeft => |x| {
                    if (self.pointer_pos >= x) {
                        self.pointer_pos -= x;
                    }
                },
                .MovRight => |x| {
                    self.pointer_pos += x;
                    if (self.pointer_pos >= self.buffer.items.len) {
                        const extend = self.pointer_pos - self.buffer.items.len + 1;
                        try self.buffer.appendNTimes(0, extend);
                    }
                },
                .EOP => {},
                .ReadIo => {
                    const value = try self.in.readByte();
                    self.selectedValue().* = value;
                },
                .WriteIo => {
                    const selected = self.selectedValue().*;
                    try self.out.writeByte(selected);
                },
            }
        }

        fn selectedValue(self: *Self) *u8 {
            return &self.buffer.items[self.pointer_pos];
        }

        fn processAllInstructions(self: *Self) !void {
            while (true) {
                const next_instruction = try self.readNext();
                std.debug.print("\nNext [{d}]: {any}", .{ self.instruction_pos, next_instruction });
                if (next_instruction.isEOP()) {
                    return;
                }
                try self.processInstruction(next_instruction);
                self.instruction_pos += 1;
                self.read_from_channel = self.instruction_pos >= self.local_instructions.items.len;
            }
        }

        fn readNextFromChannel(self: *Self) !ReducedInstruction {
            const next = try self.channel.recv();
            try self.local_instructions.append(next);
            return next;
        }
    };
}

pub fn PeekableFileReader(comptime buf_size: usize, comptime max_peek_len: usize) type {
    return struct {
        reader: std.io.BufferedReader(buf_size, std.fs.File.Reader),
        peek_buf: [max_peek_len]?u8,
        peek_len: usize,
        const Self = @This();

        pub fn init(file: std.fs.File) Self {
            return Self{
                .reader = std.io.bufferedReader(file.reader()),
                .peek_buf = [_]?u8{null} ** max_peek_len,
                .peek_len = 0,
            };
        }

        // Return the next peek and write it to the peek buffer if it doesn't exceeed the length
        pub fn peek(self: *Self) !?u8 {
            if (self.peek_len < max_peek_len) {
                const next = try self.readIgnoreWhitespace() orelse return null;
                self.peek_buf[self.peek_len] = next;
                self.peek_len += 1;
                return next;
            } else {
                @panic("The peek exceeds the length of the peek buffer. You may have forgotten to clear the peeks");
            }
        }

        // Confirms peeks, deleting them from the buffer. This essentially accepts them as normal reads
        pub fn confirmPeeks(self: *Self) void {
            self.peek_len = 0;
            self.peek_buf = [_]?u8{null} ** max_peek_len;
        }

        // Read the next non-whitespace character, taking from the peek buffer first if there is anything there.
        pub fn read(self: *Self) !?u8 {
            if (self.peek_len != 0) {
                const read_val = self.peek_buf[0];
                self.peek_len -= 1;
                for (0..1) |i| {
                    self.peek_buf[i] = self.peek_buf[i + 1];
                }
                return read_val;
            } else {
                return self.readIgnoreWhitespace();
            }
        }

        fn readIgnoreWhitespace(self: *Self) !?u8 {
            while (true) {
                const c = self.reader.reader().readByte() catch |err| switch (err) {
                    error.EndOfStream => return null,
                    else => |e| return e,
                };
                if (!std.ascii.isWhitespace(c)) {
                    return c;
                }
            }
            return null;
        }
    };
}

pub fn Interpreter(comptime config: InterpreterConfig) type {
    return struct {
        const InterpreterWriter = InstructionWriter(config.reader_buf_size);
        const InterpreterReader = InstructionReader(config.max_program_buffer_size, config.IoWriter, config.IoReader);
        const Self = @This();

        writer: InterpreterWriter,
        reader: InterpreterReader,
        channel: InstructionChannel,

        pub fn init(file: File, io_writer: config.IoWriter, io_reader: config.IoReader) Self {
            var channel = InstructionChannel.init(config.channel_alloc);

            var writer = Self.InterpreterWriter.init(file, &channel);
            var reader = Self.InterpreterReader.init(&channel, config.instr_alloc, config.jump_alloc, io_writer, io_reader);

            _ = &writer;
            _ = &reader;

            return .{
                .writer = writer,
                .reader = reader,
                .channel = channel,
            };
        }

        pub fn deinit(self: *Self) void {
            self.channel.deinit();
            self.reader.deinit();
            // Writer has no deinit
        }

        pub fn run(self: *Self) !void {
            var writer_thread = try std.Thread.spawn(.{}, InterpreterWriter.sendAllInstructions, .{&self.writer});
            try self.reader.processAllInstructions();
            writer_thread.join();
        }
    };
}

const File = std.fs.File;
const t_expect = std.testing.expect;

// test "test_hello-world_InstructionGen" {
//     const file = try std.fs.cwd().openFile("bf/test-hello_world.bf", .{});
//     defer file.close();
//
//     var channel = InstructionChannel.init(std.testing.allocator);
//     defer channel.deinit();
//
//     const WriterType = InstructionWriter(4096);
//
//     var instr_writer = WriterType.init(file, &channel);
//
//     var thread = try std.Thread.spawn(.{}, WriterType.sendAllPanic, .{&instr_writer});
//     defer thread.join();
//
//     while (channel.recv()) |instr| {
//         if (instr.isEOP()) {
//             break;
//         }
//         // std.debug.print("\nREDUCED INSTRUCTION: {any}", .{instr});
//     } else |err| {
//         return err;
//     }
// }

test "test-hello_world" {
    var file = try std.fs.cwd().openFile("bf/test-hello_world.bf", .{});
    defer file.close();

    var output = ArrayList(u8).init(std.testing.allocator);
    defer output.deinit();

    var writer = output.writer();
    var std_in = std.io.getStdIn().reader();

    _ = &writer;
    _ = &std_in;

    var interpreter = Interpreter(.{}).init(file, writer, std_in);
    defer interpreter.deinit();
    try interpreter.run();

    const expected: []const u8 = "Hello World!\n";

    std.debug.print("\nDebug Charcodes for {s} ", .{output.items});
    for (output.items) |char| {
        std.debug.print("{x} ", .{char});
    }
    std.debug.print("\n\n", .{});

    const expected_len = expected.len;
    const output_len = output.items.len;
    if (expected_len != output_len) {
        std.debug.panic("Output len does not match, expected: {d}, found: {d}", .{ expected_len, output_len });
    }

    // std.debug.print("\nExpectedLen {}", .{output.items.len});

    try t_expect(std.mem.eql(u8, output.items, expected));
}

// test "test-list_all" {
//     var file = try std.fs.cwd().openFile("bf/test-list_all.bf", .{});
//     defer file.close();
//
//     var output = ArrayList(u8).init(std.testing.allocator);
//     defer output.deinit();
//
//     var writer = output.writer();
//     var std_in = std.io.getStdIn().reader();
//
//     _ = &writer;
//     _ = &std_in;
//
//     var interpreter = Interpreter(.{}).init(file, writer, std_in);
//     defer interpreter.deinit();
//     try interpreter.run();
//
//     const expected: []const u8 = "Hello World!\n";
//
//     std.debug.print("\nDebug Charcodes for {s} ", .{output.items});
//     for (output.items) |char| {
//         std.debug.print("{x} ", .{char});
//     }
//     std.debug.print("\n\n", .{});
//
//     const expected_len = expected.len;
//     const output_len = output.items.len;
//     if (expected_len != output_len) {
//         std.debug.panic("Output len does not match, expected: {d}, found: {d}", .{ expected_len, output_len });
//     }
//
//     std.debug.print("\nExpectedLen {}", .{output.items.len});
//
//     try t_expect(std.mem.eql(u8, output.items, expected));
//
//     std.debug.print("\n\n{s}\n\n", .{output.items});
// }

test "test_1-InstructionGen" {
    const file = try std.fs.cwd().openFile("bf/test-1.bf", .{});
    defer file.close();

    var channel = InstructionChannel.init(std.testing.allocator);
    defer channel.deinit();
    var instr_writer = InstructionWriter(4096).init(file, &channel);

    try t_expect(ReducedInstruction.deepEqual(ReducedInstruction.JumpPoint, try instr_writer.genNextInstruction()));
    try t_expect(ReducedInstruction.deepEqual(ReducedInstruction{ .MovRight = 5 }, try instr_writer.genNextInstruction()));
    try t_expect(ReducedInstruction.deepEqual(ReducedInstruction.Zero, try instr_writer.genNextInstruction()));
    try t_expect(ReducedInstruction.deepEqual(ReducedInstruction.Zero, try instr_writer.genNextInstruction()));
    try t_expect(ReducedInstruction.deepEqual(ReducedInstruction{ .Incr = 3 }, try instr_writer.genNextInstruction()));
    try t_expect(ReducedInstruction.deepEqual(ReducedInstruction.JumpBackTo, try instr_writer.genNextInstruction()));
}
