pub const std = @import("std");
pub const BoundedArray = std.BoundedArray;
pub const ArrayList = std.ArrayList;
pub const Allocator = std.mem.Allocator;
pub const Channel = @import("channel.zig").Channel;

pub const SizeConstraint = union(enum) {
    sized: usize,
    dynamic: Allocator,
};

pub const InterpreterConfig = struct {
    max_program_buffer_size: SizeConstraint,
    reader_buf_size: usize,
};

pub const InstructionExecutionError = error{
    UnexpectedJumpBack,
    MovedPointerOutOfRange,
};

pub const ReducedInstruction = union(enum) {
    MovRight: usize,
    MovLeft: usize,
    JumpPoint,
    JumpBackTo: usize,
    Incr: u8,
    Decr: u8,
    Zero,
    EOP,
    ReadIo,
    WriteIo,

    const Self = @This();

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

pub const InstructionChannel = Channel(ReducedInstruction);

pub fn InstructionWriter(comptime file_reader_buf_size: usize) type {
    return struct {
        channel: *InstructionChannel,
        char_reader: CharReader(file_reader_buf_size),

        const Self = @This();

        pub fn init(file: std.fs.File, channel: *InstructionChannel) Self {
            return .{
                .channel = channel,
                .char_reader = CharReader(file_reader_buf_size).init(file),
            };
        }
        pub fn deinit(self: Self) void {
            self.char_reader.deinit();
        }

        fn genNextInstruction(self: *Self) !ReducedInstruction {
            const next_char = try self.char_reader.readChar() orelse return .EOF;
            switch (next_char) {
                '<' => {
                    var count: usize = 1;
                    while (try self.char_reader.peekChar()) |peek| {
                        if (peek == '<') {
                            count += 1;
                            self.char_reader.markPeekRead();
                        } else {
                            break;
                        }
                    }
                    return ReducedInstruction{ .MovLeft = count };
                },

                '>' => {
                    var count: usize = 1;
                    while (try self.char_reader.peekChar()) |peek| {
                        if (peek == '>') {
                            count += 1;
                            self.char_reader.markPeekRead();
                        } else {
                            break;
                        }
                    }
                    return ReducedInstruction{ .MovRight = count };
                },

                '+' => {
                    var count: u8 = 1;
                    while (try self.char_reader.peekChar()) |peek| {
                        if (peek == '+') {
                            // Keeping it as a u8 is fine since it will wrap in the program code anyways
                            count +%= 1;
                            self.char_reader.markPeekRead();
                        } else {
                            break;
                        }
                    }
                    return ReducedInstruction{ .Incr = count };
                },

                '-' => {
                    var count: u8 = 1;
                    while (try self.char_reader.peekChar()) |peek| {
                        if (peek == '-') {
                            count +%= 1;
                            self.char_reader.markPeekRead();
                        } else {
                            break;
                        }
                    }
                    return ReducedInstruction{ .Decr = count };
                },
                else => @panic("Not yet supported"),
            }
        }

        fn sendAllInstructions(self: *Self) !void {
            while (true) {
                const next_instruction = try self.genNextInstruction();
                try self.channel.send(next_instruction);
                if (next_instruction == .EOP) {
                    break;
                }
            }
        }
    };
}

pub fn InstructionReader(buffer_size: SizeConstraint) type {
    return struct {
        channel: *InstructionChannel,
        local_instructions: ArrayList(ReducedInstruction),
        instruction_pos: usize,
        read_from_channel: bool,
        pointer_pos: usize,
        loop_stack: ArrayList(usize),
        buffer: ArrayList(u8),

        const Self = @This();

        pub fn init(channel: *InstructionChannel, alloc: Allocator) Self {
            const local_instructions = ArrayList(ReducedInstruction).init(alloc);
            const instruction_pos = 0;
            const read_from_channel = true;
            const pointer_pos = 0;
            const loop_stack = ArrayList(usize).init(alloc);
            const buffer = comptime switch (buffer_size) {
                .dynamic => |a| ArrayList(usize).init(a),
                .sized => |s| blk: {
                    const buffer = [_]u8{0} ** s;
                    break :blk ArrayList(usize).init(std.heap.FixedBufferAllocator.init(buffer));
                },
            };
            return .{
                .channel = channel,
                .local_instructions = local_instructions,
                .instruction_pos = instruction_pos,
                .read_from_channel = read_from_channel,
                .pointer_pos = pointer_pos,
                .loop_stack = loop_stack,
                .buffer = buffer,
            };
        }

        pub fn deinit(self: Self) void {
            self.local_instructions.deinit();
            self.loop_stack.deinit();
            self.buffer.deinit();
        }

        fn processInstruction(self: *Self, instruction: ReducedInstruction) !void {
            switch (instruction) {
                .Incr => |x| self.selectedValue().* -%= x,
                .Decr => |x| self.selectedValue().* +%= x,
                .Zero => self.selectedValue().* = 0,
                .JumpPoint => try self.loop_stack.append(self.instruction_pos),
                .JumpBackTo => {
                    const jump_point = self.loop_stack.popOrNull() orelse return InstructionExecutionError.UnexpectedJumpBack;
                    self.instruction_pos = jump_point;
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
                        self.buffer.appendNTimes(0, extend);
                    }
                },
                .EOP => {},

                else => @panic("Not yet implemented"),
            }
        }

        fn selectedValue(self: *Self) *u8 {
            return *self.buffer.items[self.pointer_pos];
        }

        fn processAllInstructions(self: *Self) !void {
            while (true) {
                const next_instruction = blk: {
                    if (self.read_from_channel) {
                        break :blk try self.readNextFromChannel();
                    } else {
                        break :blk try self.local_instructions.items[self.instruction_pos];
                    }
                };
                if (next_instruction == .EOP) {
                    return;
                }
                self.processInstruction(next_instruction);
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
