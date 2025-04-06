pub const std = @import("std");
pub const BoundedArray = std.BoundedArray;
pub const ArrayList = std.ArrayList;
pub const Allocator = std.mem.Allocator;

pub const SizeConstraint = union(enum) {
    sized: usize,
    dynamic: Allocator,
};

pub const InterpreterConfig = struct {
    max_callstack_size: SizeConstraint,
    max_program_buffer_size: SizeConstraint,
    max_program_size: SizeConstraint,
    reader_buf_size: usize,
};

pub const ReducedInstruction = union(enum) {
    MovRight: usize,
    MovLeft: usize,
    JumpPoint,
    JumpBackTo: usize,
    Incr: usize,
    Decr: usize,
    Zero,
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

        pub fn init(file: std.fs.File) Self {
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

pub fn Interpreter(comptime config: InterpreterConfig) type {
    const call_stack_ty = switch (config.max_callstack_size) {
        .sized => |size| BoundedArray(usize, size),
        .dynamic => |_| ArrayList(usize),
    };

    const buf_ty = switch (config.max_program_buffer_size) {
        .sized => |size| BoundedArray(u8, size),
        .dynamic => |_| ArrayList(u8),
    };

    const instr_ty = switch (config.max_program_size) {
        .sized => |size| BoundedArray(ReducedInstruction, size),
        .dynamic => |_| ArrayList(ReducedInstruction),
    };

    return struct {
        call_stack: call_stack_ty,
        buffer: buf_ty,
        reduced_instructions: instr_ty,
        instruction_pos: usize = 0,
        char_reader: SelfCharReader,

        const Self = @This();
        const SelfCharReader = CharReader(config.reader_buf_size);

        pub fn init() !Self {
            return .{
                .call_stack = switch (config.max_callstack_size) {
                    .sized => |_| try call_stack_ty.init(0),
                    .dynamic => |alloc| call_stack_ty.init(alloc),
                },
                .buffer = switch (config.max_program_buffer_size) {
                    .sized => |_| try buf_ty.init(0),
                    .dynamic => |alloc| buf_ty.init(alloc),
                },
                .reduced_instructions = switch (config.max_program_size) {
                    .sized => |_| try instr_ty.init(0),
                    .dynamic => |alloc| instr_ty.init(alloc),
                },
                .char_reader = undefined,
                .instruction_pos = 0,
            };
        }
        pub fn deinit(self: Self) void {
            self.call_stack.deinit();
            self.buffer.deinit();
            self.reduced_instructions.deinit();
        }

        pub fn run(self: *Self, file_name: []const u8) !void {
            var file = try std.fs.cwd().openFile(file_name, .{});
            defer file.close();

            self.char_reader = SelfCharReader.init(file);

            std.log.warn("\n", .{});
            while (try self.nextInstruction()) |instr| {
                std.log.warn("Next: {}", .{instr});
            }
        }

        fn nextInstruction(self: *Self) !?ReducedInstruction {
            const next_instr: ?ReducedInstruction = blk: {
                const len = switch (config.max_program_size) {
                    .sized => |_| self.reduced_instructions.len,
                    .dynamic => |_| self.reduced_instructions.items.len,
                };
                if (self.instruction_pos < len) {
                    break :blk switch (config.max_program_size) {
                        .sized => |_| self.reduced_instructions.get(self.instruction_pos),
                        .dynamic => |_| self.reduced_instructions.items[self.instruction_pos],
                    };
                } else {
                    const next = try self.genNextInstruction();
                    if (next) |n| {
                        try self.reduced_instructions.append(n);
                    }
                    break :blk next;
                }
            };
            self.instruction_pos += 1;
            return next_instr;
        }

        fn genNextInstruction(self: *Self) !?ReducedInstruction {
            const next_char = try self.char_reader.readChar() orelse return null;
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
                    var count: usize = 1;
                    while (try self.char_reader.peekChar()) |peek| {
                        if (peek == '+') {
                            count += 1;
                            self.char_reader.markPeekRead();
                        } else {
                            break;
                        }
                    }
                    return ReducedInstruction{ .Incr = count };
                },

                '-' => {
                    var count: usize = 1;
                    while (try self.char_reader.peekChar()) |peek| {
                        if (peek == '-') {
                            count += 1;
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
    };
}
