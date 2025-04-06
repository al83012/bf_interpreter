pub const std = @import("std");
pub const BoundedArray = std.BoundedArray;
pub const ArrayList = std.ArrayList;

pub const SizeConstraint = union(enum) {
    sized: usize,
    dynamic: type,
};

pub const InterpreterConfig = struct {
    max_callstack_size: SizeConstraint,
    max_program_buffer_size: SizeConstraint,
    max_program_size: SizeConstraint,
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
    fn nextInstruction(reader: anytype) ?Self {
        const next_c: u8 = reader.readByte() orelse return null;


        // TODO: In the case that the instructions can be compressed, we would have to peek at the next char to see if we should do that. I don't know right now how to do that as the documentation is sadly quite poor

        switch (next_c) {
            '<' => {
            }
        }

    }
};

fn Interpreter(comptime config: InterpreterConfig) type {
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

        const Self = @This();

        pub fn init(file_name: []const u8) Self {
            return .{
                .call_stack = switch (config.max_callstack_size) {
                    .sized => |_| try call_stack_ty.init(0),
                    .dynamic => |alloc| try call_stack_ty.init(alloc),
                },
                .buffer = switch (config.max_program_buffer_size) {
                    .sized => |_| try buf_ty.init(0),
                    .dynamic => |alloc| try buf_ty.init(alloc),
                },
                .reduced_instructions = switch (config.max_program_size) {
                    .sized => |_| try instr_ty.init(0),
                    .dynamic => |alloc| try instr_ty.init(alloc),
                },
            };
        }

        fn run(self: *Self, file_name: []const u8) void {
            var file = try std.fs.cwd().openFile(file_name, .{});
            defer file.close();

            var buf_reader = std.io.bufferedReader(file.reader());
            var in_stream = buf_reader.reader();

            //TODO: How exactly do these readers work? 
            

            while (true) {}
        }

        fn nextInstruction(self: *Self, reader: anytype) ?ReducedInstruction {
            const next_instr: ?ReducedInstruction = blk: {
                if (self.instruction_pos < self.reduced_instructions.len) {
                    break :blk self.reduced_instructions.get(self.instruction_pos);
                } else {
                    const next = ReducedInstruction.nextInstruction(reader);
                    if (next) |n| {
                        self.reduced_instructions.append(n);
                    }
                    break :blk next;
                }
            };
            self.instruction_pos += 1;
            return next_instr;
            
        }
    };
}
