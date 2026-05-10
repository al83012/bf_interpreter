const std = @import("std");
const interpret = @import("interpreter.zig");

pub fn main() !void {
    
}

test "test-1.bs" {
    const testing_alloc = std.testing.allocator;
    var interpreter = try interpret.Interpreter(
        .{ .max_callstack_size = .{
            .dynamic = testing_alloc,
        }, .max_program_buffer_size = .{
            .dynamic = testing_alloc,
        }, .max_program_size = .{
            .dynamic = testing_alloc,
        }, .reader_buf_size = 4096 },
    ).init();
    defer interpreter.deinit();
    try interpreter.run("bs/test-1.bs");
}
