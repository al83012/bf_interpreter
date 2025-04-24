const std = @import("std");
const interpret = @import("interpreter.zig");

pub fn main() !void {
    // Prints to stderr (it's a shortcut based on `std.io.getStdErr()`)
    std.debug.print("All your {s} are belong to us.\n", .{"codebase"});

    // stdout is for the actual output of your application, for example if you
    // are implementing gzip, then only the compressed bytes should be sent to
    // stdout, not any debugging messages.
    const stdout_file = std.io.getStdOut().writer();
    var bw = std.io.bufferedWriter(stdout_file);
    const stdout = bw.writer();

    try stdout.print("Run `zig build test` to run the tests.\n", .{});

    try bw.flush(); // don't forget to flush!
}

// test "test-1.bs" {
//     const testing_alloc = std.testing.allocator;
//     var interpreter = try interpret.Interpreter(
//         .{ .max_callstack_size = .{
//             .dynamic = testing_alloc,
//         }, .max_program_buffer_size = .{
//             .dynamic = testing_alloc,
//         }, .max_program_size = .{
//             .dynamic = testing_alloc,
//         }, .reader_buf_size = 4096 },
//     ).init();
//     defer interpreter.deinit();
//     try interpreter.run("bs/test-1.bs");
// }
