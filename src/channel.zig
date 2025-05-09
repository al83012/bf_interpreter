const std = @import("std");

const ChannelError = error{
    Closed,
    OutOfMemory,
    NotImplemented,
    DataCorruption,
};

// pub fn Channel(comptime T: type) type {
//     return BufferedChannel(T, 0);
// }

pub fn BufferedChannel(comptime T: type, comptime bufSize: usize) type {
    return struct {
        const Self = @This();
        const bufType = [bufSize]?T;
        buf: bufType = [_]?T{null} ** bufSize,
        closed: bool = false,
        mut: std.Thread.Mutex = std.Thread.Mutex{},
        alloc: std.mem.Allocator = undefined,
        recvQ: std.ArrayList(*Receiver) = undefined,
        sendQ: std.ArrayList(*Sender) = undefined,

        // represents a thread waiting on recv
        const Receiver = struct {
            mut: std.Thread.Mutex = std.Thread.Mutex{},
            cond: std.Thread.Condition = std.Thread.Condition{},
            data: ?T = null,

            fn putDataAndSignal(self: *@This(), data: T) void {
                defer self.cond.signal();
                self.data = data;
            }
        };

        // represents a thread waiting on send
        const Sender = struct {
            mut: std.Thread.Mutex = std.Thread.Mutex{},
            cond: std.Thread.Condition = std.Thread.Condition{},
            data: T,

            fn getDataAndSignal(self: *@This()) T {
                defer self.cond.signal();
                return self.data;
            }
        };

        pub fn init(alloc: std.mem.Allocator) Self {
            return Self{
                .alloc = alloc,
                .recvQ = std.ArrayList(*Receiver).init(alloc),
                .sendQ = std.ArrayList(*Sender).init(alloc),
            };
        }

        pub fn deinit(self: *Self) void {
            self.recvQ.deinit();
            self.sendQ.deinit();
        }

        fn close(self: *Self) void {
            self.closed = true;
        }

        fn capacity(self: *Self) usize {
            return self.buf.len;
        }

        fn debugBuf(self: *Self) void {
            std.debug.print("{d} Buffer debug\n", .{std.time.milliTimestamp()});
            for (self.buf, 0..) |item, i| {
                if (item) |unwrapped| {
                    std.debug.print("[{d}] = {d}\n", .{ i, unwrapped });
                }
            }
        }

        fn len(self: *Self) usize {
            var i: usize = 0;
            for (self.buf) |item| {
                if (item) |_| {
                    i += 1;
                } else {
                    break;
                }
            }
            return i;
        }

        pub fn send(self: *Self, data: T) ChannelError!void {
            if (self.closed) return ChannelError.Closed;

            self.mut.lock();
            errdefer self.mut.unlock();

            // case: receiver already waiting
            // pull receiver (if any) and give it data. Signal receiver that it's done waiting.
            if (self.recvQ.items.len > 0) {
                defer self.mut.unlock();
                var receiver: *Receiver = self.recvQ.orderedRemove(0);
                receiver.putDataAndSignal(data);
                return;
            }

            // case: room in buffer
            const l = self.len();
            if (l < self.capacity() and bufSize > 0) {
                defer self.mut.unlock();

                // insert into first null spot in buffer
                self.buf[l] = data;
                return;
            }

            // hold on sender queue. Receivers will signal when they take data.
            var sender = Sender{ .data = data };

            // prime condition
            sender.mut.lock(); // cond.wait below will unlock it and wait until signal, then relock it
            defer sender.mut.unlock(); // unlocks the relock

            try self.sendQ.append(&sender); // make visible to other threads
            self.mut.unlock(); // allow all other threads to proceed. This thread is done reading/writing

            // now just wait for receiver to signal sender
            sender.cond.wait(&sender.mut);
            return;
        }

        pub fn recv(self: *Self) ChannelError!T {
            if (self.closed) return ChannelError.Closed;
            self.mut.lock();
            errdefer self.mut.unlock();

            // case: value in buffer
            const l = self.len();
            if (l > 0 and bufSize > 0) {
                defer self.mut.unlock();
                const val = self.buf[0] orelse return ChannelError.DataCorruption;

                // advance items in buffer
                if (l > 1) {
                    for (self.buf[1..l], 0..l - 1) |item, i| {
                        self.buf[i] = item;
                    }
                }
                self.buf[l - 1] = null;

                // top up buffer with a waiting sender, if any
                if (self.sendQ.items.len > 0) {
                    var sender: *Sender = self.sendQ.orderedRemove(0);
                    const valFromSender: T = sender.getDataAndSignal();
                    self.buf[l - 1] = valFromSender;
                }

                return val;
            }

            // case: sender already waiting
            // pull sender and take its data. Signal sender that it's done waiting.
            if (self.sendQ.items.len > 0) {
                defer self.mut.unlock();
                var sender: *Sender = self.sendQ.orderedRemove(0);
                const data: T = sender.getDataAndSignal();
                return data;
            }

            // hold on receiver queue. Senders will signal when they take it.
            var receiver = Receiver{};

            // prime condition
            receiver.mut.lock();
            defer receiver.mut.unlock();

            try self.recvQ.append(&receiver);
            self.mut.unlock();

            // now wait for sender to signal receiver
            receiver.cond.wait(&receiver.mut);
            // sender should have put data in .data
            if (receiver.data) |data| {
                return data;
            } else {
                return ChannelError.DataCorruption;
            }
        }
    };
}
