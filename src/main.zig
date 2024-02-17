const std = @import("std");
const Cpu = @import("Cpu.zig");
const Ram = @import("MemorySegments/Ram.zig");
const FileRom = @import("MemorySegments/FileRom.zig");
const FileRam = @import("MemorySegments/FileRam.zig");
const VirtualAddress = @import("Memory.zig").VirtualAddress;
const Byte = @import("defs.zig").Byte;

const startHertz: u64 = 400;

const begin = 0x80000000;

var gpa = std.heap.GeneralPurposeAllocator(.{}){};
var allocator = gpa.allocator();

fn printSignature(dBuffer: []u8, writer: anytype) !void {
    for (0..(dBuffer.len / 4)) |i| {
        var sig = std.mem.readIntLittle(u32, @as(*[4]u8, @ptrCast(dBuffer[i * 4 ..].ptr)));
        try writer.print("{x:0>8}\n", .{sig});
    }
}

pub fn main() !void {
    var args = try std.process.argsAlloc(allocator);

    var cpu: Cpu = try Cpu.New(startHertz, allocator, begin);

    var fileseg = try FileRam.NewFileRamSegment(begin, args[1], 0x100000000, allocator);
    var outpath = args[2];
    var out = try std.fs.cwd().createFile(outpath, .{});
    defer out.close();

    var retaddr = try std.fmt.parseInt(VirtualAddress, args[3], 0);
    try cpu.AddMemorySegment(fileseg);

    var testDataBegin = try std.fmt.parseInt(VirtualAddress, args[4], 0);
    var testDataEnd = try std.fmt.parseInt(VirtualAddress, args[5], 0);

    var testDataBuffer = try allocator.alloc(Byte, testDataEnd - testDataBegin);

    var ret = try cpu.Run(retaddr, testDataBegin, testDataBuffer);
    switch (ret.t) {
        .OK => {
            try printSignature(testDataBuffer, out.writer());
            return;
        },
        else => {
            try std.io.getStdErr().writer().print("ERROR: {s}\n", .{ret});
            std.os.exit(1);
        },
    }
}
