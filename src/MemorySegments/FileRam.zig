const std = @import("std");

const Memory = @import("../Memory.zig");
const MemorySegment = Memory.MemorySegment;
const PhysicalAddress = Memory.PhysicalAddress;
const MemAccessError = Memory.MemAccessError;

const defs = @import("../defs.zig");
const Byte = defs.Byte;
const File = std.fs.File;

const Allocator = std.mem.Allocator;

pub const FileRamId = "RAM";

const FileRam = struct {
    data: []Byte,

    pub fn New(allocator: Allocator, file: File, size: u64) !*FileRam {
        var f: *FileRam = try allocator.create(FileRam);
        var statData = try file.stat();
        var fsize = statData.size;
        var fBufSize = @min(size, fsize);
        var fbuffer = try allocator.alloc(Byte, fBufSize);
        _ = try file.readAll(fbuffer);

        var rambuffer = try allocator.alloc(Byte, size);

        for (fbuffer, 0..) |fbyte, i| {
            rambuffer[i] = fbyte;
        }

        allocator.free(fbuffer);

        f.data = rambuffer;

        return f;
    }
};

pub fn FileRamCallback(self: *MemorySegment, addr: PhysicalAddress, data: ?Byte) MemAccessError!?Byte {
    var ram: *FileRam = @ptrCast(@alignCast(self.userData));
    if (addr >= ram.data.len) {
        return MemAccessError.AddressTooLarge;
    }
    if (data) |byte| {
        ram.data[addr] = byte;
        return null;
    } else {
        return ram.data[addr];
    }
}

pub fn NewFileRamSegment(base: u64, filePath: [:0]const u8, size: u64, allocator: Allocator) !*MemorySegment {
    var segment: *MemorySegment = try allocator.create(MemorySegment);
    segment.base = base;

    var file = try std.fs.cwd().openFile(filePath, .{});
    defer file.close();
    var fram = try FileRam.New(allocator, file, size);
    segment.userData = fram;
    segment.limit = fram.data.len;

    var fullId = try std.fmt.allocPrintZ(allocator, "{s}-{s}", .{ FileRamId, filePath });
    segment.id = fullId;

    segment.callback = FileRamCallback;

    return segment;
}
