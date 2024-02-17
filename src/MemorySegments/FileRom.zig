const std = @import("std");

const Memory = @import("../Memory.zig");
const MemorySegment = Memory.MemorySegment;
const PhysicalAddress = Memory.PhysicalAddress;
const MemAccessError = Memory.MemAccessError;

const defs = @import("../defs.zig");
const Byte = defs.Byte;
const File = std.fs.File;

const Allocator = std.mem.Allocator;

pub const FileRomId = "ROM";

const FileRom = struct {
    data: []Byte,

    pub fn New(allocator: Allocator, file: File, size: u64) !*FileRom {
        var f: *FileRom = try allocator.create(FileRom);
        var statData = try file.stat();
        var fsize = statData.size;
        var fBufSize = @min(size, fsize);
        var fbuffer = try allocator.alloc(Byte, fBufSize);
        _ = try file.read(fbuffer);

        var rombuffer = try allocator.alloc(Byte, size);

        for (fbuffer, 0..) |fbyte, i| {
            rombuffer[i] = fbyte;
        }

        allocator.free(fbuffer);

        f.data = rombuffer;

        return f;
    }
};

pub fn FileRomCallback(self: *MemorySegment, addr: PhysicalAddress, data: ?Byte) MemAccessError!?Byte {
    var rom: *FileRom = @ptrCast(@alignCast(self.userData));
    if (addr >= rom.data.len) {
        return MemAccessError.AddressTooLarge;
    }
    if (data) |byte| {
        _ = byte;
        // ROM writes are a nop
        // TODO - possible log a message here?
        return null;
    } else {
        return rom.data[addr];
    }
}

pub fn NewFileRomSegment(base: u64, filePath: [:0]const u8, size: u64, allocator: Allocator) !*MemorySegment {
    var segment: *MemorySegment = try allocator.create(MemorySegment);
    segment.base = base;

    var file = try std.fs.cwd().openFile(filePath, .{});
    var from = try FileRom.New(allocator, file, size);
    segment.userData = from;
    segment.limit = from.data.len;

    var fullId = try std.fmt.allocPrintZ(allocator, "{s}-{s}", .{ FileRomId, filePath });
    segment.id = fullId;

    segment.callback = FileRomCallback;

    return segment;
}
