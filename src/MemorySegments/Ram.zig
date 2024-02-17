const std = @import("std");

const Memory = @import("../Memory.zig");
const MemorySegment = Memory.MemorySegment;
const PhysicalAddress = Memory.PhysicalAddress;
const MemAccessError = Memory.MemAccessError;

const defs = @import("../defs.zig");
const Byte = defs.Byte;

const Allocator = std.mem.Allocator;

pub const RamId = "RAM";

const Ram = struct {
    data: []Byte,

    pub fn init(self: *Ram, size: u64, allocator: Allocator) !void {
        var data = try allocator.alloc(Byte, size);
        self.data = data;
    }
};

pub fn RamCallback(self: *MemorySegment, addr: PhysicalAddress, data: ?Byte) MemAccessError!?Byte {
    var ram: *Ram = @ptrCast(@alignCast(self.userData));
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

pub fn NewRamSegment(base: u64, limit: u64, allocator: Allocator, id: []const u8) !*MemorySegment {
    var ram: *Ram = try allocator.create(Ram);

    try ram.init(limit + 1, allocator);

    var fullId = try std.fmt.allocPrintZ(allocator, "{s}-{s}", .{ RamId, id });

    var seg: *MemorySegment = try allocator.create(MemorySegment);
    seg.* = MemorySegment{
        .base = base,
        .limit = limit,
        .userData = ram,
        .id = fullId,
        .callback = RamCallback,
    };
    return seg;
}
