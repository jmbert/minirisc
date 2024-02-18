const std = @import("std");
const defs = @import("defs.zig");
const Byte = defs.Byte;
const Memory = @import("Memory.zig");
const MemoryAccessError = Memory.MemAccessError;
const MemorySegment = Memory.MemorySegment;
const VirtualAddress = Memory.VirtualAddress;
const Allocator = std.mem.Allocator;
const RegisterFile = @import("RegisterFile.zig");
const Register = RegisterFile.Register;
const Instructions = @import("Instructions.zig");
const Instruction = Instructions.Instruction;
const Interrupts = @import("Interrupts.zig");
const ExceptionCode = Interrupts.ExceptionCode;
const ePriorities = Interrupts.ePriorities;
const Interrupt = Interrupts.Interrupt;

const ArrayList = std.ArrayList;

const Cpu = @This();

mem: *Memory,
clockHertz: u64,

registers: *RegisterFile,
allocator: Allocator,

privilegeMode: PrivMode,
pendingInterrupts: ArrayList(Interrupt),

pub const PrivMode = enum(u2) {
    USER = 0b00,
    SUPERVISOR = 0b01,
    MACHINE = 0b11,
};

pub fn New(clockHertz: u64, allocator: Allocator, begin: VirtualAddress) !Cpu {
    var mem: *Memory = try Memory.New(allocator);
    var r: *RegisterFile = try RegisterFile.New(allocator);
    var ints = ArrayList(Interrupt).init(allocator);
    var cpu: Cpu = Cpu{
        .privilegeMode = .MACHINE,
        .mem = mem,
        .clockHertz = clockHertz,
        .registers = r,
        .allocator = allocator,
        .pendingInterrupts = ints,
    };
    cpu.registers.PCHandle().Write(begin);

    return cpu;
}

pub fn RaiseInterrupt(self: *Cpu, code: u6, isException: bool, data: u64) !void {
    var int = try Interrupt.New(code, isException, data);
    try self.pendingInterrupts.append(int);
}

pub fn AddMemorySegment(self: *Cpu, seg: *MemorySegment) !void {
    return self.mem.AddSegment(seg);
}

pub fn Fetch(self: *Cpu) !u32 {
    var instrBytes = [4]Byte{ 0, 0, 0, 0 };
    var pc = self.registers.PCHandle();
    try self.mem.Read(pc.Read(), &instrBytes);
    pc.Write(pc.Read() + 4);
    return std.mem.readIntLittle(u32, &instrBytes);
}

pub const RunReturn = struct {
    cpuState: Cpu,
    t: union(enum) {
        BadInstruction: Instruction,
        UnknownOpcode: struct { opcode: Instructions.Opcode, instr: u32 },
        MemError: *Memory,
        OK: bool,
    },

    pub fn format(
        value: RunReturn,
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) std.os.WriteError!void {
        switch (value.t) {
            .BadInstruction => |instr| return writer.print("{X}: Unimplemented instruction: {s}", .{
                value.cpuState.registers.PCHandle().Read() - 4,
                instr,
            }),
            .UnknownOpcode => |opcode| return writer.print("{X}: Unknown Opcode: {b:0>7} {X:0>8}", .{ value.cpuState.registers.PCHandle().Read() - 4, @intFromEnum(
                opcode.opcode,
            ), opcode.instr }),
            .MemError => |mem| {
                try writer.print("{X}: ", .{value.cpuState.registers.PCHandle().Read()});
                if (mem.failed_data) |data| {
                    return writer.print("Failed to write {X:0<2} to {X}\n", .{ data, mem.failed_addr });
                } else {
                    return writer.print("Failed to read from {X}\n", .{mem.failed_addr});
                }
            },
            .OK => return,
        }
    }
};

pub fn Run(self: *Cpu, endAddr: VirtualAddress, testDataAddr: VirtualAddress, testDataBuffer: []Byte) !RunReturn {
    while (true) {
        var instrb = self.Fetch() catch |err| if (err == MemoryAccessError.NoSegmentMapped) {
            return RunReturn{ .cpuState = self.*, .t = .{
                .MemError = self.mem,
            } };
        } else return err;

        var instr = Instruction.FromInt(instrb) catch |err| switch (err) {
            Instructions.InstructionError.UnknownOpcode => return RunReturn{
                .cpuState = self.*,
                .t = .{ .UnknownOpcode = .{
                    .opcode = @enumFromInt(@as(u7, @truncate(instrb))),
                    .instr = instrb,
                } },
            },
            else => return err,
        };
        std.debug.print("{X}: {s}\n", .{ self.registers.PCHandle().Read() - 4, instr });
        instr.Execute(self) catch |err| {
            switch (err) {
                Instructions.InstructionError.UnimplementedInstruction => return RunReturn{
                    .cpuState = self.*,
                    .t = .{ .BadInstruction = instr },
                },
                Instructions.InstructionError.UnknownOpcode => return RunReturn{
                    .cpuState = self.*,
                    .t = .{ .UnknownOpcode = .{
                        .opcode = @enumFromInt(@as(u7, @truncate(instrb))),
                        .instr = instrb,
                    } },
                },
                MemoryAccessError.NoSegmentMapped => return RunReturn{ .cpuState = self.*, .t = .{
                    .MemError = self.mem,
                } },

                else => return err,
            }
        };
        var mipHandle = self.registers.CSRegisterHandle(.MIP);
        var pcHandle = self.registers.PCHandle();

        // Check if instruction is not aligned
        //if (pcHandle.Read() % 4 != 0) {
        //    std.debug.print("Misaligned instruction access at {X}\n", .{pcHandle.Read()});
        //    var new = mipHandle.Read() | (1 << @intFromEnum(@as(ExceptionCode, .INSTR_ADDR_MA)));
        //    mipHandle.Write(new);
        //} else {
        //    var new = mipHandle.Read() & ~@as(u64, (1 << @intFromEnum(@as(ExceptionCode, .INSTR_ADDR_MA))));
        //    mipHandle.Write(new);
        //}

        // Check for (machine) exceptions
        var servicing = mipHandle.Read();
        for (self.pendingInterrupts.items, 0..) |int, i| {
            var code: u6 = 0;
            switch (int.n) {
                .EXCEPTION => code = @intFromEnum(int.n.EXCEPTION),
                .INTERRUPT => code = @intFromEnum(int.n.INTERRUPT),
            }
            if (servicing & (@as(u64, 1) << code) != 0) {
                // Servicing exception
                // Clear exception bit
                servicing &= ~(@as(u64, 1) << code);

                var intCpy = self.pendingInterrupts.orderedRemove(i);
                try intCpy.Service(self, .MACHINE); // TODO - Implement mideleg, etc...

                break;
            }
        }

        // Check for end

        if (pcHandle.Read() == endAddr) {
            try self.mem.Read(testDataAddr, testDataBuffer);

            return RunReturn{
                .t = .{ .OK = true },
                .cpuState = self.*,
            };
        }
    }
}
