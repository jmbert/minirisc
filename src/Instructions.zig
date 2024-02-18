const std = @import("std");
const RegisterFile = @import("RegisterFile.zig");
const xReg = RegisterFile.xReg;
const csr = RegisterFile.csr;
const Cpu = @import("Cpu.zig");
const defs = @import("defs.zig");
const Byte = defs.Byte;
const Interrupts = @import("Interrupts.zig");
const ExceptionCode = Interrupts.ExceptionCode;
const ePriorities = Interrupts.ePriorities;

pub const InstructionError = error{
    UnknownOpcode,
    UnimplementedInstruction,
    InvalidInstruction,
};

pub const Opcode = enum(u7) {
    OP = 0b0110011,
    OP_32 = 0b0111011,
    OP_IMM = 0b0010011,
    OP_IMM_32 = 0b0011011,
    JAL = 0b1101111,
    JALR = 0b1100111,
    LUI = 0b0110111,
    AUIPC = 0b0010111,
    SYSTEM = 0b1110011,
    BRANCH = 0b1100011,
    MISC_MEM = 0b0001111,
    STORE = 0b100011,
    LOAD = 0b000011,
    _,
};

const funct3_LOAD = enum(u3) {};

const funct3_MISC_MEM = enum(u3) {
    FENCE = 0b000,
    _,
};

const funct3_OPIMM32 = enum(u3) {
    ADDIW = 0b000,
    SLLIW = 0b001,
    SRIW = 0b101,
};

const funct3_SYSTEM = enum(u3) {
    CSRRW = 0b001,
    CSRRS = 0b010,
    CSRRC = 0b011,
    CSRRWI = 0b101,
    CSRRSI = 0b110,
    CSRRCI = 0b111,
    PRIV = 0b000,
};

pub const Instruction = union(enum) {
    R: InstructionR,
    I: InstructionI,
    S: InstructionS,
    B: InstructionB,
    U: InstructionU,
    J: InstructionJ,
    CSR: InstructionCSR,
    PRIV: InstructionPRIV,
    FENCE: InstructionFence,

    pub fn FromInt(i: u32) InstructionError!Instruction {
        var opcode: Opcode = @enumFromInt(@as(u7, @truncate(i)));
        var instr = switch (opcode) {
            .OP, .OP_32 => Instruction{
                .R = try InstructionR.FromInt(i),
            },
            .STORE => Instruction{
                .S = try InstructionS.FromInt(i),
            },
            .LUI, .AUIPC => Instruction{
                .U = try InstructionU.FromInt(i),
            },
            .OP_IMM, .OP_IMM_32, .LOAD, .JALR => Instruction{
                .I = try InstructionI.FromInt(i),
            },
            .JAL => Instruction{
                .J = try InstructionJ.FromInt(i),
            },
            .SYSTEM => blk: {
                var funct3: funct3_SYSTEM = @enumFromInt(@as(u3, @truncate(i >> 12)));

                if (funct3 == .PRIV) {
                    break :blk Instruction{
                        .PRIV = try InstructionPRIV.FromInt(i),
                    };
                } else {
                    break :blk Instruction{
                        .CSR = try InstructionCSR.FromInt(i),
                    };
                }
            },
            .BRANCH => Instruction{
                .B = try InstructionB.FromInt(i),
            },
            .MISC_MEM => blk: {
                var funct3: funct3_MISC_MEM = @enumFromInt(@as(u3, @truncate(i >> 12)));
                break :blk switch (funct3) {
                    .FENCE => Instruction{
                        .FENCE = try InstructionFence.FromInt(i),
                    },
                    else => return InstructionError.UnknownOpcode,
                };
            },
            _ => return InstructionError.UnknownOpcode,
        };
        return instr;
    }

    pub fn Execute(self: *Instruction, cpu: *Cpu) !void {
        return switch (self.*) {
            .R => self.R.Execute(cpu),
            .I => self.I.Execute(cpu),
            .S => self.S.Execute(cpu),
            .B => self.B.Execute(cpu),
            .U => self.U.Execute(cpu),
            .J => self.J.Execute(cpu),
            .CSR => self.CSR.Execute(cpu),
            .PRIV => self.PRIV.Execute(cpu),
            .FENCE => self.FENCE.Execute(cpu),
        };
    }

    pub fn format(
        value: Instruction,
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) std.os.WriteError!void {
        return switch (value) {
            .R => writer.print("{any}", .{value.R}),
            .I => writer.print("{any}", .{value.I}),
            .S => writer.print("{any}", .{value.S}),
            .B => writer.print("{any}", .{value.B}),
            .U => writer.print("{any}", .{value.U}),
            .J => writer.print("{any}", .{value.J}),
            .CSR => writer.print("{any}", .{value.CSR}),
            .PRIV => writer.print("{any}", .{value.PRIV}),
            .FENCE => writer.print("{any}", .{value.FENCE}),
        };
    }
};

pub fn writeOperation(
    opcode: Opcode,
    funct3: ?u3,
    funct7: ?u7,
    funct12: ?u12,
    writer: anytype,
) std.os.WriteError!void {
    switch (opcode) {
        .JAL, .LUI, .AUIPC, .JALR => {
            try writer.print("{s}", .{@tagName(opcode)});
        },
        .MISC_MEM => {
            try writer.print("{s}", .{@tagName(@as(funct3_MISC_MEM, @enumFromInt(funct3.?)))});
        },
        .OP => {
            var funct3op: funct3_OP = @enumFromInt(funct3.?);
            switch (funct3op) {
                .ADDSUB => {
                    if (funct7.? == 0b0100000) {
                        return writer.print("SUB", .{});
                    } else {
                        return writer.print("ADD", .{});
                    }
                },
                .SR => {
                    if (funct7.? == 0b0100000) {
                        return writer.print("SRA", .{});
                    } else {
                        return writer.print("SRL", .{});
                    }
                },
                else => return writer.print("{s}", .{@tagName(funct3op)}),
            }
        },
        .OP_32 => {
            var funct3op: funct3_OP_32 = @enumFromInt(funct3.?);
            switch (funct3op) {
                .ADDSUBW => {
                    if (funct7.? == 0b0100000) {
                        return writer.print("SUBW", .{});
                    } else {
                        return writer.print("ADDW", .{});
                    }
                },
                .SRW => {
                    if (funct7.? == 0b0100000) {
                        return writer.print("SRAW", .{});
                    } else {
                        return writer.print("SRLW", .{});
                    }
                },
                else => return writer.print("{s}", .{@tagName(funct3op)}),
            }
        },
        .OP_IMM => {
            var funct3op: funct3_OP_IMM = @enumFromInt(funct3.?);
            if (funct3op == .SRI) {
                if (funct7.? == 0b0100000) {
                    return writer.print("SRAI", .{});
                } else {
                    return writer.print("SRLI", .{});
                }
            }
            return writer.print("{s}", .{@tagName(funct3op)});
        },
        .OP_IMM_32 => {
            try writer.print("{s}", .{@tagName(@as(funct3_OPIMM32, @enumFromInt(funct3.?)))});
        },
        .STORE => {
            try writer.print("{s}", .{switch (funct3.?) {
                0 => "SB",
                1 => "SH",
                2 => "SW",
                3 => "SD",
                else => return std.os.WriteError.InvalidArgument,
            }});
        },
        .SYSTEM => {
            var funct3_sys: funct3_SYSTEM = @enumFromInt(funct3.?);
            switch (funct3_sys) {
                .PRIV => try writer.print("{s}", .{@tagName(@as(funct12_PRIV, @enumFromInt(funct12.?)))}),
                else => try writer.print("{s}", .{@tagName(funct3_sys)}),
            }
        },
        .BRANCH => {
            try writer.print("{s}", .{@tagName(@as(funct3_BRANCH, @enumFromInt(funct3.?)))});
        },
        .LOAD => {
            try writer.print("{s}", .{switch (funct3.?) {
                0 => "LB",
                1 => "LH",
                2 => "LW",
                3 => "LD",
                else => return std.os.WriteError.InvalidArgument,
            }});
        },
        _ => return std.os.WriteError.InvalidArgument,
    }
}

const funct3_OP = enum(u3) {
    ADDSUB = 0b000,
    SLL = 0b001,
    SLT = 0b010,
    SLTU = 0b011,
    XOR = 0b100,
    SR = 0b101,
    OR = 0b110,
    AND = 0b111,
};

const funct3_OP_32 = enum(u3) {
    ADDSUBW = 0b000,
    SLLW = 0b001,
    SRW = 0b101,
};

const funct3_OP_LOAD = enum(u3) {
    LB,
    LH,
    LW,
    LBU,
    LHU,
};

const funct3_OP_IMM = enum(u3) {
    ADDI,
    SLLI,
    SLTI,
    SLTIU,
    XORI,
    SRI,
    ORI,
    ANDI,
};

const funct3_BRANCH = enum(u3) {
    BEQ = 0b000,
    BNE = 0b001,

    BLT = 0b100,
    BGE = 0b101,

    BLTU = 0b110,
    BGEU = 0b111,
};

pub const InstructionR = struct {
    opcode: Opcode,
    rd: xReg,
    funct3: u3,
    rs1: xReg,
    rs2: xReg,
    funct7: u7,

    pub fn FromInt(i: u32) InstructionError!InstructionR {
        var opcode: Opcode = @enumFromInt(@as(u7, @truncate(i)));
        var rd: xReg = @truncate(i >> 7);
        var funct3: u3 = @truncate(i >> 12);
        var rs1: xReg = @truncate(i >> 15);
        var rs2: xReg = @truncate(i >> 20);
        var funct7: u7 = @truncate(i >> 25);
        var s: InstructionR = InstructionR{
            .opcode = opcode,
            .rd = rd,
            .funct3 = funct3,
            .rs1 = rs1,
            .rs2 = rs2,
            .funct7 = funct7,
        };
        return s;
    }

    pub fn Execute(self: *InstructionR, cpu: *Cpu) !void {
        switch (self.opcode) {
            .OP => {
                var rdHandle = cpu.registers.XRegisterHandle(self.rd);
                var rs1Handle = cpu.registers.XRegisterHandle(self.rs1);
                var rs2Handle = cpu.registers.XRegisterHandle(self.rs2);
                var funct3_op: funct3_OP = @enumFromInt(self.funct3);
                switch (funct3_op) {
                    .ADDSUB => {
                        if (self.funct7 != 0b0100000) {
                            var rs1: u64 = rs1Handle.Read();
                            var rs2: u64 = rs2Handle.Read();
                            rdHandle.Write(@addWithOverflow(rs1, rs2).@"0");
                        } else {
                            var rs1: u64 = rs1Handle.Read();
                            var rs2: u64 = rs2Handle.Read();
                            rdHandle.Write(@subWithOverflow(rs1, rs2).@"0");
                        }
                    },
                    .AND => {
                        var rs1: u64 = rs1Handle.Read();
                        var rs2: u64 = rs2Handle.Read();
                        rdHandle.Write(rs1 & rs2);
                    },
                    .OR => {
                        var rs1: u64 = rs1Handle.Read();
                        var rs2: u64 = rs2Handle.Read();
                        rdHandle.Write(rs1 | rs2);
                    },
                    .XOR => {
                        var rs1: u64 = rs1Handle.Read();
                        var rs2: u64 = rs2Handle.Read();
                        rdHandle.Write(rs1 ^ rs2);
                    },
                    .SLL => {
                        var rs1: u64 = rs1Handle.Read();
                        var rs2: u64 = rs2Handle.Read();
                        var shift: u6 = @truncate(rs2);
                        var val = @shlWithOverflow(rs1, shift).@"0";
                        rdHandle.Write(val);
                    },
                    .SR => {
                        var rs1: u64 = rs1Handle.Read();
                        var rs2: u64 = rs2Handle.Read();
                        if (self.funct7 != 0b0100000) {
                            var shift: u6 = @truncate(rs2);
                            var val = rs1 >> shift;
                            rdHandle.Write(val);
                        } else {
                            var rs1i: i64 = @bitCast(rs1);
                            var shift: u6 = @truncate(rs2);
                            rdHandle.Write(@bitCast(rs1i >> shift));
                        }
                    },
                    .SLT => {
                        var rs1: u64 = rs1Handle.Read();
                        var rs2: u64 = rs2Handle.Read();
                        if (@as(i64, @bitCast(rs1)) < @as(i64, @bitCast(rs2))) {
                            rdHandle.Write(1);
                        } else {
                            rdHandle.Write(0);
                        }
                    },
                    .SLTU => {
                        var rs1: u64 = rs1Handle.Read();
                        var rs2: u64 = rs2Handle.Read();
                        if (rs1 < rs2) {
                            rdHandle.Write(1);
                        } else {
                            rdHandle.Write(0);
                        }
                    },
                }
            },
            .OP_32 => {
                var rdHandle = cpu.registers.XRegisterHandle(self.rd);
                var rs1Handle = cpu.registers.XRegisterHandle(self.rs1);
                var rs2Handle = cpu.registers.XRegisterHandle(self.rs2);
                var funct3_op: funct3_OP_32 = @enumFromInt(self.funct3);
                switch (funct3_op) {
                    .ADDSUBW => {
                        if (self.funct7 != 0b0100000) {
                            var val = @addWithOverflow(rs1Handle.Read(), rs2Handle.Read()).@"0";
                            var valrextend = signExtend(u32, @as(u32, @truncate(val)));
                            rdHandle.Write(@truncate(valrextend));
                        } else {
                            var val = @subWithOverflow(rs1Handle.Read(), rs2Handle.Read()).@"0";
                            var valrextend = signExtend(u32, @as(u32, @truncate(val)));
                            rdHandle.Write(@truncate(valrextend));
                        }
                    },
                    .SLLW => {
                        var rs1: u32 = @truncate(rs1Handle.Read());
                        var rs2: u32 = @truncate(rs2Handle.Read());
                        var shift: u5 = @truncate(rs2);
                        var val: u32 = @shlWithOverflow(rs1, shift).@"0";
                        var valrextend = signExtend(u32, val);
                        rdHandle.Write(valrextend);
                    },
                    .SRW => {
                        var rs1: u32 = @truncate(rs1Handle.Read());
                        var rs2: u32 = @truncate(rs2Handle.Read());
                        if (self.funct7 != 0b0100000) {
                            var shift: u5 = @truncate(rs2);
                            var val: u32 = rs1 >> shift;
                            var valrextend = signExtend(u32, val);
                            rdHandle.Write(valrextend);
                        } else {
                            var rs1i: i32 = @bitCast(rs1);
                            var shift: u5 = @truncate(rs2);
                            var val: i32 = @bitCast(rs1i >> shift);
                            var valrextend: u64 = @bitCast(@as(i64, @intCast(val)));
                            rdHandle.Write(valrextend);
                        }
                    },
                }
            },
            else => return InstructionError.UnknownOpcode,
        }
    }
    pub fn format(
        value: InstructionR,
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) std.os.WriteError!void {
        try writeOperation(value.opcode, value.funct3, value.funct7, null, writer);
        try writer.print(" ", .{});
        try RegisterFile.writexReg(value.rd, writer);
        try writer.print(", ", .{});
        try RegisterFile.writexReg(value.rs1, writer);
        try writer.print(", ", .{});
        try RegisterFile.writexReg(value.rs2, writer);
    }
};

pub const InstructionI = struct {
    opcode: Opcode,
    rd: xReg,
    funct3: u3,
    rs1: xReg,
    imm_11_0: u12,

    pub fn FromInt(i: u32) InstructionError!InstructionI {
        var opcode: Opcode = @enumFromInt(@as(u7, @truncate(i)));
        var rd: xReg = @truncate(i >> 7);
        var funct3: u3 = @truncate(i >> 12);
        var rs1: xReg = @truncate(i >> 15);
        var imm_11_0: u12 = @truncate(i >> 20);
        var s: InstructionI = InstructionI{
            .opcode = opcode,
            .rd = rd,
            .funct3 = funct3,
            .rs1 = rs1,
            .imm_11_0 = imm_11_0,
        };
        return s;
    }

    pub fn Execute(self: *InstructionI, cpu: *Cpu) !void {
        var rdHandle = cpu.registers.XRegisterHandle(self.rd);
        var rs1Handle = cpu.registers.XRegisterHandle(self.rs1);
        var immsextend = signExtend(i12, @as(i12, @bitCast(self.imm_11_0)));
        switch (self.opcode) {
            .OP_IMM => {
                var f3: funct3_OP_IMM = @enumFromInt(self.funct3);
                switch (f3) {
                    .XORI => {
                        rdHandle.Write(rs1Handle.Read() ^ immsextend);
                    },
                    .ORI => {
                        rdHandle.Write(rs1Handle.Read() | immsextend);
                    },
                    .ANDI => {
                        rdHandle.Write(rs1Handle.Read() & immsextend);
                    },
                    .ADDI => {
                        var new = @addWithOverflow(rs1Handle.Read(), immsextend).@"0";
                        rdHandle.Write(new);
                    },
                    .SLLI => {
                        var shift: u6 = @truncate(self.imm_11_0);
                        var val = @shlWithOverflow(rs1Handle.Read(), shift).@"0";
                        rdHandle.Write(val);
                    },
                    .SRI => {
                        var funct7: u7 = @truncate(self.imm_11_0 >> 6);
                        if (funct7 != 0b010000) { // SRLI
                            var rs1: u64 = rs1Handle.Read();
                            var shift: u6 = @truncate(immsextend);
                            var val = rs1 >> shift;
                            rdHandle.Write(val); // SRAI
                        } else {
                            var rs1i: i64 = @bitCast(rs1Handle.Read());
                            var shift: u6 = @truncate(immsextend);
                            rdHandle.Write(@bitCast(rs1i >> shift));
                        }
                    },
                    .SLTI => {
                        if (@as(i64, @bitCast(rs1Handle.Read())) < @as(i64, @bitCast(immsextend))) {
                            rdHandle.Write(1);
                        } else {
                            rdHandle.Write(0);
                        }
                    },
                    .SLTIU => {
                        if (rs1Handle.Read() < immsextend) {
                            rdHandle.Write(1);
                        } else {
                            rdHandle.Write(0);
                        }
                    },
                }
            },
            .OP_IMM_32 => {
                var f3: funct3_OPIMM32 = @enumFromInt(self.funct3);
                switch (f3) {
                    .ADDIW => {
                        var val = @addWithOverflow(rs1Handle.Read(), immsextend).@"0";
                        var valrextend = signExtend(u32, @as(u32, @truncate(val)));
                        rdHandle.Write(@truncate(valrextend));
                    },
                    .SLLIW => {
                        var rs1: u64 = rs1Handle.Read();
                        var shift: u6 = @truncate(immsextend);
                        var val = @shlWithOverflow(rs1, shift).@"0";
                        var valrextend = signExtend(u32, @as(u32, @truncate(val)));
                        rdHandle.Write(valrextend);
                    },
                    .SRIW => {
                        var funct7: u7 = @truncate(immsextend >> 5);
                        var rs1: u32 = @truncate(rs1Handle.Read());
                        if (funct7 != 0b0100000) {
                            var shift: u5 = @truncate(immsextend);
                            var val: u32 = rs1 >> shift;
                            var valrextend = signExtend(u32, val);
                            rdHandle.Write(valrextend);
                        } else {
                            var rs1i: i32 = @bitCast(rs1);
                            var shift: u5 = @truncate(immsextend);
                            var val: i32 = rs1i >> shift;
                            var valrextend = signExtend(i32, val);
                            rdHandle.Write(valrextend);
                        }
                    },
                }
            },
            .LOAD => {
                var baseHandle = cpu.registers.XRegisterHandle(self.rs1);
                var destHandle = cpu.registers.XRegisterHandle(self.rd);
                var offset: i64 = @bitCast(signExtend(u12, self.imm_11_0));
                var addr: u64 = @bitCast(@as(i64, @intCast(baseHandle.Read())) + offset);
                var initBuffer = ([_]u8{0} ** 8);
                var buffer: []Byte = &initBuffer;
                try cpu.mem.Read(addr, buffer);
                var val64: u64 = 0;

                switch (self.funct3) {
                    0b011 => {
                        var bufmini: *[8]u8 = @ptrCast(buffer);
                        val64 = std.mem.readIntLittle(u64, bufmini);
                    },
                    0b010 => {
                        var bufmini: *[4]u8 = @ptrCast(buffer);
                        var val: u32 = std.mem.readIntLittle(u32, bufmini);
                        val64 = signExtend(u32, val);
                    },
                    0b001 => {
                        var bufmini: *[2]u8 = @ptrCast(buffer);
                        var val: u16 = std.mem.readIntLittle(u16, bufmini);
                        val64 = signExtend(u16, val);
                    },
                    0b000 => {
                        var bufmini: *[1]u8 = @ptrCast(buffer);
                        var val: u8 = std.mem.readIntLittle(u8, bufmini);
                        val64 = signExtend(u8, val);
                    },
                    0b110 => {
                        var bufmini: *[4]u8 = @ptrCast(buffer);
                        var val: u32 = std.mem.readIntLittle(u32, bufmini);
                        val64 = @intCast(val);
                    },
                    0b101 => {
                        var bufmini: *[2]u8 = @ptrCast(buffer);
                        var val: u16 = std.mem.readIntLittle(u16, bufmini);
                        val64 = @intCast(val);
                    },
                    0b100 => {
                        var bufmini: *[1]u8 = @ptrCast(buffer);
                        var val: u8 = std.mem.readIntLittle(u8, bufmini);
                        val64 = @intCast(val);
                    },
                    else => return InstructionError.InvalidInstruction,
                }

                destHandle.Write(val64);
            },
            .JALR => {
                var pcHandle = cpu.registers.PCHandle();
                var newPC: u64 = @bitCast(@as(i64, @bitCast(rs1Handle.Read())) + @as(i64, @bitCast(immsextend)));
                newPC &= ~@as(u64, 1); // Clear least significant bit

                var oldPC = pcHandle.Read();

                pcHandle.Write(newPC);
                rdHandle.Write(oldPC);
            },
            else => return InstructionError.UnknownOpcode,
        }
    }

    pub fn format(
        value: InstructionI,
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) std.os.WriteError!void {
        var funct7: u7 = @truncate(value.imm_11_0 >> 6);
        try writeOperation(value.opcode, value.funct3, funct7, null, writer);
        try writer.print(" ", .{});
        try RegisterFile.writexReg(value.rd, writer);
        try writer.print(", ", .{});

        if (value.opcode == .LOAD) {
            try writer.print("{X}(", .{@as(i12, @bitCast(value.imm_11_0))});
            try RegisterFile.writexReg(value.rs1, writer);
            try writer.print(")", .{});
            return;
        }

        try RegisterFile.writexReg(value.rs1, writer);
        try writer.print(", ", .{});
        switch (value.opcode) {
            .OP => {
                switch (@as(funct3_OP_IMM, @enumFromInt(value.funct3))) {
                    .SRI,
                    .SLLI,
                    => try writer.print("{X}", .{@as(u6, @truncate(value.imm_11_0))}),

                    else => try writer.print("{X}", .{@as(i12, @bitCast(value.imm_11_0))}),
                }
            },
            .OP_32 => {
                switch (@as(funct3_OPIMM32, @enumFromInt(value.funct3))) {
                    .SRIW,
                    .SLLIW,
                    => try writer.print("{X}", .{@as(u6, @truncate(value.imm_11_0))}),

                    else => try writer.print("{X}", .{@as(i12, @bitCast(value.imm_11_0))}),
                }
            },
            else => try writer.print("{X}", .{@as(i12, @bitCast(value.imm_11_0))}),
        }
    }
};

pub const InstructionS = struct {
    opcode: Opcode,
    imm_4_0: u5,
    funct3: u3,
    rs1: xReg,
    rs2: xReg,
    imm_11_5: u7,

    pub fn FromInt(i: u32) InstructionError!InstructionS {
        var opcode: Opcode = @enumFromInt(@as(u7, @truncate(i)));
        var imm_4_0: u5 = @truncate(i >> 7);
        var funct3: u3 = @truncate(i >> 12);
        var rs1: xReg = @truncate(i >> 15);
        var rs2: xReg = @truncate(i >> 20);
        var imm_11_5: u7 = @truncate(i >> 25);
        var s: InstructionS = InstructionS{
            .opcode = opcode,
            .imm_4_0 = imm_4_0,
            .funct3 = funct3,
            .rs1 = rs1,
            .rs2 = rs2,
            .imm_11_5 = imm_11_5,
        };
        return s;
    }

    pub fn Execute(self: *InstructionS, cpu: *Cpu) !void {
        switch (self.opcode) {
            .STORE => {
                var baseHandle = cpu.registers.XRegisterHandle(self.rs1);
                var sourceHandle = cpu.registers.XRegisterHandle(self.rs2);
                var source = sourceHandle.Read();
                var offset: i64 = @intCast(@as(i12, @bitCast(self.getImm())));
                var addr: u64 = @bitCast(@as(i64, @intCast(baseHandle.Read())) + offset);
                var initBuffer = ([_]u8{0} ** 8);
                var buffer: []Byte = &initBuffer;

                switch (self.funct3) {
                    3 => {
                        std.mem.writeIntLittle(u64, &initBuffer, source);
                    },
                    2 => {
                        var buf = ([_]u8{0} ** 4);
                        std.mem.writeIntLittle(u32, &buf, @as(u32, @truncate(source)));
                        buffer = &buf;
                    },
                    1 => {
                        var buf = ([_]u8{0} ** 2);
                        std.mem.writeIntLittle(u16, &buf, @as(u16, @truncate(source)));
                        buffer = &buf;
                    },
                    0 => {
                        var buf = ([_]u8{0} ** 1);
                        std.mem.writeIntLittle(u8, &buf, @as(u8, @truncate(source)));
                        buffer = &buf;
                    },
                    else => return InstructionError.InvalidInstruction,
                }

                try cpu.mem.Write(addr, buffer);
            },
            else => return InstructionError.UnknownOpcode,
        }
    }

    pub fn format(
        value: InstructionS,
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) std.os.WriteError!void {
        try writeOperation(value.opcode, value.funct3, null, null, writer);
        try writer.print(" ", .{});
        try RegisterFile.writexReg(value.rs2, writer);
        try writer.print(", ", .{});
        try writer.print("{X}(", .{@as(i12, @bitCast(value.getImm()))});
        try RegisterFile.writexReg(value.rs1, writer);
        try writer.print(")", .{});
        return;
    }

    pub fn getImm(self: InstructionS) u12 {
        return @as(u12, @intCast(self.imm_4_0)) | (@as(u12, @intCast(self.imm_11_5)) << 5);
    }
};

pub const InstructionB = struct {
    opcode: Opcode,
    imm_11: u1,
    imm_4_1: u4,
    funct3: u3,
    rs1: xReg,
    rs2: xReg,
    imm_10_5: u6,
    imm_12: u1,
    pub fn FromInt(i: u32) InstructionError!InstructionB {
        var opcode: Opcode = @enumFromInt(@as(u7, @truncate(i)));
        var imm_11: u1 = @truncate(i >> 7);
        var imm_4_1: u4 = @truncate(i >> 8);
        var funct3: u3 = @truncate(i >> 12);
        var rs1: xReg = @truncate(i >> 15);
        var rs2: xReg = @truncate(i >> 20);
        var imm_10_5: u6 = @truncate(i >> 25);
        var imm_12: u1 = @truncate(i >> 31);

        var s: InstructionB = InstructionB{
            .opcode = opcode,
            .imm_11 = imm_11,
            .imm_4_1 = imm_4_1,
            .funct3 = funct3,
            .rs1 = rs1,
            .rs2 = rs2,
            .imm_10_5 = imm_10_5,
            .imm_12 = imm_12,
        };
        return s;
    }

    pub fn Execute(self: *InstructionB, cpu: *Cpu) InstructionError!void {
        switch (self.opcode) {
            .BRANCH => {
                var funct3_b: funct3_BRANCH = @enumFromInt(self.funct3);
                var pcHandle = cpu.registers.PCHandle();
                var rs1Handle = cpu.registers.XRegisterHandle(self.rs1);
                var rs2Handle = cpu.registers.XRegisterHandle(self.rs2);
                var immsextend: i64 = @intCast(self.getImm());
                switch (funct3_b) {
                    .BEQ => {
                        if (rs1Handle.Read() == rs2Handle.Read()) {
                            var newPC: u64 = @bitCast(@addWithOverflow(@as(i64, @bitCast(pcHandle.Read())) - 4, immsextend).@"0");
                            pcHandle.Write(newPC);
                        }
                    },
                    .BNE => {
                        if (rs1Handle.Read() != rs2Handle.Read()) {
                            var newPC: u64 = @bitCast(@addWithOverflow(@as(i64, @bitCast(pcHandle.Read())) - 4, immsextend).@"0");
                            pcHandle.Write(newPC);
                        }
                    },
                    .BGE => {
                        if (@as(i64, @bitCast(rs1Handle.Read())) >= @as(i64, @bitCast(rs2Handle.Read()))) {
                            var newPC: u64 = @bitCast(@addWithOverflow(@as(i64, @bitCast(pcHandle.Read())) - 4, immsextend).@"0");
                            pcHandle.Write(newPC);
                        }
                    },
                    .BGEU => {
                        if (rs1Handle.Read() >= rs2Handle.Read()) {
                            var newPC: u64 = @bitCast(@addWithOverflow(@as(i64, @bitCast(pcHandle.Read())) - 4, immsextend).@"0");
                            pcHandle.Write(newPC);
                        }
                    },
                    .BLT => {
                        if (@as(i64, @bitCast(rs1Handle.Read())) < @as(i64, @bitCast(rs2Handle.Read()))) {
                            var newPC: u64 = @bitCast(@addWithOverflow(@as(i64, @bitCast(pcHandle.Read())) - 4, immsextend).@"0");
                            pcHandle.Write(newPC);
                        }
                    },
                    .BLTU => {
                        if (rs1Handle.Read() < rs2Handle.Read()) {
                            var newPC: u64 = @bitCast(@addWithOverflow(@as(i64, @bitCast(pcHandle.Read())) - 4, immsextend).@"0");
                            pcHandle.Write(newPC);
                        }
                    },
                }
            },
            else => return InstructionError.UnknownOpcode,
        }
    }

    pub fn format(
        value: InstructionB,
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) std.os.WriteError!void {
        try writeOperation(value.opcode, value.funct3, null, null, writer);
        try writer.print(" ", .{});
        try writer.print("{X}", .{value.getImm()});
        try writer.print(", ", .{});
        try RegisterFile.writexReg(value.rs1, writer);
        try writer.print(", ", .{});
        try RegisterFile.writexReg(value.rs2, writer);
    }

    fn getImm(self: InstructionB) i13 {
        var out: u13 = 0;
        out |= @as(u13, self.imm_4_1) << 1;
        out |= @as(u13, self.imm_10_5) << 5;
        out |= @as(u13, self.imm_11) << 11;
        out |= @as(u13, self.imm_12) << 12;

        return @bitCast(out);
    }
};

pub const InstructionU = struct {
    opcode: Opcode,
    rd: xReg,
    imm_31_12: u21,

    pub fn FromInt(i: u32) InstructionError!InstructionU {
        var opcode: Opcode = @enumFromInt(@as(u7, @truncate(i)));
        var rd: xReg = @truncate(i >> 7);
        var imm_31_12: u21 = @truncate(i >> 12);
        var s: InstructionU = InstructionU{
            .opcode = opcode,
            .rd = rd,
            .imm_31_12 = imm_31_12,
        };
        return s;
    }

    pub fn Execute(self: *InstructionU, cpu: *Cpu) !void {
        var rdHandle = cpu.registers.XRegisterHandle(self.rd);
        var immsextend = signExtend(u32, @as(u32, self.imm_31_12) << 12);
        switch (self.opcode) {
            .LUI => {
                rdHandle.Write(immsextend);
            },
            .AUIPC => {
                var pcHandle = cpu.registers.PCHandle();
                var pcV = pcHandle.Read() - 4;
                var iimmsextend: i64 = @bitCast(immsextend);
                var imm: i64 = iimmsextend + @as(i64, @bitCast(pcV));
                rdHandle.Write(@bitCast(imm));
            },
            else => return InstructionError.UnknownOpcode,
        }
    }

    pub fn format(
        value: InstructionU,
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) std.os.WriteError!void {
        try writeOperation(value.opcode, null, null, null, writer);
        try writer.print(" ", .{});
        try RegisterFile.writexReg(value.rd, writer);
        try writer.print(", ", .{});
        try writer.print("{X}", .{@as(u64, value.imm_31_12)});
    }
};

pub const InstructionJ = struct {
    opcode: Opcode,
    rd: xReg,
    imm_19_12: u8,
    imm_11: u1,
    imm_10_1: u10,
    imm_20: u1,

    pub fn FromInt(i: u32) InstructionError!InstructionJ {
        var opcode: Opcode = @enumFromInt(@as(u7, @truncate(i)));
        var rd: xReg = @truncate(i >> 7);
        var imm_19_12: u8 = @truncate(i >> 12);
        var imm_11: u1 = @truncate(i >> 20);
        var imm_10_1: u10 = @truncate(i >> 21);
        var imm_20: u1 = @truncate(i >> 31);
        var s: InstructionJ = InstructionJ{
            .opcode = opcode,
            .rd = rd,
            .imm_19_12 = imm_19_12,
            .imm_11 = imm_11,
            .imm_10_1 = imm_10_1,
            .imm_20 = imm_20,
        };
        return s;
    }

    pub fn Execute(self: *InstructionJ, cpu: *Cpu) !void {
        var rdHandle = cpu.registers.XRegisterHandle(self.rd);
        var pcHandle = cpu.registers.PCHandle();
        var immsextendu = signExtend(u21, @as(u21, @bitCast(self.getImm())));
        var immsextend = @as(i64, @bitCast(immsextendu));
        switch (self.opcode) {
            .JAL => {
                rdHandle.Write(pcHandle.Read());
                pcHandle.Write(@bitCast(@as(i64, @bitCast(pcHandle.Read() - 4)) + (immsextend)));
            },
            else => return InstructionError.UnknownOpcode,
        }
    }

    pub fn format(
        value: InstructionJ,
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) std.os.WriteError!void {
        try writer.print("JAL ", .{});
        try RegisterFile.writexReg(value.rd, writer);
        try writer.print(", ", .{});
        try writer.print("{X}", .{value.getImm()});
    }

    fn getImm(self: InstructionJ) i21 {
        var out: u21 = 0;
        out |= @as(u21, self.imm_10_1) << 1;
        out |= @as(u21, self.imm_11) << 11;
        out |= @as(u21, self.imm_19_12) << 12;
        out |= @as(u21, self.imm_20) << 20;

        return @bitCast(out);
    }
};

pub const InstructionCSR = struct {
    opcode: Opcode,
    rd: xReg,
    funct3: u3,
    rs1: xReg,
    csr: csr,

    pub fn FromInt(i: u32) InstructionError!InstructionCSR {
        var opcode: Opcode = @enumFromInt(@as(u7, @truncate(i)));
        var rd: xReg = @truncate(i >> 7);
        var funct3: u3 = @truncate(i >> 12);
        var rs1: xReg = @truncate(i >> 15);
        var csr_to: csr = @enumFromInt(@as(u12, @truncate(i >> 20)));
        var s: InstructionCSR = InstructionCSR{
            .opcode = opcode,
            .rd = rd,
            .funct3 = funct3,
            .rs1 = rs1,
            .csr = csr_to,
        };
        return s;
    }

    pub fn Execute(self: *InstructionCSR, cpu: *Cpu) !void {
        switch (self.opcode) {
            .SYSTEM => {
                var funct3csr: funct3_SYSTEM = @enumFromInt(self.funct3);
                var csrhandle = cpu.registers.CSRegisterHandle(self.csr);
                var rdhandle = cpu.registers.XRegisterHandle(self.rd);
                var rs1handle = cpu.registers.XRegisterHandle(self.rs1);
                switch (funct3csr) {
                    .CSRRW => {
                        var csrV = csrhandle.Read();
                        var rs1 = rs1handle.Read();
                        csrhandle.Write(rs1);
                        rdhandle.Write(csrV);
                    },
                    .CSRRS => {
                        var csrV = csrhandle.Read();
                        var rs1 = rs1handle.Read();
                        var newcsrV = csrV | rs1;
                        csrhandle.Write(newcsrV);
                        rdhandle.Write(csrV);
                    },
                    .CSRRC => {
                        var csrV = csrhandle.Read();
                        var rs1 = rs1handle.Read();
                        var newcsrV = csrV & (~rs1);
                        csrhandle.Write(newcsrV);
                        rdhandle.Write(csrV);
                    },
                    .CSRRWI => {
                        var csrV = csrhandle.Read();
                        var val: u64 = @intCast(self.rs1);
                        csrhandle.Write(val);
                        rdhandle.Write(csrV);
                    },
                    .CSRRSI => {
                        var csrV = csrhandle.Read();
                        var val: u64 = @intCast(self.rs1);
                        var newcsrV = csrV | val;
                        csrhandle.Write(newcsrV);
                        rdhandle.Write(csrV);
                    },
                    .CSRRCI => {
                        var csrV = csrhandle.Read();
                        var val: u64 = @intCast(self.rs1);
                        var newcsrV = csrV & (~val);
                        csrhandle.Write(newcsrV);
                        rdhandle.Write(csrV);
                    },
                    else => return InstructionError.UnknownOpcode,
                }
            },
            else => return InstructionError.UnknownOpcode,
        }
    }

    pub fn format(
        value: InstructionCSR,
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) std.os.WriteError!void {
        try writeOperation(value.opcode, value.funct3, null, null, writer);
        try writer.print(" ", .{});
        try RegisterFile.writecsrReg(value.csr, writer);
        try writer.print(", ", .{});
        try RegisterFile.writexReg(value.rd, writer);
        try writer.print(", ", .{});
        try RegisterFile.writexReg(value.rs1, writer);
    }
};

const funct12_PRIV = enum(u12) {
    ECALL = 0b0000000_00000,
    EBREAK = 0b0000000_00001,
    SRET = 0b0001000_00010,
    MRET = 0b0011000_00010,
    WFI = 0b0001000_00101,
};

pub const InstructionPRIV = struct {
    opcode: Opcode,
    rd: xReg,
    funct3: u3,
    rs1: xReg,
    funct12: u12,

    pub fn FromInt(i: u32) InstructionError!InstructionPRIV {
        var opcode: Opcode = @enumFromInt(@as(u7, @truncate(i)));
        var rd: xReg = @truncate(i >> 7);
        var funct3: u3 = @truncate(i >> 12);
        var rs1: xReg = @truncate(i >> 15);
        var funct12: u12 = @truncate(i >> 20);
        var s: InstructionPRIV = InstructionPRIV{
            .opcode = opcode,
            .rd = rd,
            .funct3 = funct3,
            .rs1 = rs1,
            .funct12 = funct12,
        };
        return s;
    }

    pub fn Execute(self: *InstructionPRIV, cpu: *Cpu) !void {
        switch (self.opcode) {
            .SYSTEM => {
                var funct3System: funct3_SYSTEM = @enumFromInt(self.funct3);
                switch (funct3System) {
                    .PRIV => {
                        var funct12System: funct12_PRIV = @enumFromInt(self.funct12);
                        switch (funct12System) {
                            .MRET => {
                                var rah = cpu.registers.CSRegisterHandle(.MEPC);
                                var ra = rah.Read();
                                var pcHandle = cpu.registers.PCHandle();
                                pcHandle.Write(ra);
                            },
                            .ECALL => {
                                var code: ExceptionCode = .ECALL_M;
                                switch (cpu.privilegeMode) {
                                    .MACHINE => code = .ECALL_M,
                                    .SUPERVISOR => code = .ECALL_S,
                                    .USER => code = .ECALL_U,
                                }

                                try cpu.RaiseException(code, 0);
                            },
                            .EBREAK => {
                                try cpu.RaiseException(.BRK, 0);
                            },
                            else => return InstructionError.UnimplementedInstruction,
                        }
                    },
                    else => return InstructionError.UnknownOpcode,
                }
            },
            else => return InstructionError.UnknownOpcode,
        }
    }

    pub fn format(
        value: InstructionPRIV,
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) std.os.WriteError!void {
        try writeOperation(value.opcode, value.funct3, null, value.funct12, writer);
        try writer.print(" ", .{});
        try RegisterFile.writexReg(value.rd, writer);
        try writer.print(", ", .{});
        try RegisterFile.writexReg(value.rs1, writer);
    }
};

pub const InstructionFence = struct {
    opcode: Opcode,
    rd: xReg,
    funct3: u3,
    rs1: xReg,
    sw: u1,
    sr: u1,
    so: u1,
    si: u1,
    pw: u1,
    pr: u1,
    po: u1,
    pi: u1,
    fm: u4,

    pub fn FromInt(i: u32) InstructionError!InstructionFence {
        var opcode: Opcode = @enumFromInt(@as(u7, @truncate(i)));
        var rd: xReg = @truncate(i >> 7);
        var funct3: u3 = @truncate(i >> 12);
        var rs1: xReg = @truncate(i >> 15);
        var sw: u1 = @truncate(i >> 20);
        var sr: u1 = @truncate(i >> 21);
        var so: u1 = @truncate(i >> 22);
        var si: u1 = @truncate(i >> 23);
        var pw: u1 = @truncate(i >> 24);
        var pr: u1 = @truncate(i >> 25);
        var po: u1 = @truncate(i >> 26);
        var pi: u1 = @truncate(i >> 27);
        var fm: u4 = @truncate(i >> 28);

        var s: InstructionFence = InstructionFence{
            .opcode = opcode,
            .rd = rd,
            .funct3 = funct3,
            .rs1 = rs1,
            .sw = sw,
            .sr = sr,
            .so = so,
            .si = si,
            .pw = pw,
            .pr = pr,
            .po = po,
            .pi = pi,
            .fm = fm,
        };
        return s;
    }

    pub fn Execute(self: *InstructionFence, cpu: *Cpu) !void {
        _ = self;
        _ = cpu;
        // TODO - Add Harts, figure out FENCE, Add FENCE
    }

    pub fn format(
        value: InstructionFence,
        comptime _: []const u8,
        _: std.fmt.FormatOptions,
        writer: anytype,
    ) std.os.WriteError!void {
        try writer.print("FENCE ", .{});
        try RegisterFile.writexReg(value.rd, writer);
        try writer.print(", ", .{});
        try RegisterFile.writexReg(value.rs1, writer);
        try writer.print(", ", .{});
        try writer.print("({X})", .{value.fm});
    }
};

pub fn signExtend(comptime T: type, u: T) u64 {
    if (@typeInfo(T) != .Int) {
        @compileError("Can only sign-extend integers\n");
    }
    const bits: u16 = @typeInfo(T).Int.bits;
    return @bitCast(@as(i64, @as(std.meta.Int(std.builtin.Signedness.signed, bits), @bitCast(u))));
}
