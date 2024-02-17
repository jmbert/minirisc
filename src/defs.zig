const std = @import("std");

pub const Register = u64;
pub const RegisterSize = @typeInfo(Register).Int.bits;

pub const Byte = u8;

pub const mextensionType = enum(u26) {
    A = 0b1 << 0,
    B = 0b1 << 1,
    C = 0b1 << 2,
    D = 0b1 << 3,
    E = 0b1 << 4,
    F = 0b1 << 5,
    G = 0b1 << 6,
    H = 0b1 << 7,
    I = 0b1 << 8,
    J = 0b1 << 9,
    K = 0b1 << 10,
    L = 0b1 << 11,
    M = 0b1 << 12,
    N = 0b1 << 13,
    O = 0b1 << 14,
    P = 0b1 << 15,
    Q = 0b1 << 16,
    R = 0b1 << 17,
    S = 0b1 << 18,
    T = 0b1 << 19,
    U = 0b1 << 20,
    V = 0b1 << 21,
    W = 0b1 << 22,
    X = 0b1 << 23,
    Y = 0b1 << 24,
    Z = 0b1 << 25,
};

const I: mextensionType = .I;

pub const mextension: u26 = @intFromEnum(I);
