//===----------------------------------------------------------------------===//
//
// TinySwift Builtins -- The entire TinySwift "standard library"
//
// This file defines all types, protocols, and functions available to TinySwift
// programs. It is compiled with stock swiftc using -parse-stdlib to access
// Builtin.* intrinsics, producing a .swiftmodule that TinySwift loads as its
// implicit standard library.
//
// Compile with:
//   swiftc -parse-stdlib -parse-as-library \
//     -module-name TinySwift \
//     -emit-module -emit-module-path TinySwift.swiftmodule \
//     -Xfrontend -disable-objc-interop \
//     Builtins.swift
//
//===----------------------------------------------------------------------===//

// MARK: - Marker Protocols

/// A type whose values can be implicitly copied.
/// All TinySwift types are Copyable unless annotated ~Copyable.
public protocol Copyable {}

/// A type that can be copied with a bitwise copy (memcpy).
/// All TinySwift primitive types conform to this.
public protocol BitwiseCopyable: Copyable {}

/// A type that is safe to share across concurrency domains.
/// Included for forward compatibility; TinySwift has no concurrency runtime.
public protocol Sendable {}

// MARK: - Core Protocols

/// A type that can be compared for value equality.
public protocol Equatable {
  static func == (lhs: Self, rhs: Self) -> Bool
}

extension Equatable {
  @_transparent
  public static func != (lhs: Self, rhs: Self) -> Bool {
    return !(lhs == rhs)
  }
}

/// A type that can be compared for ordering.
public protocol Comparable: Equatable {
  static func < (lhs: Self, rhs: Self) -> Bool
}

extension Comparable {
  @_transparent
  public static func > (lhs: Self, rhs: Self) -> Bool {
    return rhs < lhs
  }

  @_transparent
  public static func <= (lhs: Self, rhs: Self) -> Bool {
    return !(rhs < lhs)
  }

  @_transparent
  public static func >= (lhs: Self, rhs: Self) -> Bool {
    return !(lhs < rhs)
  }
}

/// A type that can produce an integer hash value.
public protocol Hashable: Equatable {
  func hash(into hasher: inout Int)
}

// MARK: - Bool

@frozen
public struct Bool: Equatable, Comparable, Hashable, BitwiseCopyable, Sendable {
  @usableFromInline
  internal var _value: Builtin.Int1

  @_transparent
  public init() {
    let zero = Builtin.zeroInitializer() as Builtin.Int1
    self._value = zero
  }

  @_transparent
  @usableFromInline
  internal init(_ v: Builtin.Int1) {
    self._value = v
  }

  @_transparent
  public static func == (lhs: Bool, rhs: Bool) -> Bool {
    return Bool(Builtin.cmp_eq_Int1(lhs._value, rhs._value))
  }

  @_transparent
  public static func < (lhs: Bool, rhs: Bool) -> Bool {
    // false < true
    return Bool(Builtin.cmp_slt_Int1(lhs._value, rhs._value))
  }

  @inlinable
  public func hash(into hasher: inout Int) {
    let byte: Int8 = self ? 1 : 0
    hasher = hasher &+ Int(byte)
  }

  @_transparent
  public static prefix func ! (a: Bool) -> Bool {
    let trueVal = Builtin.zeroInitializer() as Builtin.Int1
    return Bool(Builtin.xor_Int1(a._value, Builtin.xor_Int1(trueVal, trueVal) /* workaround for 1-bit xor */))
  }

  @_transparent
  public static func && (lhs: Bool, rhs: @autoclosure () -> Bool) -> Bool {
    if lhs { return rhs() }
    return false
  }

  @_transparent
  public static func || (lhs: Bool, rhs: @autoclosure () -> Bool) -> Bool {
    if lhs { return true }
    return rhs()
  }
}

extension Bool: ExpressibleByBooleanLiteral {
  @_transparent
  public init(booleanLiteral value: Bool) {
    self = value
  }
}

/// Protocol for Boolean literal conversion.
public protocol ExpressibleByBooleanLiteral {
  init(booleanLiteral value: Bool)
}

// MARK: - Integer Literal Protocol

public protocol ExpressibleByIntegerLiteral {
  associatedtype IntegerLiteralType: _ExpressibleByBuiltinIntegerLiteral
  init(integerLiteral value: IntegerLiteralType)
}

public protocol _ExpressibleByBuiltinIntegerLiteral {
  init(_builtinIntegerLiteral value: Builtin.IntLiteral)
}

// MARK: - Float Literal Protocol

public protocol ExpressibleByFloatLiteral {
  associatedtype FloatLiteralType: _ExpressibleByBuiltinFloatLiteral
  init(floatLiteral value: FloatLiteralType)
}

public protocol _ExpressibleByBuiltinFloatLiteral {
  init(_builtinFloatLiteral value: Builtin.FPIEEE64)
}

// MARK: - Int8

@frozen
public struct Int8: Equatable, Comparable, Hashable, BitwiseCopyable, Sendable,
                    ExpressibleByIntegerLiteral, _ExpressibleByBuiltinIntegerLiteral {
  @usableFromInline
  internal var _value: Builtin.Int8

  @_transparent
  public init() {
    self._value = Builtin.zeroInitializer()
  }

  @_transparent
  @usableFromInline
  internal init(_ v: Builtin.Int8) {
    self._value = v
  }

  @_transparent
  public init(_builtinIntegerLiteral value: Builtin.IntLiteral) {
    self._value = Builtin.s_to_s_checked_trunc_IntLiteral_Int8(value).0
  }

  @_transparent
  public init(integerLiteral value: Int8) {
    self = value
  }

  @_transparent
  public static func == (lhs: Int8, rhs: Int8) -> Bool {
    return Bool(Builtin.cmp_eq_Int8(lhs._value, rhs._value))
  }

  @_transparent
  public static func < (lhs: Int8, rhs: Int8) -> Bool {
    return Bool(Builtin.cmp_slt_Int8(lhs._value, rhs._value))
  }

  @inlinable
  public func hash(into hasher: inout Int) {
    hasher = hasher &+ Int(self)
  }

  @_transparent
  public static func + (lhs: Int8, rhs: Int8) -> Int8 {
    let (result, overflow) = Builtin.sadd_with_overflow_Int8(lhs._value, rhs._value)
    Builtin.condfail_message(overflow, StaticString("arithmetic overflow").unsafeRawPointer)
    return Int8(result)
  }

  @_transparent
  public static func - (lhs: Int8, rhs: Int8) -> Int8 {
    let (result, overflow) = Builtin.ssub_with_overflow_Int8(lhs._value, rhs._value)
    Builtin.condfail_message(overflow, StaticString("arithmetic overflow").unsafeRawPointer)
    return Int8(result)
  }

  @_transparent
  public static func &+ (lhs: Int8, rhs: Int8) -> Int8 {
    return Int8(Builtin.add_Int8(lhs._value, rhs._value))
  }

  @_transparent
  public static func &- (lhs: Int8, rhs: Int8) -> Int8 {
    return Int8(Builtin.sub_Int8(lhs._value, rhs._value))
  }

  @_transparent
  public static func &* (lhs: Int8, rhs: Int8) -> Int8 {
    return Int8(Builtin.mul_Int8(lhs._value, rhs._value))
  }
}

// MARK: - Int16

@frozen
public struct Int16: Equatable, Comparable, Hashable, BitwiseCopyable, Sendable,
                     ExpressibleByIntegerLiteral, _ExpressibleByBuiltinIntegerLiteral {
  @usableFromInline
  internal var _value: Builtin.Int16

  @_transparent
  public init() {
    self._value = Builtin.zeroInitializer()
  }

  @_transparent
  @usableFromInline
  internal init(_ v: Builtin.Int16) {
    self._value = v
  }

  @_transparent
  public init(_builtinIntegerLiteral value: Builtin.IntLiteral) {
    self._value = Builtin.s_to_s_checked_trunc_IntLiteral_Int16(value).0
  }

  @_transparent
  public init(integerLiteral value: Int16) {
    self = value
  }

  @_transparent
  public static func == (lhs: Int16, rhs: Int16) -> Bool {
    return Bool(Builtin.cmp_eq_Int16(lhs._value, rhs._value))
  }

  @_transparent
  public static func < (lhs: Int16, rhs: Int16) -> Bool {
    return Bool(Builtin.cmp_slt_Int16(lhs._value, rhs._value))
  }

  @inlinable
  public func hash(into hasher: inout Int) {
    hasher = hasher &+ Int(self)
  }

  @_transparent
  public static func + (lhs: Int16, rhs: Int16) -> Int16 {
    let (result, overflow) = Builtin.sadd_with_overflow_Int16(lhs._value, rhs._value)
    Builtin.condfail_message(overflow, StaticString("arithmetic overflow").unsafeRawPointer)
    return Int16(result)
  }

  @_transparent
  public static func - (lhs: Int16, rhs: Int16) -> Int16 {
    let (result, overflow) = Builtin.ssub_with_overflow_Int16(lhs._value, rhs._value)
    Builtin.condfail_message(overflow, StaticString("arithmetic overflow").unsafeRawPointer)
    return Int16(result)
  }

  @_transparent
  public static func &+ (lhs: Int16, rhs: Int16) -> Int16 {
    return Int16(Builtin.add_Int16(lhs._value, rhs._value))
  }

  @_transparent
  public static func &- (lhs: Int16, rhs: Int16) -> Int16 {
    return Int16(Builtin.sub_Int16(lhs._value, rhs._value))
  }

  @_transparent
  public static func &* (lhs: Int16, rhs: Int16) -> Int16 {
    return Int16(Builtin.mul_Int16(lhs._value, rhs._value))
  }
}

// MARK: - Int32

@frozen
public struct Int32: Equatable, Comparable, Hashable, BitwiseCopyable, Sendable,
                     ExpressibleByIntegerLiteral, _ExpressibleByBuiltinIntegerLiteral {
  @usableFromInline
  internal var _value: Builtin.Int32

  @_transparent
  public init() {
    self._value = Builtin.zeroInitializer()
  }

  @_transparent
  @usableFromInline
  internal init(_ v: Builtin.Int32) {
    self._value = v
  }

  @_transparent
  public init(_builtinIntegerLiteral value: Builtin.IntLiteral) {
    self._value = Builtin.s_to_s_checked_trunc_IntLiteral_Int32(value).0
  }

  @_transparent
  public init(integerLiteral value: Int32) {
    self = value
  }

  @_transparent
  public static func == (lhs: Int32, rhs: Int32) -> Bool {
    return Bool(Builtin.cmp_eq_Int32(lhs._value, rhs._value))
  }

  @_transparent
  public static func < (lhs: Int32, rhs: Int32) -> Bool {
    return Bool(Builtin.cmp_slt_Int32(lhs._value, rhs._value))
  }

  @inlinable
  public func hash(into hasher: inout Int) {
    hasher = hasher &+ Int(self)
  }

  @_transparent
  public static func + (lhs: Int32, rhs: Int32) -> Int32 {
    let (result, overflow) = Builtin.sadd_with_overflow_Int32(lhs._value, rhs._value)
    Builtin.condfail_message(overflow, StaticString("arithmetic overflow").unsafeRawPointer)
    return Int32(result)
  }

  @_transparent
  public static func - (lhs: Int32, rhs: Int32) -> Int32 {
    let (result, overflow) = Builtin.ssub_with_overflow_Int32(lhs._value, rhs._value)
    Builtin.condfail_message(overflow, StaticString("arithmetic overflow").unsafeRawPointer)
    return Int32(result)
  }

  @_transparent
  public static func * (lhs: Int32, rhs: Int32) -> Int32 {
    let (result, overflow) = Builtin.smul_with_overflow_Int32(lhs._value, rhs._value)
    Builtin.condfail_message(overflow, StaticString("arithmetic overflow").unsafeRawPointer)
    return Int32(result)
  }

  @_transparent
  public static func / (lhs: Int32, rhs: Int32) -> Int32 {
    return Int32(Builtin.sdiv_Int32(lhs._value, rhs._value))
  }

  @_transparent
  public static func % (lhs: Int32, rhs: Int32) -> Int32 {
    return Int32(Builtin.srem_Int32(lhs._value, rhs._value))
  }

  @_transparent
  public static func &+ (lhs: Int32, rhs: Int32) -> Int32 {
    return Int32(Builtin.add_Int32(lhs._value, rhs._value))
  }

  @_transparent
  public static func &- (lhs: Int32, rhs: Int32) -> Int32 {
    return Int32(Builtin.sub_Int32(lhs._value, rhs._value))
  }

  @_transparent
  public static func &* (lhs: Int32, rhs: Int32) -> Int32 {
    return Int32(Builtin.mul_Int32(lhs._value, rhs._value))
  }

  @_transparent
  public static func & (lhs: Int32, rhs: Int32) -> Int32 {
    return Int32(Builtin.and_Int32(lhs._value, rhs._value))
  }

  @_transparent
  public static func | (lhs: Int32, rhs: Int32) -> Int32 {
    return Int32(Builtin.or_Int32(lhs._value, rhs._value))
  }

  @_transparent
  public static func ^ (lhs: Int32, rhs: Int32) -> Int32 {
    return Int32(Builtin.xor_Int32(lhs._value, rhs._value))
  }

  @_transparent
  public static func << (lhs: Int32, rhs: Int32) -> Int32 {
    return Int32(Builtin.shl_Int32(lhs._value, rhs._value))
  }

  @_transparent
  public static func >> (lhs: Int32, rhs: Int32) -> Int32 {
    return Int32(Builtin.ashr_Int32(lhs._value, rhs._value))
  }
}

// MARK: - Int64

@frozen
public struct Int64: Equatable, Comparable, Hashable, BitwiseCopyable, Sendable,
                     ExpressibleByIntegerLiteral, _ExpressibleByBuiltinIntegerLiteral {
  @usableFromInline
  internal var _value: Builtin.Int64

  @_transparent
  public init() {
    self._value = Builtin.zeroInitializer()
  }

  @_transparent
  @usableFromInline
  internal init(_ v: Builtin.Int64) {
    self._value = v
  }

  @_transparent
  public init(_builtinIntegerLiteral value: Builtin.IntLiteral) {
    self._value = Builtin.s_to_s_checked_trunc_IntLiteral_Int64(value).0
  }

  @_transparent
  public init(integerLiteral value: Int64) {
    self = value
  }

  @_transparent
  public init(_ value: Int32) {
    self._value = Builtin.sextOrBitCast_Int32_Int64(value._value)
  }

  @_transparent
  public init(_ value: Int16) {
    self._value = Builtin.sextOrBitCast_Int16_Int64(value._value)
  }

  @_transparent
  public init(_ value: Int8) {
    self._value = Builtin.sextOrBitCast_Int8_Int64(value._value)
  }

  @_transparent
  public static func == (lhs: Int64, rhs: Int64) -> Bool {
    return Bool(Builtin.cmp_eq_Int64(lhs._value, rhs._value))
  }

  @_transparent
  public static func < (lhs: Int64, rhs: Int64) -> Bool {
    return Bool(Builtin.cmp_slt_Int64(lhs._value, rhs._value))
  }

  @inlinable
  public func hash(into hasher: inout Int) {
    hasher = hasher &+ Int(_value: self._value)
  }

  @_transparent
  public static func + (lhs: Int64, rhs: Int64) -> Int64 {
    let (result, overflow) = Builtin.sadd_with_overflow_Int64(lhs._value, rhs._value)
    Builtin.condfail_message(overflow, StaticString("arithmetic overflow").unsafeRawPointer)
    return Int64(result)
  }

  @_transparent
  public static func - (lhs: Int64, rhs: Int64) -> Int64 {
    let (result, overflow) = Builtin.ssub_with_overflow_Int64(lhs._value, rhs._value)
    Builtin.condfail_message(overflow, StaticString("arithmetic overflow").unsafeRawPointer)
    return Int64(result)
  }

  @_transparent
  public static func * (lhs: Int64, rhs: Int64) -> Int64 {
    let (result, overflow) = Builtin.smul_with_overflow_Int64(lhs._value, rhs._value)
    Builtin.condfail_message(overflow, StaticString("arithmetic overflow").unsafeRawPointer)
    return Int64(result)
  }

  @_transparent
  public static func / (lhs: Int64, rhs: Int64) -> Int64 {
    return Int64(Builtin.sdiv_Int64(lhs._value, rhs._value))
  }

  @_transparent
  public static func % (lhs: Int64, rhs: Int64) -> Int64 {
    return Int64(Builtin.srem_Int64(lhs._value, rhs._value))
  }

  @_transparent
  public static func &+ (lhs: Int64, rhs: Int64) -> Int64 {
    return Int64(Builtin.add_Int64(lhs._value, rhs._value))
  }

  @_transparent
  public static func &- (lhs: Int64, rhs: Int64) -> Int64 {
    return Int64(Builtin.sub_Int64(lhs._value, rhs._value))
  }

  @_transparent
  public static func &* (lhs: Int64, rhs: Int64) -> Int64 {
    return Int64(Builtin.mul_Int64(lhs._value, rhs._value))
  }

  @_transparent
  public static func & (lhs: Int64, rhs: Int64) -> Int64 {
    return Int64(Builtin.and_Int64(lhs._value, rhs._value))
  }

  @_transparent
  public static func | (lhs: Int64, rhs: Int64) -> Int64 {
    return Int64(Builtin.or_Int64(lhs._value, rhs._value))
  }

  @_transparent
  public static func ^ (lhs: Int64, rhs: Int64) -> Int64 {
    return Int64(Builtin.xor_Int64(lhs._value, rhs._value))
  }

  @_transparent
  public static func << (lhs: Int64, rhs: Int64) -> Int64 {
    return Int64(Builtin.shl_Int64(lhs._value, rhs._value))
  }

  @_transparent
  public static func >> (lhs: Int64, rhs: Int64) -> Int64 {
    return Int64(Builtin.ashr_Int64(lhs._value, rhs._value))
  }
}

// MARK: - Int (platform word-size, 64-bit)

@frozen
public struct Int: Equatable, Comparable, Hashable, BitwiseCopyable, Sendable,
                   ExpressibleByIntegerLiteral, _ExpressibleByBuiltinIntegerLiteral {
  @usableFromInline
  internal var _value: Builtin.Int64

  @_transparent
  public init() {
    self._value = Builtin.zeroInitializer()
  }

  @_transparent
  @usableFromInline
  internal init(_value v: Builtin.Int64) {
    self._value = v
  }

  @_transparent
  public init(_builtinIntegerLiteral value: Builtin.IntLiteral) {
    self._value = Builtin.s_to_s_checked_trunc_IntLiteral_Int64(value).0
  }

  @_transparent
  public init(integerLiteral value: Int) {
    self = value
  }

  @_transparent
  public init(_ value: Int8) {
    self._value = Builtin.sextOrBitCast_Int8_Int64(value._value)
  }

  @_transparent
  public init(_ value: Int16) {
    self._value = Builtin.sextOrBitCast_Int16_Int64(value._value)
  }

  @_transparent
  public init(_ value: Int32) {
    self._value = Builtin.sextOrBitCast_Int32_Int64(value._value)
  }

  @_transparent
  public init(_ value: Int64) {
    self._value = value._value
  }

  @_transparent
  public init(_ value: UInt) {
    self._value = value._value
  }

  @_transparent
  public static func == (lhs: Int, rhs: Int) -> Bool {
    return Bool(Builtin.cmp_eq_Int64(lhs._value, rhs._value))
  }

  @_transparent
  public static func < (lhs: Int, rhs: Int) -> Bool {
    return Bool(Builtin.cmp_slt_Int64(lhs._value, rhs._value))
  }

  @inlinable
  public func hash(into hasher: inout Int) {
    hasher = hasher &+ self
  }

  @_transparent
  public static func + (lhs: Int, rhs: Int) -> Int {
    let (result, overflow) = Builtin.sadd_with_overflow_Int64(lhs._value, rhs._value)
    Builtin.condfail_message(overflow, StaticString("arithmetic overflow").unsafeRawPointer)
    return Int(_value: result)
  }

  @_transparent
  public static func - (lhs: Int, rhs: Int) -> Int {
    let (result, overflow) = Builtin.ssub_with_overflow_Int64(lhs._value, rhs._value)
    Builtin.condfail_message(overflow, StaticString("arithmetic overflow").unsafeRawPointer)
    return Int(_value: result)
  }

  @_transparent
  public static func * (lhs: Int, rhs: Int) -> Int {
    let (result, overflow) = Builtin.smul_with_overflow_Int64(lhs._value, rhs._value)
    Builtin.condfail_message(overflow, StaticString("arithmetic overflow").unsafeRawPointer)
    return Int(_value: result)
  }

  @_transparent
  public static func / (lhs: Int, rhs: Int) -> Int {
    return Int(_value: Builtin.sdiv_Int64(lhs._value, rhs._value))
  }

  @_transparent
  public static func % (lhs: Int, rhs: Int) -> Int {
    return Int(_value: Builtin.srem_Int64(lhs._value, rhs._value))
  }

  @_transparent
  public static func &+ (lhs: Int, rhs: Int) -> Int {
    return Int(_value: Builtin.add_Int64(lhs._value, rhs._value))
  }

  @_transparent
  public static func &- (lhs: Int, rhs: Int) -> Int {
    return Int(_value: Builtin.sub_Int64(lhs._value, rhs._value))
  }

  @_transparent
  public static func &* (lhs: Int, rhs: Int) -> Int {
    return Int(_value: Builtin.mul_Int64(lhs._value, rhs._value))
  }

  @_transparent
  public static func & (lhs: Int, rhs: Int) -> Int {
    return Int(_value: Builtin.and_Int64(lhs._value, rhs._value))
  }

  @_transparent
  public static func | (lhs: Int, rhs: Int) -> Int {
    return Int(_value: Builtin.or_Int64(lhs._value, rhs._value))
  }

  @_transparent
  public static func ^ (lhs: Int, rhs: Int) -> Int {
    return Int(_value: Builtin.xor_Int64(lhs._value, rhs._value))
  }

  @_transparent
  public static func << (lhs: Int, rhs: Int) -> Int {
    return Int(_value: Builtin.shl_Int64(lhs._value, rhs._value))
  }

  @_transparent
  public static func >> (lhs: Int, rhs: Int) -> Int {
    return Int(_value: Builtin.ashr_Int64(lhs._value, rhs._value))
  }
}

// MARK: - UInt8

@frozen
public struct UInt8: Equatable, Comparable, Hashable, BitwiseCopyable, Sendable,
                     ExpressibleByIntegerLiteral, _ExpressibleByBuiltinIntegerLiteral {
  @usableFromInline
  internal var _value: Builtin.Int8

  @_transparent
  public init() { self._value = Builtin.zeroInitializer() }

  @_transparent
  @usableFromInline
  internal init(_ v: Builtin.Int8) { self._value = v }

  @_transparent
  public init(_builtinIntegerLiteral value: Builtin.IntLiteral) {
    self._value = Builtin.s_to_u_checked_trunc_IntLiteral_Int8(value).0
  }

  @_transparent
  public init(integerLiteral value: UInt8) { self = value }

  @_transparent
  public static func == (lhs: UInt8, rhs: UInt8) -> Bool {
    return Bool(Builtin.cmp_eq_Int8(lhs._value, rhs._value))
  }

  @_transparent
  public static func < (lhs: UInt8, rhs: UInt8) -> Bool {
    return Bool(Builtin.cmp_ult_Int8(lhs._value, rhs._value))
  }

  @inlinable
  public func hash(into hasher: inout Int) {
    hasher = hasher &+ Int(Int8(self._value))
  }

  @_transparent
  public static func &+ (lhs: UInt8, rhs: UInt8) -> UInt8 {
    return UInt8(Builtin.add_Int8(lhs._value, rhs._value))
  }

  @_transparent
  public static func &- (lhs: UInt8, rhs: UInt8) -> UInt8 {
    return UInt8(Builtin.sub_Int8(lhs._value, rhs._value))
  }

  @_transparent
  public static func &* (lhs: UInt8, rhs: UInt8) -> UInt8 {
    return UInt8(Builtin.mul_Int8(lhs._value, rhs._value))
  }
}

// MARK: - UInt16

@frozen
public struct UInt16: Equatable, Comparable, Hashable, BitwiseCopyable, Sendable,
                      ExpressibleByIntegerLiteral, _ExpressibleByBuiltinIntegerLiteral {
  @usableFromInline
  internal var _value: Builtin.Int16

  @_transparent
  public init() { self._value = Builtin.zeroInitializer() }

  @_transparent
  @usableFromInline
  internal init(_ v: Builtin.Int16) { self._value = v }

  @_transparent
  public init(_builtinIntegerLiteral value: Builtin.IntLiteral) {
    self._value = Builtin.s_to_u_checked_trunc_IntLiteral_Int16(value).0
  }

  @_transparent
  public init(integerLiteral value: UInt16) { self = value }

  @_transparent
  public static func == (lhs: UInt16, rhs: UInt16) -> Bool {
    return Bool(Builtin.cmp_eq_Int16(lhs._value, rhs._value))
  }

  @_transparent
  public static func < (lhs: UInt16, rhs: UInt16) -> Bool {
    return Bool(Builtin.cmp_ult_Int16(lhs._value, rhs._value))
  }

  @inlinable
  public func hash(into hasher: inout Int) {
    hasher = hasher &+ Int(Int16(self._value))
  }

  @_transparent
  public static func &+ (lhs: UInt16, rhs: UInt16) -> UInt16 {
    return UInt16(Builtin.add_Int16(lhs._value, rhs._value))
  }

  @_transparent
  public static func &- (lhs: UInt16, rhs: UInt16) -> UInt16 {
    return UInt16(Builtin.sub_Int16(lhs._value, rhs._value))
  }

  @_transparent
  public static func &* (lhs: UInt16, rhs: UInt16) -> UInt16 {
    return UInt16(Builtin.mul_Int16(lhs._value, rhs._value))
  }
}

// MARK: - UInt32

@frozen
public struct UInt32: Equatable, Comparable, Hashable, BitwiseCopyable, Sendable,
                      ExpressibleByIntegerLiteral, _ExpressibleByBuiltinIntegerLiteral {
  @usableFromInline
  internal var _value: Builtin.Int32

  @_transparent
  public init() { self._value = Builtin.zeroInitializer() }

  @_transparent
  @usableFromInline
  internal init(_ v: Builtin.Int32) { self._value = v }

  @_transparent
  public init(_builtinIntegerLiteral value: Builtin.IntLiteral) {
    self._value = Builtin.s_to_u_checked_trunc_IntLiteral_Int32(value).0
  }

  @_transparent
  public init(integerLiteral value: UInt32) { self = value }

  @_transparent
  public static func == (lhs: UInt32, rhs: UInt32) -> Bool {
    return Bool(Builtin.cmp_eq_Int32(lhs._value, rhs._value))
  }

  @_transparent
  public static func < (lhs: UInt32, rhs: UInt32) -> Bool {
    return Bool(Builtin.cmp_ult_Int32(lhs._value, rhs._value))
  }

  @inlinable
  public func hash(into hasher: inout Int) {
    hasher = hasher &+ Int(Int32(self._value))
  }

  @_transparent
  public static func &+ (lhs: UInt32, rhs: UInt32) -> UInt32 {
    return UInt32(Builtin.add_Int32(lhs._value, rhs._value))
  }

  @_transparent
  public static func &- (lhs: UInt32, rhs: UInt32) -> UInt32 {
    return UInt32(Builtin.sub_Int32(lhs._value, rhs._value))
  }

  @_transparent
  public static func &* (lhs: UInt32, rhs: UInt32) -> UInt32 {
    return UInt32(Builtin.mul_Int32(lhs._value, rhs._value))
  }
}

// MARK: - UInt64

@frozen
public struct UInt64: Equatable, Comparable, Hashable, BitwiseCopyable, Sendable,
                      ExpressibleByIntegerLiteral, _ExpressibleByBuiltinIntegerLiteral {
  @usableFromInline
  internal var _value: Builtin.Int64

  @_transparent
  public init() { self._value = Builtin.zeroInitializer() }

  @_transparent
  @usableFromInline
  internal init(_ v: Builtin.Int64) { self._value = v }

  @_transparent
  public init(_builtinIntegerLiteral value: Builtin.IntLiteral) {
    self._value = Builtin.s_to_u_checked_trunc_IntLiteral_Int64(value).0
  }

  @_transparent
  public init(integerLiteral value: UInt64) { self = value }

  @_transparent
  public static func == (lhs: UInt64, rhs: UInt64) -> Bool {
    return Bool(Builtin.cmp_eq_Int64(lhs._value, rhs._value))
  }

  @_transparent
  public static func < (lhs: UInt64, rhs: UInt64) -> Bool {
    return Bool(Builtin.cmp_ult_Int64(lhs._value, rhs._value))
  }

  @inlinable
  public func hash(into hasher: inout Int) {
    hasher = hasher &+ Int(_value: self._value)
  }

  @_transparent
  public static func &+ (lhs: UInt64, rhs: UInt64) -> UInt64 {
    return UInt64(Builtin.add_Int64(lhs._value, rhs._value))
  }

  @_transparent
  public static func &- (lhs: UInt64, rhs: UInt64) -> UInt64 {
    return UInt64(Builtin.sub_Int64(lhs._value, rhs._value))
  }

  @_transparent
  public static func &* (lhs: UInt64, rhs: UInt64) -> UInt64 {
    return UInt64(Builtin.mul_Int64(lhs._value, rhs._value))
  }
}

// MARK: - UInt (platform word-size, 64-bit)

@frozen
public struct UInt: Equatable, Comparable, Hashable, BitwiseCopyable, Sendable,
                    ExpressibleByIntegerLiteral, _ExpressibleByBuiltinIntegerLiteral {
  @usableFromInline
  internal var _value: Builtin.Int64

  @_transparent
  public init() { self._value = Builtin.zeroInitializer() }

  @_transparent
  @usableFromInline
  internal init(_value v: Builtin.Int64) { self._value = v }

  @_transparent
  public init(_builtinIntegerLiteral value: Builtin.IntLiteral) {
    self._value = Builtin.s_to_u_checked_trunc_IntLiteral_Int64(value).0
  }

  @_transparent
  public init(integerLiteral value: UInt) { self = value }

  @_transparent
  public init(_ value: Int) { self._value = value._value }

  @_transparent
  public static func == (lhs: UInt, rhs: UInt) -> Bool {
    return Bool(Builtin.cmp_eq_Int64(lhs._value, rhs._value))
  }

  @_transparent
  public static func < (lhs: UInt, rhs: UInt) -> Bool {
    return Bool(Builtin.cmp_ult_Int64(lhs._value, rhs._value))
  }

  @inlinable
  public func hash(into hasher: inout Int) {
    hasher = hasher &+ Int(_value: self._value)
  }

  @_transparent
  public static func &+ (lhs: UInt, rhs: UInt) -> UInt {
    return UInt(_value: Builtin.add_Int64(lhs._value, rhs._value))
  }

  @_transparent
  public static func &- (lhs: UInt, rhs: UInt) -> UInt {
    return UInt(_value: Builtin.sub_Int64(lhs._value, rhs._value))
  }

  @_transparent
  public static func &* (lhs: UInt, rhs: UInt) -> UInt {
    return UInt(_value: Builtin.mul_Int64(lhs._value, rhs._value))
  }
}

// MARK: - Float

@frozen
public struct Float: Equatable, Comparable, BitwiseCopyable, Sendable,
                     ExpressibleByFloatLiteral, ExpressibleByIntegerLiteral,
                     _ExpressibleByBuiltinFloatLiteral, _ExpressibleByBuiltinIntegerLiteral {
  @usableFromInline
  internal var _value: Builtin.FPIEEE32

  @_transparent
  public init() {
    self._value = Builtin.zeroInitializer()
  }

  @_transparent
  @usableFromInline
  internal init(_ v: Builtin.FPIEEE32) {
    self._value = v
  }

  @_transparent
  public init(_builtinFloatLiteral value: Builtin.FPIEEE64) {
    self._value = Builtin.fptrunc_FPIEEE64_FPIEEE32(value)
  }

  @_transparent
  public init(floatLiteral value: Float) {
    self = value
  }

  @_transparent
  public init(_builtinIntegerLiteral value: Builtin.IntLiteral) {
    self._value = Builtin.itofp_with_overflow_IntLiteral_FPIEEE32(value)
  }

  @_transparent
  public init(integerLiteral value: Int64) {
    self._value = Builtin.sitofp_Int64_FPIEEE32(value._value)
  }

  @_transparent
  public static func == (lhs: Float, rhs: Float) -> Bool {
    return Bool(Builtin.fcmp_oeq_FPIEEE32(lhs._value, rhs._value))
  }

  @_transparent
  public static func < (lhs: Float, rhs: Float) -> Bool {
    return Bool(Builtin.fcmp_olt_FPIEEE32(lhs._value, rhs._value))
  }

  @_transparent
  public static func + (lhs: Float, rhs: Float) -> Float {
    return Float(Builtin.fadd_FPIEEE32(lhs._value, rhs._value))
  }

  @_transparent
  public static func - (lhs: Float, rhs: Float) -> Float {
    return Float(Builtin.fsub_FPIEEE32(lhs._value, rhs._value))
  }

  @_transparent
  public static func * (lhs: Float, rhs: Float) -> Float {
    return Float(Builtin.fmul_FPIEEE32(lhs._value, rhs._value))
  }

  @_transparent
  public static func / (lhs: Float, rhs: Float) -> Float {
    return Float(Builtin.fdiv_FPIEEE32(lhs._value, rhs._value))
  }

  /// Convert to Double (lossless widening).
  @_transparent
  public var asDouble: Double {
    return Double(Builtin.fpext_FPIEEE32_FPIEEE64(self._value))
  }
}

// MARK: - Double

@frozen
public struct Double: Equatable, Comparable, BitwiseCopyable, Sendable,
                      ExpressibleByFloatLiteral, ExpressibleByIntegerLiteral,
                      _ExpressibleByBuiltinFloatLiteral, _ExpressibleByBuiltinIntegerLiteral {
  @usableFromInline
  internal var _value: Builtin.FPIEEE64

  @_transparent
  public init() {
    self._value = Builtin.zeroInitializer()
  }

  @_transparent
  @usableFromInline
  internal init(_ v: Builtin.FPIEEE64) {
    self._value = v
  }

  @_transparent
  public init(_builtinFloatLiteral value: Builtin.FPIEEE64) {
    self._value = value
  }

  @_transparent
  public init(floatLiteral value: Double) {
    self = value
  }

  @_transparent
  public init(_builtinIntegerLiteral value: Builtin.IntLiteral) {
    self._value = Builtin.itofp_with_overflow_IntLiteral_FPIEEE64(value)
  }

  @_transparent
  public init(integerLiteral value: Int64) {
    self._value = Builtin.sitofp_Int64_FPIEEE64(value._value)
  }

  @_transparent
  public static func == (lhs: Double, rhs: Double) -> Bool {
    return Bool(Builtin.fcmp_oeq_FPIEEE64(lhs._value, rhs._value))
  }

  @_transparent
  public static func < (lhs: Double, rhs: Double) -> Bool {
    return Bool(Builtin.fcmp_olt_FPIEEE64(lhs._value, rhs._value))
  }

  @_transparent
  public static func + (lhs: Double, rhs: Double) -> Double {
    return Double(Builtin.fadd_FPIEEE64(lhs._value, rhs._value))
  }

  @_transparent
  public static func - (lhs: Double, rhs: Double) -> Double {
    return Double(Builtin.fsub_FPIEEE64(lhs._value, rhs._value))
  }

  @_transparent
  public static func * (lhs: Double, rhs: Double) -> Double {
    return Double(Builtin.fmul_FPIEEE64(lhs._value, rhs._value))
  }

  @_transparent
  public static func / (lhs: Double, rhs: Double) -> Double {
    return Double(Builtin.fdiv_FPIEEE64(lhs._value, rhs._value))
  }

  /// Truncate to Float (potentially lossy).
  @_transparent
  public var asFloat: Float {
    return Float(Builtin.fptrunc_FPIEEE64_FPIEEE32(self._value))
  }

  /// Convert to Int64 (truncates toward zero).
  @_transparent
  public var asInt64: Int64 {
    return Int64(Builtin.fptosi_FPIEEE64_Int64(self._value))
  }
}

// MARK: - Optional

@frozen
public enum Optional<Wrapped>: ExpressibleByNilLiteral {
  case none
  case some(Wrapped)

  @_transparent
  public init(_ some: Wrapped) {
    self = .some(some)
  }

  @_transparent
  public init(nilLiteral: ()) {
    self = .none
  }

  /// Force-unwrap the optional value. Traps if nil.
  @_transparent
  public var unsafelyUnwrapped: Wrapped {
    get {
      if case .some(let value) = self {
        return value
      }
      Builtin.condfail_message(
        true._value,
        StaticString("force-unwrap of nil Optional").unsafeRawPointer
      )
      Builtin.unreachable()
    }
  }
}

/// Protocol for nil literal conversion.
public protocol ExpressibleByNilLiteral {
  init(nilLiteral: ())
}

// MARK: - Never

/// The uninhabited type. A function returning Never cannot return normally.
@frozen
public enum Never {}

// MARK: - StaticString (minimal, for diagnostics)

@frozen
public struct StaticString: BitwiseCopyable, Sendable {
  @usableFromInline
  internal var _startPtrOrData: Builtin.Word
  @usableFromInline
  internal var _utf8CodeUnitCount: Builtin.Word
  @usableFromInline
  internal var _flags: Builtin.Int8

  @_transparent
  public var unsafeRawPointer: Builtin.RawPointer {
    return Builtin.inttoptr_Word(_startPtrOrData)
  }
}

extension StaticString: _ExpressibleByBuiltinStringLiteral {
  @_transparent
  public init(_builtinStringLiteral start: Builtin.RawPointer,
              utf8CodeUnitCount: Builtin.Word,
              isASCII: Builtin.Int1) {
    self._startPtrOrData = Builtin.ptrtoint_Word(start)
    self._utf8CodeUnitCount = utf8CodeUnitCount
    self._flags = Builtin.zeroInitializer()
  }
}

extension StaticString: ExpressibleByStringLiteral {
  @_transparent
  public init(stringLiteral value: StaticString) {
    self = value
  }
}

public protocol ExpressibleByStringLiteral {
  init(stringLiteral value: StaticString)
}

public protocol _ExpressibleByBuiltinStringLiteral {
  init(_builtinStringLiteral start: Builtin.RawPointer,
       utf8CodeUnitCount: Builtin.Word,
       isASCII: Builtin.Int1)
}

// MARK: - UnsafeRawPointer

@frozen
public struct UnsafeRawPointer: Equatable, Comparable, BitwiseCopyable, Sendable {
  @usableFromInline
  internal var _rawValue: Builtin.RawPointer

  @_transparent
  @usableFromInline
  internal init(_ rawValue: Builtin.RawPointer) {
    self._rawValue = rawValue
  }

  @_transparent
  public static func == (lhs: UnsafeRawPointer, rhs: UnsafeRawPointer) -> Bool {
    return Bool(Builtin.cmp_eq_RawPointer(lhs._rawValue, rhs._rawValue))
  }

  @_transparent
  public static func < (lhs: UnsafeRawPointer, rhs: UnsafeRawPointer) -> Bool {
    return Bool(Builtin.cmp_ult_RawPointer(lhs._rawValue, rhs._rawValue))
  }

  /// Load a value of type T from this pointer.
  @_transparent
  public func load<T>(as type: T.Type) -> T {
    return Builtin.loadRaw(self._rawValue) as T
  }

  /// Return a new pointer offset by `n` bytes.
  @_transparent
  public func advanced(by n: Int) -> UnsafeRawPointer {
    return UnsafeRawPointer(Builtin.gepRaw_Word(self._rawValue, n._value))
  }
}

// MARK: - UnsafeMutableRawPointer

@frozen
public struct UnsafeMutableRawPointer: Equatable, Comparable, BitwiseCopyable, Sendable {
  @usableFromInline
  internal var _rawValue: Builtin.RawPointer

  @_transparent
  @usableFromInline
  internal init(_ rawValue: Builtin.RawPointer) {
    self._rawValue = rawValue
  }

  @_transparent
  public static func == (lhs: UnsafeMutableRawPointer, rhs: UnsafeMutableRawPointer) -> Bool {
    return Bool(Builtin.cmp_eq_RawPointer(lhs._rawValue, rhs._rawValue))
  }

  @_transparent
  public static func < (lhs: UnsafeMutableRawPointer, rhs: UnsafeMutableRawPointer) -> Bool {
    return Bool(Builtin.cmp_ult_RawPointer(lhs._rawValue, rhs._rawValue))
  }

  /// Allocate raw memory. Traps if no allocator is available.
  @inlinable
  public static func allocate(byteCount: Int, alignment: Int) -> UnsafeMutableRawPointer {
    return UnsafeMutableRawPointer(Builtin.allocRaw(byteCount._value, alignment._value))
  }

  /// Deallocate memory previously allocated with allocate().
  @inlinable
  public func deallocate() {
    Builtin.deallocRaw(self._rawValue, (-1)._value, (0)._value)
  }

  /// Load a value of type T from this pointer.
  @_transparent
  public func load<T>(as type: T.Type) -> T {
    return Builtin.loadRaw(self._rawValue) as T
  }

  /// Store a value of type T to this pointer.
  @_transparent
  public func storeBytes<T>(of value: T, as type: T.Type) {
    Builtin.storeRaw(value, self._rawValue)
  }

  /// Return a new pointer offset by `n` bytes.
  @_transparent
  public func advanced(by n: Int) -> UnsafeMutableRawPointer {
    return UnsafeMutableRawPointer(Builtin.gepRaw_Word(self._rawValue, n._value))
  }

  /// Convert to immutable raw pointer.
  @_transparent
  public var immutable: UnsafeRawPointer {
    return UnsafeRawPointer(self._rawValue)
  }
}

// MARK: - UnsafePointer<Pointee>

@frozen
public struct UnsafePointer<Pointee>: Equatable, Comparable, BitwiseCopyable, Sendable {
  @usableFromInline
  internal var _rawValue: Builtin.RawPointer

  @_transparent
  @usableFromInline
  internal init(_ rawValue: Builtin.RawPointer) {
    self._rawValue = rawValue
  }

  /// Access the pointed-to value.
  @_transparent
  public var pointee: Pointee {
    @_transparent get {
      return Builtin.load(self._rawValue) as Pointee
    }
  }

  @_transparent
  public static func == (lhs: UnsafePointer, rhs: UnsafePointer) -> Bool {
    return Bool(Builtin.cmp_eq_RawPointer(lhs._rawValue, rhs._rawValue))
  }

  @_transparent
  public static func < (lhs: UnsafePointer, rhs: UnsafePointer) -> Bool {
    return Bool(Builtin.cmp_ult_RawPointer(lhs._rawValue, rhs._rawValue))
  }

  /// Return a pointer to the element at offset `n` from this pointer.
  @_transparent
  public func advanced(by n: Int) -> UnsafePointer<Pointee> {
    return UnsafePointer(Builtin.gepRaw_Word(
      self._rawValue,
      (n &* Int(_value: Builtin.strideof(Pointee.self)))._value
    ))
  }

  /// Subscript access to elements relative to this pointer.
  @_transparent
  public subscript(i: Int) -> Pointee {
    @_transparent get {
      return self.advanced(by: i).pointee
    }
  }
}

// MARK: - UnsafeMutablePointer<Pointee>

@frozen
public struct UnsafeMutablePointer<Pointee>: Equatable, Comparable, BitwiseCopyable, Sendable {
  @usableFromInline
  internal var _rawValue: Builtin.RawPointer

  @_transparent
  @usableFromInline
  internal init(_ rawValue: Builtin.RawPointer) {
    self._rawValue = rawValue
  }

  /// Access the pointed-to value.
  public var pointee: Pointee {
    @_transparent get {
      return Builtin.load(self._rawValue) as Pointee
    }
    @_transparent nonmutating set {
      Builtin.initialize(newValue, self._rawValue)
    }
  }

  @_transparent
  public static func == (lhs: UnsafeMutablePointer, rhs: UnsafeMutablePointer) -> Bool {
    return Bool(Builtin.cmp_eq_RawPointer(lhs._rawValue, rhs._rawValue))
  }

  @_transparent
  public static func < (lhs: UnsafeMutablePointer, rhs: UnsafeMutablePointer) -> Bool {
    return Bool(Builtin.cmp_ult_RawPointer(lhs._rawValue, rhs._rawValue))
  }

  /// Allocate memory for `capacity` elements of Pointee.
  @inlinable
  public static func allocate(capacity: Int) -> UnsafeMutablePointer<Pointee> {
    let rawPtr = Builtin.allocRaw(
      (capacity &* Int(_value: Builtin.strideof(Pointee.self)))._value,
      Builtin.alignof(Pointee.self)
    )
    return UnsafeMutablePointer(rawPtr)
  }

  /// Deallocate memory previously allocated with allocate().
  @inlinable
  public func deallocate() {
    Builtin.deallocRaw(self._rawValue, (-1)._value, (0)._value)
  }

  /// Initialize the pointee with the given value.
  @_transparent
  public func initialize(to value: Pointee) {
    Builtin.initialize(value, self._rawValue)
  }

  /// Return a pointer to the element at offset `n` from this pointer.
  @_transparent
  public func advanced(by n: Int) -> UnsafeMutablePointer<Pointee> {
    return UnsafeMutablePointer(Builtin.gepRaw_Word(
      self._rawValue,
      (n &* Int(_value: Builtin.strideof(Pointee.self)))._value
    ))
  }

  /// Subscript access to elements relative to this pointer.
  public subscript(i: Int) -> Pointee {
    @_transparent get {
      return self.advanced(by: i).pointee
    }
    @_transparent nonmutating set {
      self.advanced(by: i).pointee = newValue
    }
  }

  /// Convert to an immutable pointer.
  @_transparent
  public var immutable: UnsafePointer<Pointee> {
    return UnsafePointer(self._rawValue)
  }
}

// MARK: - MemoryLayout

/// Provides compile-time information about the memory layout of a type.
/// All properties resolve to constants -- no runtime metadata is used.
@frozen
public enum MemoryLayout<T> {
  /// The contiguous memory footprint of T, in bytes.
  @_transparent
  public static var size: Int {
    return Int(_value: Builtin.sizeof(T.self))
  }

  /// The number of bytes from the start of one instance to the start of the next
  /// in a contiguous array. Always >= size.
  @_transparent
  public static var stride: Int {
    return Int(_value: Builtin.strideof(T.self))
  }

  /// The minimum alignment of T, in bytes. Always a power of two.
  @_transparent
  public static var alignment: Int {
    return Int(_value: Builtin.alignof(T.self))
  }
}

// MARK: - Global Utility Functions

/// Exchange the values of `a` and `b`.
@_transparent
public func swap<T>(_ a: inout T, _ b: inout T) {
  let tmp = a
  a = b
  b = tmp
}

/// Return the lesser of `a` and `b`.
@_transparent
public func min<T: Comparable>(_ a: T, _ b: T) -> T {
  return a < b ? a : b
}

/// Return the greater of `a` and `b`.
@_transparent
public func max<T: Comparable>(_ a: T, _ b: T) -> T {
  return a > b ? a : b
}

/// Unconditionally stop execution. The message is for diagnostic purposes only.
@_transparent
public func fatalError(_ message: StaticString = "fatal error") -> Never {
  Builtin.condfail_message(true._value, message.unsafeRawPointer)
  Builtin.unreachable()
}

/// Check a necessary condition. Traps if the condition is false.
@_transparent
public func precondition(_ condition: @autoclosure () -> Bool,
                          _ message: StaticString = "precondition failure") {
  if !condition() {
    Builtin.condfail_message(true._value, message.unsafeRawPointer)
    Builtin.unreachable()
  }
}

/// Assert a condition in debug builds. In TinySwift, this is always checked.
@_transparent
public func assert(_ condition: @autoclosure () -> Bool,
                    _ message: StaticString = "assertion failure") {
  precondition(condition(), message)
}
