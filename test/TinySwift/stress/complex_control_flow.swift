// RUN: %target-swift-frontend -typecheck -enable-experimental-feature TinySwift -disable-objc-interop -parse-stdlib %s
// REQUIRES: swift_feature_TinySwift

// Stress test: conditional moves across branches.
// Exercises phi-node-like ownership merges in the SIL verifier.
// All values are Builtin-based structs — no ARC symbols should result.

// --- Types ---

struct Sensor {
  var id: Builtin.Int32
  var reading: Builtin.Int64
}

struct SensorPair {
  var primary: Sensor
  var backup: Sensor
}

enum Status {
  case ok
  case degraded
  case failed
}

enum Priority {
  case low
  case medium
  case high
  case critical
}

// --- Helpers ---

func makeSensor(_ id: Builtin.Int32, _ reading: Builtin.Int64) -> Sensor {
  return Sensor(id: id, reading: reading)
}

func zeroed() -> Builtin.Int64 {
  return Builtin.zeroInitializer()
}

func isZero(_ v: Builtin.Int64) -> Builtin.Int1 {
  return Builtin.cmp_eq_Int64(v, Builtin.zeroInitializer())
}

// --- Test 1: simple if/else value merge ---

func selectSensor(_ flag: Builtin.Int1,
                  _ a: Sensor, _ b: Sensor) -> Sensor {
  // Ownership must merge correctly at the join point.
  if flag {
    return a
  } else {
    return b
  }
}

// --- Test 2: nested if/else with intermediate assignments ---

func nestedBranchMerge(_ cond1: Builtin.Int1,
                       _ cond2: Builtin.Int1,
                       _ base: Sensor) -> Sensor {
  var result = base
  if cond1 {
    result = Sensor(id: base.id, reading: Builtin.zeroInitializer())
    if cond2 {
      // Re-assign inside nested branch — tests multi-level phi merge.
      result = Sensor(id: Builtin.zeroInitializer(), reading: base.reading)
    }
  } else {
    result = makeSensor(Builtin.zeroInitializer(), zeroed())
  }
  return result
}

// --- Test 3: switch with all-paths assignment ---

func classifySensor(_ s: Sensor, _ status: Status) -> Sensor {
  var out: Sensor
  switch status {
  case .ok:
    out = s
  case .degraded:
    out = Sensor(id: s.id, reading: Builtin.zeroInitializer())
  case .failed:
    out = makeSensor(Builtin.zeroInitializer(), Builtin.zeroInitializer())
  }
  // Value must be definitely initialized on all paths.
  return out
}

// --- Test 4: switch on two enums with cross-product branches ---

func prioritizedResponse(_ status: Status,
                         _ priority: Priority,
                         _ s: Sensor) -> Builtin.Int64 {
  var reading = s.reading
  switch status {
  case .ok:
    switch priority {
    case .low, .medium:
      reading = s.reading
    case .high, .critical:
      reading = Builtin.zeroInitializer()
    }
  case .degraded:
    reading = Builtin.zeroInitializer()
  case .failed:
    switch priority {
    case .critical:
      reading = s.reading
    default:
      reading = zeroed()
    }
  }
  return reading
}

// --- Test 5: while loop with conditional reassignment ---

func loopWithConditionalMove(_ initial: Sensor,
                             _ iterations: Builtin.Int64) -> Sensor {
  var current = initial
  var i = Builtin.zeroInitializer() as Builtin.Int64
  let one: Builtin.Int64 = Builtin.intLiteral_Int64(1._builtinIntegerLiteral)
  while !Builtin.cmp_eq_Int64(i, iterations) {
    let even = Builtin.cmp_eq_Int64(
      Builtin.and_Int64(i, one),
      Builtin.zeroInitializer()
    )
    if even {
      // Reassign on even iterations — ownership transfer inside loop.
      current = Sensor(id: current.id,
                       reading: Builtin.add_Int64(current.reading, one))
    }
    i = Builtin.add_Int64(i, one)
  }
  return current
}

// --- Test 6: SensorPair with branch-dependent field selection ---

func chooseSensor(_ pair: SensorPair,
                  _ usePrimary: Builtin.Int1) -> Sensor {
  if usePrimary {
    return pair.primary
  } else {
    return pair.backup
  }
}

func conditionalSwap(_ pair: SensorPair,
                     _ doSwap: Builtin.Int1) -> SensorPair {
  if doSwap {
    return SensorPair(primary: pair.backup, backup: pair.primary)
  } else {
    return pair
  }
}

// --- Test 7: complex multi-exit function ---

func multiExit(_ s: Sensor, _ status: Status,
               _ flag: Builtin.Int1) -> Builtin.Int64 {
  if flag {
    return s.reading
  }
  switch status {
  case .ok:
    return s.reading
  case .degraded:
    let adjusted = Builtin.add_Int64(s.reading, s.reading)
    return adjusted
  case .failed:
    return Builtin.zeroInitializer()
  }
}

// --- Test 8: deeply nested branches with struct construction ---

func deepNesting(_ a: Builtin.Int1, _ b: Builtin.Int1,
                 _ c: Builtin.Int1, _ d: Builtin.Int1) -> SensorPair {
  var s: Sensor
  if a {
    if b {
      s = makeSensor(Builtin.zeroInitializer(), zeroed())
    } else {
      s = makeSensor(Builtin.zeroInitializer(), Builtin.zeroInitializer())
    }
  } else {
    if c {
      if d {
        s = Sensor(id: Builtin.zeroInitializer(),
                   reading: Builtin.zeroInitializer())
      } else {
        s = makeSensor(Builtin.zeroInitializer(), zeroed())
      }
    } else {
      s = Sensor(id: Builtin.zeroInitializer(),
                 reading: Builtin.zeroInitializer())
    }
  }
  // 's' must be definitely initialized through all 5 leaf paths.
  return SensorPair(primary: s, backup: s)
}
