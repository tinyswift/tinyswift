// RUN: %target-swift-frontend -typecheck -enable-experimental-feature TinySwift -disable-objc-interop -parse-stdlib %s
// REQUIRES: swift_feature_TinySwift

// Stress test: nested struct destructuring with partial moves.
// Exercises ownership tracking through multi-level field extraction,
// partial struct access, and reassignment after partial reads.
// All types are Builtin-based — no ARC symbols should result.

// --- Inner types ---

struct Coord {
  var x: Builtin.Int64
  var y: Builtin.Int64
}

struct Color {
  var r: Builtin.Int8
  var g: Builtin.Int8
  var b: Builtin.Int8
  var a: Builtin.Int8
}

// --- Mid-level nesting ---

struct Vertex {
  var position: Coord
  var color: Color
}

struct Edge {
  var start: Vertex
  var end: Vertex
}

// --- Top-level nesting (3 levels deep) ---

struct Triangle {
  var a: Vertex
  var b: Vertex
  var c: Vertex
}

struct Mesh {
  var edge0: Edge
  var edge1: Edge
}

// --- Test 1: extract individual nested fields ---

func getPositionX(_ v: Vertex) -> Builtin.Int64 {
  return v.position.x
}

func getColorAlpha(_ v: Vertex) -> Builtin.Int8 {
  return v.color.a
}

func getEdgeStartY(_ e: Edge) -> Builtin.Int64 {
  return e.start.position.y
}

func getTriangleVertexColor(_ t: Triangle) -> Color {
  return t.b.color
}

// --- Test 2: extract at each nesting level independently ---

func extractAllLevels(_ e: Edge) -> (Vertex, Coord, Builtin.Int64) {
  let vertex = e.start
  let coord = vertex.position
  let component = coord.x
  return (vertex, coord, component)
}

// --- Test 3: partial field access then reconstruct ---

func swapCoords(_ v: Vertex) -> Vertex {
  let flipped = Coord(x: v.position.y, y: v.position.x)
  return Vertex(position: flipped, color: v.color)
}

func invertColor(_ v: Vertex) -> Vertex {
  // Read individual color fields and reconstruct.
  let inv = Color(r: v.color.a, g: v.color.b,
                  b: v.color.g, a: v.color.r)
  return Vertex(position: v.position, color: inv)
}

// --- Test 4: cross-field extraction from nested aggregate ---

func mixVertices(_ e: Edge) -> Vertex {
  // Take position from start, color from end.
  return Vertex(position: e.start.position, color: e.end.color)
}

func diagonalCoord(_ t: Triangle) -> Coord {
  // Take x from vertex a, y from vertex c — cross-struct extraction.
  return Coord(x: t.a.position.x, y: t.c.position.y)
}

// --- Test 5: destructure into tuple and rebuild ---

func decomposeVertex(_ v: Vertex) -> (Coord, Color) {
  return (v.position, v.color)
}

func recomposeVertex(_ parts: (Coord, Color)) -> Vertex {
  return Vertex(position: parts.0, color: parts.1)
}

func roundTrip(_ v: Vertex) -> Vertex {
  let parts = decomposeVertex(v)
  return recomposeVertex(parts)
}

// --- Test 6: nested struct with mutable partial updates ---

func updatePositionX(_ v: Vertex, _ newX: Builtin.Int64) -> Vertex {
  var mutable = v
  mutable.position.x = newX
  return mutable
}

func updateColorChannels(_ v: Vertex,
                         _ r: Builtin.Int8,
                         _ g: Builtin.Int8) -> Vertex {
  var mutable = v
  mutable.color.r = r
  mutable.color.g = g
  return mutable
}

func updateEdgeEndColor(_ e: Edge, _ c: Color) -> Edge {
  var mutable = e
  mutable.end.color = c
  return mutable
}

// --- Test 7: deeply nested mutable field write ---

func setMeshEdge0StartX(_ m: Mesh, _ val: Builtin.Int64) -> Mesh {
  var mutable = m
  mutable.edge0.start.position.x = val
  return mutable
}

// --- Test 8: triangle field permutation ---

func rotateTriangle(_ t: Triangle) -> Triangle {
  return Triangle(a: t.b, b: t.c, c: t.a)
}

func mirrorTriangle(_ t: Triangle) -> Triangle {
  return Triangle(a: t.c, b: t.b, c: t.a)
}

// --- Test 9: multiple partial reads of the same struct ---

func sumCoord(_ c: Coord) -> Builtin.Int64 {
  return Builtin.add_Int64(c.x, c.y)
}

func sumAllPositions(_ t: Triangle) -> Builtin.Int64 {
  let sa = sumCoord(t.a.position)
  let sb = sumCoord(t.b.position)
  let sc = sumCoord(t.c.position)
  return Builtin.add_Int64(Builtin.add_Int64(sa, sb), sc)
}

// --- Test 10: construct deeply nested value bottom-up ---

func buildTriangle() -> Triangle {
  let zero: Builtin.Int64 = Builtin.zeroInitializer()
  let zByte: Builtin.Int8 = Builtin.zeroInitializer()
  let coord = Coord(x: zero, y: zero)
  let color = Color(r: zByte, g: zByte, b: zByte, a: zByte)
  let vertex = Vertex(position: coord, color: color)
  return Triangle(a: vertex, b: vertex, c: vertex)
}

func buildMesh() -> Mesh {
  let zero: Builtin.Int64 = Builtin.zeroInitializer()
  let zByte: Builtin.Int8 = Builtin.zeroInitializer()
  let c = Coord(x: zero, y: zero)
  let col = Color(r: zByte, g: zByte, b: zByte, a: zByte)
  let v = Vertex(position: c, color: col)
  let e = Edge(start: v, end: v)
  return Mesh(edge0: e, edge1: e)
}
