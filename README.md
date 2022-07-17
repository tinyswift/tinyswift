# tinyswift

- [x] Lexer
  - [x] Comments

  Available
  ```swift
  // This is a comment.
  ```
  
  Unavailable
  ```swift
  /* This is also a comment
  but is written over multiple lines. */
  ```
  - [x] Numbers

  Available
  ```swift
  let meaningOfLife = 42
  let pi = 3.14159
  let decimalInteger = 17
  let hexadecimalInteger = 0x11     // 17 in hexadecimal notation
  let decimalDouble = 12.1875
  let exponentDouble = 1.21875e1
  let paddedDouble = 000123.456
  ```
  
  Unavailable
  ```swift
  let binaryInteger = 0b10001       // 17 in binary notation
  let octalInteger = 0o21           // 17 in octal notation
  let hexadecimalDouble = 0xC.3p0
  let oneMillion = 1_000_000
  let justOverOneMillion = 1_000_000.000_000_1
  ```
- [ ] Parser
- [ ] AST
- [ ] IRGen

