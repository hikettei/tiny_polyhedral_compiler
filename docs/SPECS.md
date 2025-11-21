# Caten Operations (Ops) Specification

This document details the operations available in Caten's Intermediate Representation (IR), as defined in `caten/ops.py`. These operations are grouped by their nature to provide a clear understanding of the instruction set.

Each operation is represented as a `Node` in the computation graph.

---

## 1. Unary Operations (`UnaryOps`)

These operations take a single input `Node` and produce a single output `Node`.

| Operation | Description                            | Inputs (`src` tuple) | Arguments (`arg`) |
| :-------- | :------------------------------------- | :------------------- | :---------------- |
| `NEG`     | Negation (e.g., `-x`)                  | `(input_node,)`      | `None`            |
| `RECIP`   | Reciprocal (e.g., `1/x`)               | `(input_node,)`      | `None`            |
| `SIN`     | Sine function (e.g., `sin(x)`)         | `(input_node,)`      | `None`            |
| `EXP2`    | Base-2 exponential (e.g., `2^x`)       | `(input_node,)`      | `None`            |
| `LOG2`    | Base-2 logarithm (e.g., `log2(x)`)     | `(input_node,)`      | `None`            |
| `SQRT`    | Square root (e.g., `sqrt(x)`)          | `(input_node,)`      | `None`            |
| `NOT`     | Logical NOT (e.g., `!x`)               | `(input_node,)`      | `None`            |
| `CAST`    | Type casting                         | `(input_node,)`      | Target `DType`    |

---

## 2. Binary Operations (`BinaryOps`)

These operations take two input `Node`s and produce a single output `Node`.

| Operation | Description                            | Inputs (`src` tuple)     | Arguments (`arg`) |
| :-------- | :------------------------------------- | :----------------------- | :---------------- |
| `ADD`     | Addition (e.g., `a + b`)               | `(input_a, input_b)`     | `None`            |
| `MUL`     | Multiplication (e.g., `a * b`)         | `(input_a, input_b)`     | `None`            |
| `IDIV`    | Integer division (e.g., `a // b`)      | `(input_a, input_b)`     | `None`            |
| `AND`     | Logical AND (e.g., `a && b`)           | `(input_a, input_b)`     | `None`            |
| `OR`      | Logical OR (e.g., `a \|\| b`)           | `(input_a, input_b)`     | `None`            |
| `XOR`     | Logical XOR (e.g., `a ^ b`)            | `(input_a, input_b)`     | `None`            |
| `MAX`     | Maximum of two inputs (e.g., `max(a, b)`) | `(input_a, input_b)`     | `None`            |
| `MOD`     | Modulo operation (e.g., `a % b`)       | `(input_a, input_b)`     | `None`            |
| `NEQ`     | Not Equal (e.g., `a != b`)             | `(input_a, input_b)`     | `None`            |
| `LT`      | Less Than (e.g., `a < b`)              | `(input_a, input_b)`     | `None`            |

---

## 3. Ternary Operations (`TernaryOps`)

These operations take three input `Node`s and produce a single output `Node`.

| Operation | Description                            | Inputs (`src` tuple)             | Arguments (`arg`) |
| :-------- | :------------------------------------- | :------------------------------- | :---------------- |
| `WHERE`   | Conditional select (e.g., `cond ? a : b`) | `(condition_node, true_node, false_node)` | `None`            |

---

## 4. Memory Operations (`MemoryOps`)

These operations interact with tensor memory.

| Operation | Description                            | Inputs (`src` tuple)                  | Arguments (`arg`)          |
| :-------- | :------------------------------------- | :------------------------------------ | :------------------------- |
| `LOAD`    | Read from tensor memory                | `(tensor_node,)`                      | Index (tuple or scalar)    |
| `STORE`   | Write to tensor memory                 | `(tensor_node, value_node)`           | Index (tuple or scalar)    |

---

## 5. Control Flow Operations (`ControlOps`)

These operations define the structure of control flow in the computation graph. Their `arg` often contains nested blocks of `Node`s.

| Operation | Description                            | Inputs (`src` tuple) | Arguments (`arg`)                                                                   |
| :-------- | :------------------------------------- | :------------------- | :---------------------------------------------------------------------------------- |
| `RANGE`   | Loop structure (e.g., `for` loop)      | `()`                 | `(iter_sym: Symbol, bounds: tuple, body_block: List[Node], directives: List[Directive])` |
| `IF`      | Conditional branch (e.g., `if` / `else`) | `()`                 | `(condition_node: Node, then_block: List[Node], else_block: List[Node])`          |

---

## 6. Meta Operations (`MetaOps`)

These operations represent terminals, arguments, or metadata within the graph.

| Operation    | Description                            | Inputs (`src` tuple) | Arguments (`arg`)                                  |
| :----------- | :------------------------------------- | :------------------- | :------------------------------------------------- |
| `CONST`      | Literal constant value                 | `()`                 | Constant value (e.g., `0.0`, `5`, `True`)          |
| `VAR`        | Symbolic variable (e.g., loop iterator) | `()`                 | `Symbol` object                                    |
| `PLACEHOLDER`| Function argument (input tensor)       | `()`                 | `TensorSpec` object                                |
| `DIRECTIVE`  | Compiler directive (e.g., parallel)    | `()`                 | `Directive` object (name, args)                    |

