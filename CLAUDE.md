# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

MyLisp is a Lisp interpreter written in Rust that demonstrates **meta-circular evaluation** (bootstrapping) - implementing a Lisp interpreter using Lisp itself. This is an educational project based on concepts from SICP (Structure and Interpretation of Computer Programs) Chapter 4.

## Build and Run Commands

### Build
```bash
# Install Rust toolchain if needed
source ~/.cargo/env

# Build release version
cargo build --release
```

### Run
```bash
# Run a Lisp file
./target/release/mylisp <file.lisp>

# Start REPL (interactive mode)
./target/release/mylisp
```

### Test Bootstrap
```bash
# Run bootstrap demonstration
./target/release/mylisp final_bootstrap.lisp

# Run comprehensive feature test
./target/release/mylisp test_all.lisp
```

## Architecture

The interpreter follows a classic multi-stage architecture:

### 1. **Lexer** (`src/lexer.rs`)
- Tokenizes input string into tokens (numbers, symbols, strings, booleans, parentheses)
- Supports string escaping (`\n`, `\t`, `\"`, etc.)
- Skips comments starting with `;`

### 2. **Parser** (`src/parser.rs`)
- Converts token stream into Abstract Syntax Tree (AST)
- Handles S-expressions with proper nesting
- Supports multi-line expressions across entire files

### 3. **AST** (`src/ast.rs`)
- Defines expression types: `Number`, `Symbol`, `String`, `Bool`, `List`, `Nil`
- Implements `Display` for pretty-printing

### 4. **Environment** (`src/env.rs`)
- Manages lexical scope with parent-child chains
- Supports `define` (create new binding), `get` (lookup), `set` (mutate existing)
- Implements proper variable shadowing

### 5. **Evaluator** (`src/eval.rs`)
- **Core evaluation**: Interprets AST expressions
- **Special forms**: `define`, `set!`, `if`, `lambda`, `let`, `quote`, `eval`, `load`
- **Built-in operations**:
  - Arithmetic: `+`, `-`, `*`, `/`
  - Comparison: `=`, `<`, `>`, `<=`, `>=`
  - Logical: `not`, `and`, `or`
  - List operations: `car/head`, `cdr/tail`, `cons`, `append`, `list`
  - Predicates: `null?`, `symbol?`, `list?`, `number?`, `string?`, `eq?`
  - I/O: `display`, `newline`
- **Function application**: Evaluates lambda closures with proper argument binding

### 6. **Main** (`src/main.rs`)
- File mode: Parses and evaluates entire Lisp files (supports multi-line expressions)
- REPL mode: Interactive read-eval-print loop with proper EOF handling

## Key Implementation Details

### Lambda Closures
Lambda functions are stored as lists: `[lambda, params, body1, body2, ...]`
When called, a new environment is created with parent = calling environment, parameters are bound, and body expressions are evaluated sequentially.

### Multi-line Expressions
The file reader (`eval_program`) parses entire files as a single token stream, allowing expressions to span multiple lines without requiring continuation markers.

### Function Definition Syntax
Supports both styles:
- `(define name (lambda (x) body))`
- `(define (name x) body)` - syntactic sugar converted to lambda form

### Bootstrap Concept
The `final_bootstrap.lisp` file demonstrates meta-circular evaluation by implementing a simple `my-eval` function in Lisp that evaluates Lisp expressions. This shows how a language can define itself.

## Known Limitations

1. **Closure mutation**: `set!` cannot modify variables from outer scopes (only current scope)
2. **Missing `cond`**: Use nested `if` instead
3. **Missing `begin`**: Lambda bodies implicitly sequence, but no explicit `begin` form
4. **No macro system**: All macros would need to be implemented as special forms in Rust

## Adding New Features

### New Special Forms
Add to `Evaluator::eval()` match statement in `src/eval.rs`:
```rust
"your-form" => Self::eval_your_form(&list[1..], env),
```

### New Built-in Functions
Add to function application dispatch in `src/eval.rs`:
```rust
"your-fn" => Self::apply_your_fn(&evaluated_args),
```

Then implement the method following the pattern of existing functions.

## Important Files

- `final_bootstrap.lisp` - Demonstrates meta-circular evaluation (the core concept)
- `test_all.lisp` - Comprehensive feature test suite
- `BOOTSTRAP.md` - Detailed explanation of bootstrap concepts
- `src/eval.rs` - Core evaluation logic (most complex file)
- `src/lexer.rs` - Tokenization with comment/string support
