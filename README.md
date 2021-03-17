# Assembly Language Interpreter in Haskell

Advanced Assembly language interpreter.

### Structure

- **`Grammar`**: Defines the data structures used in the abstract syntax tree (AST) and CPU state.
- **`Lexer`**: Provides lexical analysis functions and basic parsing utilities.
- **`Parser`**: Parses assembly source code and converts it into an AST.
- **`Evaluator`**: Executes the parsed instructions by simulating a CPU state.
- **`Interface`**: Offers convenient functions to run programs and interact with the evaluator.
- **`Main`**: A sample pr

#### Instructions

- **Arithmetic Operations**: `inc`, `dec`, `add`, `sub`, `mul`, `div`
- **Bitwise Operations**: `xor`, `and`, `or`, `not`, `neg`
- **Shift Operations**: `shr`, `shl`, `rol`, `ror`
- **Control Flow**: `jmp`, `je`, `jne`, `jg`, `jl`, `jge`, `jle`, `jz`, `jo`, `jc`, `jp`, `js`, `jnz`, `jno`, `jnc`, `jnp`, `jns`, `call`, `ret`
- **Stack Operations**: `push`, `pop`, `pushf`, `popf`
- **Miscellaneous**: `mov`, `cmp`, `msg`, `end`

## Usage

### Inline

```haskell
import Interface (runStrIO)

main :: IO ()
main = do
  let code = "mov ax, 5\nmsg 'ax is ', ax\nend"
  runStrIO code
```

### External module

```haskell
import Interface (runFileIO)

main :: IO ()
main = do
  runFileIO "code.asm"
```

### Via CLI

Compile and run the `Main.hs` file:

```bash
ghc Main.hs -o assembler
./assembler
```
