# Assembly Language Interpreter in Haskell

Advanced Assembly language interpreter.

### Structure

- **`Grammar`**: Defines the data structures used in the abstract syntax tree (AST) and CPU state.
- **`Lexer`**: Provides lexical analysis functions and basic parsing utilities.
- **`Parser`**: Parses assembly source code and converts it into an AST.
- **`Evaluator`**: Executes the parsed instructions by simulating a CPU state.
- **`Interface`**: Offers convenient functions to run programs and interact with the evaluator.
- **`Main`**: A sample pr