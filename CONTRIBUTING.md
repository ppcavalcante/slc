# Contributing to SLC

Thank you for your interest in the SLC project. This is a research implementation, and I welcome feedback, bug reports, and discussions from the community. If you're interested in extending the work, I'd be happy to hear from you.

## How to Contribute

The best way to contribute is to start a conversation.

*   **For bug reports or unexpected behavior:** Please [open an issue](https://github.com/ppcavalcante/slc/issues) with a clear description of the problem and, if possible, steps to reproduce it.

*   **For new ideas or feature suggestions:** Please [open an issue](https://github.com/ppcavalcante/slc/issues) or [start a discussion](https://github.com/ppcavalcante/slc/discussions) to talk about the proposal. This is especially helpful for larger ideas like:
    *   Adding new algebraic theories.
    *   Implementing new search algorithms.
    *   Extending the system to support multi-sorted or higher-order theories.

*   **For documentation typos or small fixes:** Feel free to submit a pull request directly.

## Development Workflow

If you'd like to work on the code, the process is straightforward:

1.  **Fork the repository** and clone it locally.
2.  **Set up your environment.** You'll need Racket 8.0+.
3.  **Run the tests** to make sure everything is working:
    ```bash
    racket tests/main-tests.rkt
    ```
4.  **Create a new branch** for your changes.
5.  **Write your code.** Please try to follow the existing style. If you add new features, please also add tests for them in the `tests/` directory.
6.  **Submit a Pull Request** with a clear description of your changes.

I appreciate you taking the time to contribute! This project aims to be a useful tool for researchers and students exploring the connections between algebra, logic, and computation.