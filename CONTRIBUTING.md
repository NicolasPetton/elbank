# Contributing

Contributions are welcome. If you discover bugs or issues, or have ideas for
improvements or new features, please file a report on the issue tracker for this
repository. Follow the guidelines below to make sure everything goes smoothly.

## Issue reporting

- Check that the issue has not already been reported
- Check that the issue has not already been fixed in the latest code
- Open an issue with a clear title
- Write as grammatically correct as you can in the description.

## Pull requests

- Perform all changes on a topic branch for easier merging
- Follow the coding conventions already in use
- Verify Emacs Lisp code with `checkdoc`
- Add unit tests whenever possible
- Open a [pull request](https://help.github.com/articles/using-pull-requests)
  relating to a single issue.

## Coding Conventions

### Naming

- Use a `elbank-` prefix for all public names.
- Use a `elbank--` prefix for all internal names.

### Docstrings

Write meaningful docstrings for all functions and vars.

- Document all functions and variables as directed by `checkdoc`.
- Consider using [Flycheck](https://github.com/flycheck/flycheck) to automate
  `checkdoc` while you're editing.

### Libraries

- Use `seq` and `map` for manipulating collections.
- Use `cl-lib`, which adds prefixes to all cl function names
- Use [noflet](https://github.com/nicferrier/emacs-noflet) instead of `flet`
  when you need to dynamically rebind functions.
