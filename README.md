# pythontest.el

Execute python tests

## Motivation

I used to use `elpy` to set up a programming environment for python, I use `LSP` with `pyright` now so I only was using
`elpy` to run tests. I wanted a simpler solution to run tests in 3 scenarios:

- Run the whole test suite
- Run a test file
- Run a specific test at point

So I make this package to have a simpler solution :)

## Installation

### Cloning the repo

Clone this repo somewhere, and add this to your config:

```elisp
(add-to-list 'load-path "path where the repo was cloned")

(require 'pythontest)
```

### Using straight.el

```emacs-lisp
(use-package pythontest
  :straight (pythontest
             :type git
             :host github
             :repo "erickgnavar/pythontest.el"))
```

## Usage

There are no default key binding, these functions can be used to run tests.

| Function                   | Description                        |
|----------------------------|------------------------------------|
| `pythontest-test-all`      | Run all the project tests          |
| `pythontest-test-file`     | Run only the current file          |
| `pythontest-test-at-point` | Run the function or class at point |

## FAQ

`How to pass flags when running pytest?`

You can customize variable `pythontest-pytest-command`, by default it points to `pytest` but it can be modified for
example to enable more verbosity with `(setq pythontest-pytest-command "pytest -vv")`.

`How can configure different runner per project?`

You can use a `.dir-locals.el` file to customize the variable `pythontest-test-runner` per project, for example to use
`unittest`:

```emacs-lisp
((python-ts-mode . ((pythontest-test-runner . "unittest"))))
```
