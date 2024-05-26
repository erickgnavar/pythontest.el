# pythontest.el

Execute python tests

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
