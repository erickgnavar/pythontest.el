;;; pythontest.el --- Testing executor for python -*- lexical-binding: t -*-

;; Copyright © 2024 Erick Navarro
;; Author: Erick Navarro <erick@navarro.io>
;; URL: https://github.com/erickgnavar/pythontest.el
;; Version: 0.1.0
;; SPDX-License-Identifier: GPL-3.0-or-later
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:
;; Run test on demand with support for many test runners

;;; Code:

(require 'compile)
(require 'project)
(require 'treesit)

(defcustom pythontest-test-runner "unittest"
  "Test runner to be executed."
  :type '(choice (const :tag "Unittest" "unittest")
                 (const :tag "Pytest" "pytest")
                 (const :tag "Django" "django"))
  :group 'python)

(defcustom pythontest-unittest-command "python3 -m unittest"
  "Command to be executed when running unittest."
  :type 'string
  :group 'python)

(defcustom pythontest-pytest-command "pytest"
  "Command to be executed when running pytest."
  :type 'string
  :group 'python)

(defcustom pythontest-django-command "python3 manage.py test"
  "Command to be executed when running tests with Djando."
  :type 'string
  :group 'python)

;;;###autoload
(defun pythontest-change-test-runner ()
  "Chang test runner."
  (interactive)
  (setq pythontest-test-runner (completing-read "Choose test runner: " '("unittest" "pytest" "django"))))

;;;###autoload
(defun pythontest-test-all ()
  "Run all the project test suite."
  (interactive)
  (cond ((string-equal pythontest-test-runner "unittest")
         (pythontest--run-compile pythontest-unittest-command))
        ((string-equal pythontest-test-runner "pytest")
         (pythontest--run-compile pythontest-pytest-command))
        ((string-equal pythontest-test-runner "django")
         (pythontest--run-compile pythontest-django-command))
        (t (pythontest--not-valid-runner-print-message))))

;;;###autoload
(defun pythontest-test-file ()
  "Run all file test suite."
  (interactive)
  (let* ((command (cond ((string-equal pythontest-test-runner "unittest")
                         (concat pythontest-unittest-command " " (pythontest--unittest-get-file-path)))
                        ((string-equal pythontest-test-runner "pytest")
                         (concat pythontest-pytest-command " " (pythontest--pytest-get-file-path)))
                        ((string-equal pythontest-test-runner "django")
                         (concat pythontest-django-command " " (pythontest--unittest-get-file-path))))))
    (unless command
      (pythontest--not-valid-runner-print-message))
    (pythontest--run-compile command)))

;;;###autoload
(defun pythontest-test-at-point ()
  "Run test at point."
  (interactive)
  (let ((command (cond ((string-equal pythontest-test-runner "unittest")
                        (concat pythontest-unittest-command " " (pythontest--unittest-get-file-path) "." (pythontest--get-test-at-point ".")))
                       ((string-equal pythontest-test-runner "pytest")
                        (concat pythontest-pytest-command " " (pythontest--pytest-get-file-path) "::" (pythontest--get-test-at-point "::")))
                       ((string-equal pythontest-test-runner "django")
                        (concat pythontest-django-command " " (pythontest--unittest-get-file-path) "." (pythontest--get-test-at-point ".")))
                       (t (pythontest--not-valid-runner-print-message)))))
    (pythontest--run-compile command)))

(defun pythontest--not-valid-runner-print-message ()
  "Print `user-error' message it the configured runner is not valid."
  (user-error "Not valid value for `pythontest-test-runner': %s" pythontest-test-runner))

(defun pythontest--project-root ()
  "Return project root using project.el."
  (expand-file-name (project-root (project-current))))

(defun pythontest--unittest-get-file-path ()
  "Return relative filepath as valid unittest format."
  (let* ((relative-path (string-replace (pythontest--project-root) "" (buffer-file-name)))
         (path-no-extension (string-replace ".py" "" relative-path)))
    (string-replace "/" "." path-no-extension)))

(defun pythontest--pytest-get-file-path ()
  "Return relative filepath as valid pytest format."
  (string-replace (pythontest--project-root) "" (buffer-file-name)))

(defun pythontest--run-compile (command)
  "Run compile COMMAND."
  (let ((default-directory (pythontest--project-root))
        (compile-command command))
    (compile (shell-quote-argument compile-command))))

(defun pythontest--get-test-at-point (separator)
  "Compute test path at point and return expression using SEPARATOR.
It can be the path of a function or a class.
It will depends on the position of the cursos when this function is called."
  (unless (treesit-language-available-p 'python)
    (user-error "Python grammar is required to use this function"))
  (let* ((lang (treesit-language-at (point)))
         (node (treesit-node-at (point) lang))
         (class-node (treesit-parent-until node (lambda (n)
                                                  (string-equal (treesit-node-type n) "class_definition"))))
         (function-node (treesit-parent-until node (lambda (n)
                                                     (string-equal (treesit-node-type n) "function_definition")))))
    (cond ((and class-node function-node)
           (concat (treesit-node-text (treesit-node-child class-node 1)) separator (treesit-node-text (treesit-node-child function-node 1))))

          ((and class-node (not function-node))
           (treesit-node-text (treesit-node-child class-node 1)))

          ((and function-node (not class-node))
           (treesit-node-text (treesit-node-child function-node 1)))
          (t nil))))

(provide 'pythontest)
;;; pythontest.el ends here.
