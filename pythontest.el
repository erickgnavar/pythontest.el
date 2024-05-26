;;; pythontest.el --- Testing executor for python -*- lexical-binding: t -*-

;; Copyright © 2024 Erick Navarro
;; Author: Erick Navarro <erick@navarro.io>
;; URL: https://github.com/erickgnavar/pythontest.el
;; Version: 0.1.0
;; SPDX-License-Identifier: GNU General Public License v3.0 or later
;; Package-Requires: ((emacs "29.1"))

;;; Commentary:
;; Run test on demand with support for many test runners

;;; Code:

(require 'compile)
(require 'project)
(require 'treesit)

(defcustom pythontest-test-runner "unittest"
  "Test runner to be executed."
  :type 'string
  :group 'python)

(defcustom pythontest-unittest-command "python3 -m unittest"
  "Command to be executed when running unittest."
  :type 'string
  :group 'python)

(defun pythontest-test-all ()
  "Run all the project test suite."
  (interactive)
  (pythontest--run-compile pythontest-unittest-command))

(defun pythontest-test-file ()
  "Run all file test suite."
  (interactive)
  (let* ((command (concat pythontest-unittest-command " " (pythontest--unittest-get-file-path))))
    (pythontest--run-compile command)))

(defun pythontest-test-at-point ()
  "Run test at point."
  (interactive)
  (let ((command (concat pythontest-unittest-command " " (pythontest--unittest-get-file-path) "." (pythontest--get-test-at-point))))
    (pythontest--run-compile command)))

(defun pythontest--project-root ()
  "Return project root using project.el."
  (expand-file-name (project-root (project-current))))

(defun pythontest--unittest-get-file-path ()
  "Return relative filepath as valid unittest format."
  (let* ((relative-path (string-replace (pythontest--project-root) "" (buffer-file-name)))
         (path-no-extension (string-replace ".py" "" relative-path)))
    (string-replace "/" "." path-no-extension)))

(defun pythontest--run-compile (command)
  "Run compile COMMAND."
  (let ((default-directory (pythontest--project-root))
        (compile-command command))
    (compile compile-command)))

(defun pythontest--get-test-at-point ()
  "Compute test path at point using treesitter.
It can be the path of a function or a class.
It will depends on the position of the cursos when this function is called."
  (unless (featurep 'treesit)
    (user-error "Treesit support is required"))
  (unless (treesit-language-available-p 'python)
    (user-error "Python grammar is required to use this function"))
  (let* ((lang (treesit-language-at (point)))
         (node (treesit-node-at (point) lang))
         (class-node (treesit-parent-until node (lambda (n)
                                                  (string-equal (treesit-node-type n) "class_definition"))))
         (function-node (treesit-parent-until node (lambda (n)
                                                     (string-equal (treesit-node-type n) "function_definition")))))
    (cond ((and class-node function-node)
           (format "%s.%s" (treesit-node-text (treesit-node-child class-node 1)) (treesit-node-text (treesit-node-child function-node 1))))

          ((and class-node (not function-node))
           (treesit-node-text (treesit-node-child class-node 1)))

          ((and function-node (not class-node))
           (treesit-node-text (treesit-node-child function-node 1)))
          (t nil))))

(provide 'pythontest)
;;; pythontest.el ends here.
