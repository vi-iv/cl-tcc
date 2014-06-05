;; =============================================================================
;; The MIT License (MIT)
;;
;; Copyright (c) 2013, Koutarou FURUKAWA (<Furukawa.Koutarou@Gmail.com>)
;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
;; THE SOFTWARE.
;; =============================================================================

;;; Test

(in-package :cl-tcc)

;; environment

(defvar *test-source-directory* "test")

(defvar *tc-file-postfix*  ".tc")
(defvar *a-file-postfix*   ".asm")
(defvar *o-file-postfix*   ".o")
(defvar *exe-file-postfix* ".out")

(defvar *nasm* "/usr/bin/nasm")
(defvar *nasm-options* (list "-f" "elf32"))

(defvar *gcc* "/usr/bin/gcc")
(defvar *gcc-options* (list "-m32" "-W" "-Wall"))

(defparameter *test-source-files*
  (list (list "zero.tc" "print_number.c")
        (list "assign.tc" "print_number.c")
        (list "add_constant.tc" "print_number.c")
        (list "add_variable.tc" "print_number.c")
        (list "expression.tc" "print_number.c")
        (list "function.tc" "print_number.c")
        (list "global_variable.tc" "print_number.c")
        (list "if.tc" "print_number.c")

        ))

;; main

(defun run-test (&optional optimize)
  (dolist (test-files *test-source-files*)
    (destructuring-bind (tc-file-name &rest c-file-names) test-files
      (let ((basename (subseq tc-file-name 0 (- (length tc-file-name) 3))))
        (labels ((mkpath (name) (mkstr *test-source-directory* "/" name)))
          (let ((tc-file  (mkpath tc-file-name))
                (c-files  (mapcar #'mkpath c-file-names))
                (a-file   (mkpath (mkstr basename *a-file-postfix*)))
                (o-file   (mkpath (mkstr basename *o-file-postfix*)))
                (exe-file (mkpath (mkstr basename *exe-file-postfix*))))
            (let ((nasm-args (append *nasm-options* (list "-o" o-file a-file)))
                  (gcc-args (append *gcc-options* c-files (list "-o" exe-file o-file))))
              (format t "TEST: ~A~%" tc-file)
              (format t "~3Tcl-tcc:tcc ~A ~A ...~%" tc-file a-file)
              (cl-tcc:tcc tc-file a-file optimize)
              (format t "~3T~A ~{~A ~}...~%" *nasm* nasm-args)
              (sb-ext:run-program *nasm* nasm-args :output *standard-output*)
              (format t "~3T~A ~{~A ~}...~%" *gcc* gcc-args)
              (sb-ext:run-program *gcc* gcc-args :output *standard-output*)
              (format t "~3T~A ...~%" exe-file)
              (sb-ext:run-program exe-file nil :output *standard-output*)
              (format t "~%"))))))))
