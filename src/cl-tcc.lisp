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

;;; Tiny-C Compiler

(in-package :cl-tcc)

(defun initialize-global-variables ()
  (setf *current-level* 0)
  (setf *parameters-quantity* 0)

  (setf *top-alloc* 0)
  (reset-last-alloc)
  (reset-label-number)

  (setf *code* nil)
  (setf *return-to* nil))

;; main

(defvar *options* '(("o" :required) ("O" :none)))

(defun main ()
  (multiple-value-bind (arg opts illegal)
      (getopt:getopt (cdr sb-ext:*posix-argv*) *options*)
    (awhen illegal (error "invalid arguments : ~A" illegal))
    (let ((ifname (car arg))
          (ofname (aif (cdr (assoc "o" opts :test #'string=)) it "a.asm")))
      (tcc ifname ofname (assoc "O" opts :test #'string=)))))

(defun tcc (ifname ofname &optional optimize)
  (with-open-file (in ifname :if-does-not-exist :error)
    (let ((src (do ((line (read-line in nil :eof) (read-line in nil :eof))
                    lines)
                   ((eq line :eof)
                    (reduce (lambda (f s) (mkstr f " " s)) (reverse lines)))
                 (push line lines))))
      (setf *token-table* nil)
      (setf *variable-table* nil)
      (setf *external-table* nil)
      (initialize-global-variables)
      (with-open-file (out ofname :direction :output :if-exists :rename-and-delete)
        (eval (yacc:parse-with-lexer (tc-lexer src) tc-parser))
        (setf *code* (reverse *code*))
        (when optimize (optimize-loop))
        (dolist (code *code*)
          (print-code code out))))))
