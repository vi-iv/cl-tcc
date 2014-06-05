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

;;; ASDF

(defpackage :cl-tiny-c-compiler-asd
  (:use :cl :asdf))

(in-package :cl-tiny-c-compiler-asd)

(asdf:defsystem :cl-tcc
  :name "cl-tcc"
  :description "KUIS le3b"
  :licence "MIT"

  :serial t
  :depends-on (:getopt :yacc :lispbuilder-regex :lispbuilder-lexer)
  :components ((:file "package")
               (:file "utility")
               (:file "parser"    :depends-on ("package" "utility"))
               (:file "generator" :depends-on ("package" "utility"))
               (:file "optimizer" :depends-on ("package" "utility"))
               (:file "cl-tcc"    :depends-on ("parser" "generator" "optimizer"))
               (:file "test"      :depends-on ("cl-tcc"))
               ))
