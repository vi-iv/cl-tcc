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

;;; Code Generator

(in-package :cl-tcc)

;; register

(defstruct code label inst subinst op1 op2)

(defstruct (reg (:constructor reg)) name)  ; レジスタ
(defstruct (mem (:constructor mem)) addrs) ; 引数、局所変数、一時変数
(defstruct (gvr (:constructor gvr)) name)  ; 大域変数
(defstruct (lbl (:constructor lbl)) name)  ; ラベル

(defvar +eax+ (reg :name "eax"))
(defvar +ebx+ (reg :name "ebx"))
(defvar +ecx+ (reg :name "ecx")) ; 未使用
(defvar +edx+ (reg :name "edx"))
(defvar +ebp+ (reg :name "ebp"))
(defvar +esp+ (reg :name "esp"))
(defvar +al+  (reg :name "al"))

;; variable

(defvar *variable-table* nil)

(defun lookup-variable (name)
  (find name *variable-table* :key #'name :test #'string=))

(defun push-variable (token)
  (when (not (lookup-variable (name token)))
    (setf (offset (car (push token *variable-table*)))
          (allocate-loc))))

(defun pop-variables ()
  (do ()
      ((or (null *variable-table*)
           (<= (level (car *variable-table*)) *current-level*)));;;last_alloc?
    (pop *variable-table*)
    (release-loc)))

;; main

(defvar *code* nil)

(defun emit (&key label inst subinst op1 op2)
  (push (make-code :label label :inst inst :subinst subinst :op1 op1 :op2 op2) *code*))

(defun print-code (code &optional (stream t))
  (with-slots (label inst subinst op1 op2) code
    (let* ((oplst (list label op1 op2))
           (f (if (and (notany #'reg-p oplst)
                       (or (some #'mem-p oplst) (some #'gvr-p oplst)))
                  1 0)))
      (destructuring-bind (lbln op1n op2n)
          (mapcar (lambda (op) (cond ((reg-p op) (reg-name op))
                                     ((mem-p op) (format nil "[ebp~@D]" (mem-addrs op)))
                                     ((gvr-p op) (format nil "[~A]" (gvr-name op)))
                                     ((lbl-p op) (format nil "~A" (lbl-name op)))
                                     ((stringp op) op)
                                     ((numberp op) (mkstr op))))
                  oplst)
        (let* ((inst-form "~@[~A~]~A~A~[~:; dword~]~*~@[~:*~A~A~]~@[\, ~A~]")
               (subinst-form "~@[~A~]~A~A~*~@[~:*~A~A~]~@[ ~A~]")
               (form (cond (inst    (format nil inst-form
                                            lbln #\tab inst f #\tab op1n op2n))
                           (subinst (format nil subinst-form
                                            lbln #\tab subinst #\tab op1n op2n))
                           (label   (mkstr lbln #\:)))))
          (awhen form (format stream "~A~%" it)))))))

(let ((label-number 0))
  (defun make-label (name)
    (format nil "L~D_~A" (1- (incf label-number)) name))
  (defun reset-label-number ()
    (setf label-number 0)))

;; code emitter

(defvar *return-to* nil)
(defvar *external-table* nil)

(defgeneric emit-program (node &key)
  (:documentation ""))

(defmethod emit-program ((n (eql nil)) &key) n)

(defmethod emit-program ((const const) &key)
  (emit :inst "mov" :op1 +eax+ :op2 (value const)))

(defmethod emit-program ((token token) &key)
  (with-slots (name kind level offset label) token
    (when (or (eq kind :var) (eq kind :parm))
      (let ((mem (locate-identifier token)))
        (emit :inst "mov" :op1 +eax+ :op2 mem)))))

(defun locate-identifier (token)
  (with-slots (name kind level offset label) token
    (when (eq kind :var)
      (if (and (= level 0) (null label))
          (let ((l (make-label name)))
            ;; 未定義の大域変数の場合
            (setf label l)
            (emit :subinst "COMMON" :op1 (lbl :name l) :op2 4))
          (push-variable token)))
    (when (or (eq kind :var) (eq kind :parm))
      ;; 変数または仮引数の場合
      (if (= level 0) (gvr :name label) (mem :addrs offset)))))

(defmethod emit-program ((tuple tuple) &key to reverse)
  (with-slots (op children) tuple
    (labels ((cmp-assign (cc)
               (let ((mem (mem :addrs (allocate-loc))))
                 (emit-program (nth 1 children))
                 (emit :inst "mov" :op1 mem :op2 +eax+)
                 (emit-program (nth 0 children))
                 (emit :inst "cmp" :op1 +eax+ :op2 mem)
                 (emit :inst (mkstr "set" cc) :op1 +al+)
                 (emit :inst "movzx" :op1 +eax+ :op2 +al+))
               (release-loc)))
      (case op
        (:cons
         (emit-program (nth 0 children))
         (emit-program (nth 1 children)))
        (:cmpd-stm
         (incf *current-level*)
         (emit-program (nth 0 children))
         (emit-program (nth 1 children))
         (decf *current-level*)
         (pop-variables))
        (:fundef
         (with-slots (name offset) (nth 0 (children (nth 0 children)))
           (let ((lbl0 (lbl :name name)) (lbl1 (lbl :name (mkstr "ret_" name))))
             (setf *return-to* lbl1)
             (emit :subinst "GLOBAL" :op1 lbl0)
             (emit :label lbl0)
             (emit :inst "push" :op1 +ebp+)
             (emit :inst "mov" :op1 +ebp+ :op2 +esp+)
             (incf *current-level*)
             (let ((len (length *code*)))
               (emit-program (nth 2 children))
               (when (/= *top-alloc* 0)
                 (let ((head (popn (- (length *code*) len) *code*)))
                   (emit :inst "sub" :op1 +esp+ :op2 (mkstr (- *top-alloc*)))
                   (setf *code* (append head *code*)))))
             (emit :label lbl1)
             (when (/= *top-alloc* 0)
               (emit :inst "mov" :op1 +esp+ :op2 +ebp+))
             (emit :inst "pop" :op1 +ebp+)
             (if (/= offset 0)
                 (emit :inst "ret" :op1 (mkstr (* 4 offset)))
                 (emit :inst "ret"))
             (setf *top-alloc* 0)
             (decf *current-level*)
             (pop-variables))))
        (:if
         (let ((lbl0 (lbl :name (make-label "if_else")))
               (lbl1 (lbl :name (make-label "if_end")))
               (cnd (nth 0 children)))
           (if (and (tuple-p cnd)
                    (member (op cnd)
                            '(:eq-assign :le-assign :ge-assign :< :> :ne-assign)))
               (if (nth 2 children)
                   (emit-program cnd :to lbl0 :reverse t)
                   (emit-program cnd :to lbl1 :reverse t))
               (progn (emit-program cnd)
                      (emit :inst "cmp" :op1 +eax+ :op2 0)
                      (if (nth 2 children)
                          (emit :inst "je" :op1 lbl0)
                          (emit :inst "je" :op1 lbl1))))
           (emit-program (nth 1 children))
           (when (nth 2 children)
             ;; 常に評価しても良い(跳ばされるだけ)
             (emit :inst "jmp" :op1 lbl1)
             (emit :label lbl0)
             (emit-program (nth 2 children)))
           (emit :label lbl1)))
        (:while
         (let ((lbl0 (lbl :name (make-label "while_begin")))
               (lbl1 (lbl :name (make-label "while_end"))))
           (emit :label lbl0)
           (emit-program (nth 0 children))
           (emit :inst "cmp" :op1 +eax+ :op2 "0")
           (emit :inst "je" :op1 lbl1)
           (emit-program (nth 1 children))
           (emit :inst "jmp" :op1 lbl0)
           (emit :label lbl1)))
        (:return
          (emit-program (nth 0 children))
          (emit :inst "jmp" :op1 *return-to*))
        (:=
         (let ((mem (locate-identifier (nth 0 children))))
           (with-slots (kind offset level label) (nth 0 children)
             (emit-program (nth 0 children))
             (emit-program (nth 1 children))
             (emit :inst "mov" :op1 mem :op2 +eax+))))
        (:+
         (let ((left (nth 0 children)) (right (nth 1 children)))
           (cond ((const-p right)
                  (emit-program left)
                  (emit :inst "add" :op1 +eax+ :op2 (value right)))  ; L
                 ((token-p right)
                  (emit-program left)
                  (awhen (locate-identifier right)
                    (emit :inst "add" :op1 +eax+ :op2 it)))          ; L
                 ((const-p left)
                  (emit-program right)
                  (emit :inst "add" :op1 +eax+ :op2 (value right)))  ; R
                 ((token-p left)
                  (emit-program right)
                  (awhen (locate-identifier left)
                    (emit :inst "add" :op1 +eax+ :op2 it)))          ; R
                 (t
                  (let ((mem (mem :addrs (allocate-loc))))
                    (emit-program right)
                    (emit :inst "mov" :op1 mem :op2 +eax+)
                    (emit-program left)
                    (emit :inst "add" :op1 +eax+ :op2 mem)
                    (release-loc))))))                               ; RSL
        (:-
         (let ((left (nth 0 children)) (right (nth 1 children)))
           (cond ((const-p right)
                  (emit-program left)
                  (emit :inst "sub" :op1 +eax+ :op2 (value right)))  ; L
                 ((token-p right)
                  (emit-program left)
                  (awhen (locate-identifier right)
                    (emit :inst "sub" :op1 +eax+ :op2 it)))          ; L
                 ((const-p left)
                  (emit-program right)
                  (emit :inst "mov" :op1 +ebx+ :op2 +eax+)
                  (emit-program left)
                  (emit :inst "sub" :op1 +eax+ :op2 +ebx+))          ; RL
                 ((token-p left)
                  (emit-program right)
                  (emit :inst "mov" :op1 +ebx+ :op2 +eax+)
                  (emit-program left)
                  (emit :inst "sub" :op1 +eax+ :op2 +ebx+))          ; RL
                 (t
                  (let ((mem (mem :addrs (allocate-loc))))
                    (emit-program right)
                    (emit :inst "mov" :op1 mem :op2 +eax+)
                    (emit-program left)
                    (emit :inst "sub" :op1 +eax+ :op2 mem)
                    (release-loc))))))                               ; RSL
        (:*
         (let ((left (nth 0 children)) (right (nth 1 children)))
           (cond ((const-p right)
                  (emit-program left)
                  (emit :inst "imul" :op1 +eax+ :op2 (value right))) ; L
                 ((token-p right)
                  (emit-program left)
                  (awhen (locate-identifier right)
                    (emit :inst "imul" :op1 +eax+ :op2 it)))         ; L
                 ((const-p left)
                  (emit-program right)
                  (emit :inst "imul" :op1 +eax+ :op2 (value right))) ; R
                 ((token-p left)
                  (emit-program right)
                  (awhen (locate-identifier left)
                    (emit :inst "imul" :op1 +eax+ :op2 it)))         ; R
                 (t
                  (let ((mem (mem :addrs (allocate-loc))))
                    (emit-program right)
                    (emit :inst "mov" :op1 mem :op2 +eax+)
                    (emit-program left)
                    (emit :inst "imul" :op1 +eax+ :op2 mem)
                    (release-loc))))))                               ; RSL
        (:/
         (let ((left (nth 0 children)) (right (nth 1 children))
               (mem (mem :addrs (allocate-loc))))
           (cond ((const-p right)
                  (emit :inst "mov" :op1 mem :op2 (value right))
                  (emit-program left)
                  (emit :inst "cdq")
                  (emit :inst "idiv" :op1 mem))                      ; L
                 ((token-p right)
                  (awhen (locate-identifier right)
                    (emit-program left)
                    (emit :inst "cdq")
                    (emit :inst "idiv" :op1 it)))                    ; L
                 ((const-p left)
                  (emit-program right)
                  (emit :inst "mov" :op1 +ebx+ :op2 +eax+)
                  (emit-program left)
                  (emit :inst "cdq")
                  (emit :inst "idiv" :op1 +ebx+))                    ; RL
                 ((token-p left)
                  (emit-program right)
                  (emit :inst "mov" :op1 +ebx+ :op2 +eax+)
                  (emit-program left)
                  (emit :inst "cdq")
                  (emit :inst "idiv" :op1 +ebx+))                    ; RL
                 (t
                  (let ((mem (mem :addrs (allocate-loc))))
                    (emit-program right)
                    (emit :inst "mov" :op1 mem :op2 +eax+)
                    (emit-program left)
                    (emit :inst "cdq")
                    (emit :inst "idiv" :op1 mem))))                  ; RSL
           (release-loc)))
        (:%
         (let ((left (nth 0 children)) (right (nth 1 children))
               (mem (mem :addrs (allocate-loc))))
           (cond ((const-p right)
                  (emit :inst "mov" :op1 mem :op2 (value right))
                  (emit-program left)
                  (emit :inst "cdq")
                  (emit :inst "idiv" :op1 mem))                      ; L
                 ((token-p right)
                  (awhen (locate-identifier right)
                    (emit-program left)
                    (emit :inst "cdq")
                    (emit :inst "idiv" :op1 it)))                    ; L
                 ((const-p left)
                  (emit-program right)
                  (emit :inst "mov" :op1 +ebx+ :op2 +eax+)
                  (emit-program left)
                  (emit :inst "cdq")
                  (emit :inst "idiv" :op1 +ebx+))                    ; RL
                 ((token-p left)
                  (emit-program right)
                  (emit :inst "mov" :op1 +ebx+ :op2 +eax+)
                  (emit-program left)
                  (emit :inst "cdq")
                  (emit :inst "idiv" :op1 +ebx+))                    ; RL
                 (t
                  (let ((mem (mem :addrs (allocate-loc))))
                    (emit-program right)
                    (emit :inst "mov" :op1 mem :op2 +eax+)
                    (emit-program left)
                    (emit :inst "cdq")
                    (emit :inst "idiv" :op1 mem))))                  ; RSL
           (emit :inst "mov" :op1 +eax+ :op2 +edx+)
           (release-loc)))
        ((:eq-assign :le-assign :ge-assign :< :> :ne-assign)
         (let ((pos (position op '(:eq-assign :le-assign :ge-assign :< :> :ne-assign)))
               (ccs '("e" "le" "ge" "l" "g" "ne"))
               (fns '(= <= >= < > /=)))
           (let ((cc (if reverse (elt ccs (- (length ccs) pos 1)) (elt ccs pos)))
                 (fn (if reverse (elt fns (- (length fns) pos 1)) (elt fns pos)))
                 (left (nth 0 children)) (right (nth 1 children))
                 (mem (mem :addrs (allocate-loc))))
             (if to
                 (cond ((and (const-p left) (const-p right))
                        (when (funcall fn
                                       (parse-integer (value left))
                                       (parse-integer (value right)))
                          (emit :inst "jmp" :op1 to)))
                       ((const-p right)
                        (emit-program left)
                        (emit :inst "cmp" :op1 +eax+ :op2 (value right))
                        (emit :inst (mkstr "j" cc) :op1 to))             ; L
                       ((token-p right)
                        (emit-program left)
                        (awhen (locate-identifier right)
                          (emit :inst "cmp" :op1 +eax+ :op2 it)
                          (emit :inst (mkstr "j" cc) :op1 to)))          ; L
                       (t
                        (emit-program (nth 1 children))
                        (emit :inst "mov" :op1 mem :op2 +eax+)
                        (emit-program (nth 0 children))
                        (emit :inst "cmp" :op1 +eax+ :op2 mem)
                        (emit :inst (mkstr "j" cc) :op1 to)))            ; RSL
                 (cmp-assign cc))
             (release-loc))))
        (:and-assign
         (let ((lbl (lbl :name (make-label "and_to")))
               (mem (mem :addrs (allocate-loc))))
           (emit :inst "mov" :op1 mem :op2 "0")
           (emit-program (nth 0 children))
           (emit :inst "cmp" :op1 +eax+ :op2 "0")
           (emit :inst "je" :op1 lbl)
           (emit-program (nth 1 children))
           (emit :inst "cmp" :op1 +eax+ :op2 "0")
           (emit :inst "je" :op1 lbl)
           (emit :inst "mov" :op1 mem :op2 "1")
           (emit :label lbl)
           (emit :inst "mov" :op1 +eax+ :op2 mem)
           (release-loc)))
        (:or-assign
         (let ((lbl (lbl :name (make-label "or_to")))
               (mem (mem :addrs (allocate-loc))))
           (emit :inst "mov" :op1 mem :op2 "1")
           (emit-program (nth 0 children))
           (emit :inst "cmp" :op1 +eax+ :op2 "0")
           (emit :inst "jne" :op1 lbl)
           (emit-program (nth 1 children))
           (emit :inst "cmp" :op1 +eax+ :op2 "0")
           (emit :inst "jne" :op1 lbl)
           (emit :inst "mov" :op1 mem :op2 "0")
           (emit :label lbl)
           (emit :inst "mov" :op1 +eax+ :op2 mem)
           (release-loc)))
        (:pstfix
         (with-slots (name kind offset) (nth 0 children)
           (when (eq kind :undeffun)
             (push name *external-table*)
             (emit :subinst "EXTERN" :op1 name)
             (setf kind :fun))
           (emit-program (nth 1 children))
           (emit :inst "call" :op1 name)
           (when (and (find name *external-table* :test #'string=)
                      (/= offset 0))
             (emit :inst "add" :op1 +esp+ :op2 (mkstr (* 4 offset))))))
        (:args
         (with-slots (op children) tuple
           (emit-program (nth 0 children))
           (emit :inst "push" :op1 +eax+)
           (emit-program (nth 1 children))))))))
