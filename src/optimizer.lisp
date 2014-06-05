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

;;; Optimizer

(in-package :cl-tcc)

;; equation

(defun code= (c1 c2)
  (with-slots ((l1 label) (i1 inst) (s1 subinst) (op1-1 op1) (op2-1 op2)) c1
    (with-slots ((l2 label) (i2 inst) (s2 subinst) (op1-2 op1) (op2-2 op2)) c2
      (and (label= l1 l2) (inst= i1 i2) (inst= s1 s2)
           (op= op1-1 op1-2) (op= op2-1 op2-2)))))

(defun inst= (i1 i2) (string= i1 i2))

(defun label= (l1 l2)
  (destructuring-bind (l1 l2)
      (mapcar (lambda (l) (cond ((stringp l) l) ((lbl-p l) (lbl-name l))))
              (list l1 l2))
    (string= l1 l2)))

(defun op= (op1 op2)
  (destructuring-bind (op1 op2)
      (mapcar (lambda (op) (cond ((stringp op) op)
                                 ((reg-p op) (reg-name op))
                                 ((mem-p op) (mkstr op))
                                 ((gvr-p op) (gvr-name op))))
              (list op1 op2))
    (string= op1 op2)))

;; optimization tools

(defvar *optimize-temp-0* nil)
(defvar *optimize-temp-1* nil)

(defun meaningless-mov-2 (c1 c2)
  (with-slots ((l1 label) (i1 inst) (op1-1 op1) (op2-1 op2)) c1
    (with-slots ((l2 label) (i2 inst) (op1-2 op1) (op2-2 op2)) c2
      (and (null l1) (inst= i1 "mov") (inst= i2 "mov") (op= op1-1 op1-2)
           (values t (list c2))))))

(defun replace-to-specific-inst (c2)
  (with-slots (label inst op1 op2) c2
    (cond ((or (and (inst= inst "add") (op= op2 "1"))
               (and (inst= inst "sub") (op= op2 "-1")))
           (values t (list (make-code :label label :inst "inc" :op1 op1))))
          ((or (and (inst= inst "add") (op= op2 "-1"))
               (and (inst= inst "sub") (op= op2 "1")))
           (values t (list (make-code :label label :inst "dec" :op1 op1)))))))

(defun meaningless-add-and-mul (c2)
  (with-slots (label inst op1 op2) c2
    (when (and (null label)
               (or (and (inst= inst "add") (op= op2 "0"))
                   (and (inst= inst "sub") (op= op2 "0"))
                   (and (inst= inst "imul") (op= op2 "1"))))
      (values t nil))))

(defun meaningless-jmp-2 (c2 c3)
  (with-slots ((l2 label) (i2 inst) (op1-2 op1) (op2-2 op2)) c2
    (with-slots ((l3 label) (i3 inst) (op1-3 op1) (op2-3 op2)) c3
      (when (and (inst= i2 "jmp") (label= l3 op1-2))
        (values t (list c3))))))

(defun meaningless-jmp-3 (c1 c2 c3)
  (with-slots ((l1 label) (i1 inst) (op1-1 op1) (op2-1 op2)) c1
    (with-slots ((l2 label) (i2 inst) (op1-2 op1) (op2-2 op2)) c2
      (with-slots ((l3 label) (i3 inst) (op1-3 op1) (op2-3 op2)) c3
        (and (inst= i2 "jmp") (label= op1-1 l3) (null l2) l3
             (values t
                     (list (cond ((inst= i1 "je")
                                  (make-code :label l1 :inst "jne" :op1 op1-2))
                                 ((inst= i1 "jne")
                                  (make-code :label l1 :inst "je" :op1 op1-2))
                                 ((inst= i1 "jle")
                                  (make-code :label l1 :inst "jg" :op1 op1-2))
                                 ((inst= i1 "jge")
                                  (make-code :label l1 :inst "jl" :op1 op1-2))
                                 ((inst= i1 "jl")
                                  (make-code :label l1 :inst "jge" :op1 op1-2))
                                 ((inst= i1 "jg")
                                  (make-code :label l1 :inst "jle" :op1 op1-2))
                                 (t c2))
                           c3)))))))

(defun unexecutable-inst (c1 c2)
  (with-slots ((l1 label) (i1 inst) (op1-1 op1) (op2-1 op2)) c1
    (with-slots ((l2 label) (i2 inst) (op1-2 op1) (op2-2 op2)) c2
      (and (inst= i1 "jmp") (null l2)
           (values t (list c1))))))

(defun refp (op code)
  (with-slots (label inst op1 op2) code
    (or (and (or (inst= inst "add") (inst= inst "sub") (inst= inst "imul")
                 (inst= inst "cmp") (inst= inst "movzx"))
             (or (op= op1 op) (op= op2 op)))
        (and (or (inst= inst "idiv") (inst= inst "cdq"))
             (or (op= op "eax") (op= op "edx")))
        (and (inst= inst "mov") (op= op op2))
        (and (inst= inst "push") (op= op op1)))))

(defun overwrite-p (op code)
  (with-slots (label inst op1 op2) code
    (or (and (or (inst= inst "mov") (inst= inst "movzx") (inst= inst "pop"))
             (op= op op1))
        (and (or (inst= inst "idiv") (inst= inst "call"))
             (op= op "eax"))
        (and (inst= inst "cdq") (or (op= op "edx") (op= op "eax"))))))

(defun meaningless-mov-precedind-in-push (c1 c2 c3)
  (with-slots ((l1 label) (i1 inst) (op1-1 op1) (op2-1 op2)) c1
    (with-slots ((l2 label) (i2 inst) (op1-2 op1) (op2-2 op2)) c2
      (with-slots ((l3 label) (i3 inst) (op1-3 op1) (op2-3 op2)) c3
        (and (null l1) (null l2)
             (inst= i1 "mov")
             (inst= i2 "push")
             (overwrite-p op1-1 c3)
             (op= op1-1 op1-2)
             (values t (list (make-code :inst "push" :op1 op2-1) c3)))))))

(defun meaningless-compare-assign (c1 c2 c3 c4)
  (with-slots ((l1 label) (i1 inst) (op1-1 op1) (op2-1 op2)) c1
    (with-slots ((l2 label) (i2 inst) (op1-2 op1) (op2-2 op2)) c2
      (with-slots ((l3 label) (i3 inst) (op1-3 op1) (op2-3 op2)) c3
        (with-slots ((l4 label) (i4 inst) (op1-4 op1) (op2-4 op2)) c4
          (and (null l1) (null l2) (null l3) (null l4)
               (inst= i1 "mov") (inst= i2 "mov") (inst= i3 "mov") (inst= i4 "cmp")
               (op= op1-3 op1-4) (op= op1-1 op2-2) (op= op1-2 op2-4)
               (if (not (op= op2-1 op2-3))
                   (values t (list c3 (make-code :inst "cmp" :op1 op1-4 :op2 op2-1)))
                   (values t (list c3 c4)))))))))

(defun remove-duplicate-label (c1 c2)
  (with-slots ((l1 label) (i1 inst) (s1 subinst)) c1
    (with-slots ((l2 label) (i2 inst) (s2 subinst)) c2
      (if (and (null i1) (null i2) (null s1) (null s2) l1 l2)
          (destructuring-bind (removed survive)
              (if (char= #\L (elt (lbl-name l1) 0)) (list l1 l2) (list l2 l1))
            (push (list removed survive) *optimize-temp-0*)
            (values t (list (make-code :label survive))))))))

(defun replace-removed-label (now 2labels)
  (destructuring-bind (removed survive) 2labels
    (mapcar (lambda (code)
              (with-slots (label inst subinst op1 op2) code
                (cond ((label= op1 removed)
                       (make-code :label label :inst inst :subinst subinst
                                  :op1 survive :op2 op2))
                      ((label= op2 removed)
                       (make-code :label label :inst inst :subinst subinst
                                  :op1 removed :op2 survive))
                      (t code))))
            now)))

(defun all-positions-if (pred list)
  (do ((i 0 (1+ i))
       result)
      ((> i (1- (length list))) (sort result #'<))
    (aif (position-if pred list :start i)
         (setf result (adjoin it result)))))

(defun put-together-insts-preceding-in-jmp (now)
  (let* ((poss (all-positions-if
                (lambda (code)
                  (let ((i (code-inst code)))
                    (or (inst= i "jmp") (inst= i "je") (inst= i "jne")
                        (inst= i "jl") (inst= i "jle") (inst= i "jg")
                        (inst= i "jge"))))
                now)))
    (dolist (pos poss)
      (let* ((lpos (position-if (lambda (code)
                                  (label= (code-op1 (elt now pos))
                                          (code-label code)))
                                now))
             (next-j (elt now (1+ pos)))
             (next-l (elt now (1+ lpos))))
        (when (and (code= next-j next-l) (not (inst= (code-inst next-j) "cmp")))
          (setf (elt now (1+ pos)) (elt now pos)
                (elt now pos) next-j
                (elt now (1+ lpos)) (make-code :inst "")))))
    (remove-if (lambda (c) (inst= (code-inst c) "")) now)))

(defun remove-never-refered-label (now)
  (let* ((poss (all-positions-if (lambda (code) (code-label code)) now))
         rms)
    (dolist (pos poss)
      (with-slots ((l label) (i inst) (s subinst)) (elt now pos)
        (and (null i) (null s)
             (never-refered-p l now)
             (push l rms))))
    (remove-if (lambda (c) (find (code-label c) rms :test #'label=)) now)))

(defun never-refered-p (label now)
  (every (lambda (c) (not (label= label (code-op1 c)))) now))

(defun label-with-inst (c1 c2) ;; 極力最後に行うこと
  (with-slots ((l1 label) (i1 inst) (s1 subinst)) c1
    (with-slots ((l2 label) (i2 inst) (s2 subinst) op1 op2) c2
      (and (null i1) (null s1) l1 (null l2)
           (values t (list (make-code :label l1 :inst i2 :subinst s2
                                      :op1 op1 :op2 op2)))))))
;; optimization

(defun substitute-n (n pred list)
  (do ((result list)
       prevs
       (i 0 (1+ i))
       (lasti (- (length list) n) (- (length result) n)))
      ((or (< lasti i) (find result prevs :test #'equal)) result)
    (multiple-value-bind (p val) (apply pred (subseq result i (+ i n)))
      (if (< n (length val))
          (error "cannot replace to list longer than n=~A: ~A" n val)
          (when p
            (push result prevs)
            (setf result (append (subseq result 0 i) val (subseq result (+ i n)))
                  i 0))))))

(defun optimize-loop ()
  (let ((pre nil) (now *code*))
    (till (find now pre :test #'equal)
      (push now pre)
      (setf now (substitute-n 2 #'meaningless-mov-2 now))
      (setf now (substitute-n 1 #'meaningless-add-and-mul now))
      (setf now (substitute-n 2 #'meaningless-jmp-2 now))
      (setf now (substitute-n 3 #'meaningless-jmp-3 now))
      (setf now (substitute-n 1 #'replace-to-specific-inst now))
      (setf now (substitute-n 2 #'unexecutable-inst now))
      (setf now (substitute-n 3 #'meaningless-mov-precedind-in-push now))
      (setf now (substitute-n 4 #'meaningless-compare-assign now))
      (setf now (substitute-n 2 #'remove-duplicate-label now))
      (awhen (pop *optimize-temp-0*) (setf now (replace-removed-label now it)))
      (setf now (put-together-insts-preceding-in-jmp now))
      (setf now (remove-never-refered-label now)))
    ;;(setf now (substitute-n 2 #'label-with-inst now))
    ;;(format t "count:~A~%" (incf c))
    (setf *code* now)))
