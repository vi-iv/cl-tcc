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

;;; Lexer and Parser

(in-package :cl-tcc)

;; lexer using "lispbuilder-lexer"

(lispbuilder-lexer:deflexer tc-lexer
  ("[:space:]+")

  ("int"       (return (values :int)))
  ("if"        (return (values :if)))
  ("else"      (return (values :else)))
  ("while"     (return (values :while)))
  ("return"    (return (values :return)))

  ("[0-9]+"
   (return (values :constant
                   (lispbuilder-lexer:int lispbuilder-lexer:%0))))

  ("[a-zA-Z][a-zA-Z0-9_]*"
   (return (values :identifier lispbuilder-lexer:%0)))

  ("\\&\\&"    (return (values :and-assign)))
  ("\\|\\|"    (return (values :or-assign)))
  ("\<\\="     (return (values :le-assign)))
  ("\>\\="     (return (values :ge-assign)))
  ("\\!\\="    (return (values :ne-assign)))
  ("\\=\\="    (return (values :eq-assign)))
  ("\\="       (return (values :=)))
  ("\\+"       (return (values :+)))
  ("\\-"       (return (values :-)))
  ("\\*"       (return (values :*)))
  ("\\/"       (return (values :/)))
  ("\\%"       (return (values :%)))
  ("\<"        (return (values :<)))
  ("\>"        (return (values :>)))
  ("\\("       (return (values :|(|)))
  ("\\)"       (return (values :|)|)))
  ("\\{"       (return (values :|{|)))
  ("\\}"       (return (values :|}|)))
  ("\\,"       (return (values :|,|)))
  ("\\;"       (return (values :|;|))))

;; data structure

(defvar *token-table* nil)

(defvar *top-alloc* 0)
(defvar *current-level* 0)
(defvar *parameters-quantity* 0)

(defclass node () ())

(defclass const (node)
  ((value :accessor value :initarg :value)))

(defclass token (node)
  ((name :accessor name :initarg :name)
   (kind :accessor kind :initarg :kind :initform :fresh)
   (level :accessor level :initarg :level :initform 0)
   (offset :accessor offset :initarg :offset :initform 0)
   (label :accessor label :initarg :label :initform nil)))

(defclass tuple (node)
  ((op :accessor op :initarg :op)
   (children :accessor children :initarg :children)))

(defun token-p (node) (eq (class-of node) (class-of (make-instance 'token))))
(defun tuple-p (node) (eq (class-of node) (class-of (make-instance 'tuple))))
(defun const-p (node) (eq (class-of node) (class-of (make-instance 'const))))

;; parsing tools

(defun make-constant-node (value)
  (make-instance 'const :value (mkstr value)))

(defun make-token-node (name &key (kind :fresh) (level 0) (offset 0))
  (car (push (make-instance 'token :name name :kind kind :level level :offset offset)
             *token-table*)))

(defun make-tuple (op &rest children)
  (make-instance 'tuple :op op :children children))

(defun lookup-token (name)
  (find name *token-table* :key #'name :test #'string=))

(defun pop-tokens ()
  (labels ((rec (table)
             (awhen (car table)
               (cond ((and (<= (level it) *current-level*)
                           (not (eq (kind it) :fun))
                           (not (eq (kind it) :undeffun)))
                      table)
                     ((and (> (level it) *current-level*)
                           (not (eq (kind it) :fun))
                           (not (eq (kind it) :undeffun)))
                      (rec (cdr table)))
                     (t (cons it (rec (cdr table))))))))
    (setf *token-table* (rec *token-table*))))

(defun push-token (name)
  (aif (lookup-token name)
       it
       (make-token-node name :level *current-level*)))

(defgeneric quantity (node)
  (:documentation ""))

(defmethod quantity ((node (eql nil))) 0)

(defmethod quantity ((const const)) 0)

(defmethod quantity ((token token)) 0)

(defmethod quantity ((tuple tuple))
  (with-slots (op children) tuple
    (if (or (eq op :args) (eq op :parms))
        (1+ (quantity (nth 1 children)))
        (quantity (nth 1 children)))))

(defgeneric make-decl (node)
  (:documentation ""))

(defmethod make-decl ((node node))
  node)

(defmethod make-decl ((token token))
  (with-slots (name kind level) token
    (let ((n (case kind
               (:var
                (when (= level *current-level*)
                  (warn "redeclaration of '~A'" name))
                (make-token-node name :level *current-level*))
               ((:fun :undeffun)
                (when (= level *current-level*)
                  (warn "'~A' redeclared as different kind of symbol" name))
                (make-token-node name :level *current-level*))
               (:parm
                (warn "declaration of '~A' shadows a parameter" name)
                (make-token-node name :level *current-level*))
               (:fresh token)
               (otherwise token))))
      (setf (kind n) :var)
      n)))

(defgeneric make-parm-decl (node)
  (:documentation ""))

(defmethod make-parm-decl ((node node))
  node)

(defmethod make-parm-decl ((token token))
  (with-slots (name kind level offset) token
    (let ((n (case kind
               ((:var :fun :undeffun)
                (make-token-node name :level *current-level*))
               (:parm
                (warn "redeclaration of '~A'" name)
                (make-token-node name :level *current-level*))
               (:fresh token)
               (otherwise token))))
      (with-slots (kind offset) n
        (setf kind :parm)
        (setf offset (* 4 (1+ *parameters-quantity*)))
        n))))

(defgeneric make-fun-def (node)
  (:documentation ""))

(defmethod make-fun-def ((node node))
  node)

(defmethod make-fun-def ((token token))
  (with-slots (name kind level offset) token
    (let ((n (case kind
               (:var (warn "'~A' redeclared as different kind of symbol" name) token)
               (:fun (warn "redefinition of '~A'" name) token)
               (:undeffun
                (setf kind :fun)
                (compare-quantity-arguments token *parameters-quantity*))
               (:fresh (setf kind :fun) token)
               (otherwise token))))
      (setf offset *parameters-quantity*)
      (setf level 0)
      n)))

(defgeneric ref-var (node)
  (:documentation ""))

(defmethod ref-var ((node node))
  node)

(defmethod ref-var ((token token))
  (with-slots (name kind level) token
    (case kind
      ((:var :parm) token)
      ((:fun :undeffun) (warn "function '~A' is used as variable" name) token)
      (:fresh (warn "'~A' undeclared variable" name) (setf kind :var) token)
      (otherwise token))))

(defgeneric ref-fun (node &key)
  (:documentation ""))

(defmethod ref-fun ((node node) &key)
  node)

(defmethod ref-fun ((token token) &key quantity)
  (with-slots (name kind level offset) token
    (case kind
      ((:var :parm) (warn "variable '~A' is used as function" name) token)
      ((:fun :undeffun) (compare-quantity-arguments token quantity))
      (:fresh
       (warn "'~A' undeclared function" name)
       (setf kind :undeffun)
       (setf offset quantity)
       (when (> level 0) (globalize-node token))
       token)
      (otherwise token))))

(defun globalize-node (node)
  (setf *token-table*
        (append1 (remove node *token-table*) node)))

(defun compare-quantity-arguments (token current)
  (with-slots (name offset) token
    (cond ((< current offset)
           (error "too few arguments ~A to function '~A', required ~A"
                 current name offset))
          ((> current offset)
           (error "too many arguments ~A to function '~A', required ~A"
                 current name offset))
          (t nil))
    token))

(defun compare-quantity-parameters (token current)
  (with-slots (name offset) token
    (cond ((< current offset)
           (error "too few parameters ~A to function '~A', required ~A"
                 current name offset))
          ((> current offset)
           (error "too many parameters ~A to function '~A', required ~A"
                 current name offset))
          (t nil))
    token))

(let ((last-alloc 0))
  (defun allocate-loc ()
    (decf last-alloc 4)
    (when (< last-alloc *top-alloc*)
      (setf *top-alloc* last-alloc))
    last-alloc)

  (defun release-loc ()
    (incf last-alloc 4))

  (defun reset-last-alloc ()
    (setf last-alloc 0)))

;; parser using "yacc"

(yacc:define-parser tc-parser
  (:muffle-conflicts (1 0))
  (:start-symbol main)
  (:terminals (:constant :identifier :constant
               :int :if :else :while :return :and-assign :or-assign
               :ge-assign :le-assign :eq-assign :ne-assign
               :cons :fundef :pstfix :cmpd-stm :parms :vars :args
               :+ :- :* :/ :% :< :> := :|(| :|)| :|{| :|}| :|,| :|;|))
  ;; (:precedence ((:left :* :/ :%) (:left :+ :-) (:left :< :le-assign :> :ge-assign)
  ;;               (:left :eq-assign :ne-assign) (:left :and-assign :or-assign)
  ;;               (:right :=) (:left :|,|)))

  (main
   (program
    (lambda ($1)
      `(let ((p ,$1))
         (initialize-global-variables)
         (emit-program p)))))

  (program
   (external-declaration
    (lambda ($1) $1))
   (program external-declaration
    (lambda ($1 $2) `(make-tuple :cons ,$1 ,$2))))

  (external-declaration
   (declaration
    (lambda ($1) $1))
   (function-definition
    (lambda ($1) $1)))

  (declaration
   (:int declarator-list :|;|
    (lambda ($1 $2 $3)
      (declare (ignorable $1 $3))
      `(make-tuple :int ,$2))))

  (declarator-list
   (declarator
    (lambda ($1) `(make-decl ,$1)))
   (declarator-list :|,| declarator
    (lambda ($1 $2 $3)
      (declare (ignorable $2))
      `(make-tuple :vars ,$1 (make-decl ,$3)))))

  (declarator
   (:identifier
    (lambda ($1) `(push-token ,$1))))

  (function-definition
   (:int declarator :|(| :|)| compound-statement
    (lambda ($1 $2 $3 $4 $5)
      (declare (ignorable $1 $3 $4))
      `(progn
         (setf *parameters-quantity* 0)
         (let ((n (make-fun-def ,$2)))
           (incf *current-level*)
           (let ((tp (make-tuple :fundef (make-tuple :int n) nil ,$5)))
             (decf *current-level*)
             (pop-tokens)
             tp)))))
   (:int declarator :|(| parameter-type-list :|)| compound-statement
    (lambda ($1 $2 $3 $4 $5 $6)
      (declare (ignorable $1 $3 $5))
      `(progn
         (setf *parameters-quantity* 0)
         (incf *current-level*)
         (let ((parms ,$4)
               (n (make-fun-def ,$2)))
           (let ((tp (make-tuple :fundef (make-tuple :int n) parms ,$6)))
             (decf *current-level*)
             (pop-tokens)
             tp))))))

  (parameter-type-list
   (parameter-declaration
    (lambda ($1) $1))
   (parameter-type-list :|,| parameter-declaration
    (lambda ($1 $2 $3)
      (declare (ignorable $2))
      `(make-tuple :var ,$1 ,$3))))

  (parameter-declaration
   (:int declarator
    (lambda ($1 $2)
      (declare (ignorable $1))
      `(progn
         (incf *parameters-quantity*)
         (make-tuple :int (make-parm-decl ,$2))))))

  (statement
   :|;|
   (expression :|;|
    (lambda ($1 $2)
      (declare (ignorable $2))
      $1))
   (compound-statement
    (lambda ($1) $1))
   (:if :|(| expression :|)| statement
    (lambda ($1 $2 $3 $4 $5)
      (declare (ignorable $1 $2 $4))
      `(make-tuple :if ,$3 ,$5)))
   (:if :|(| expression :|)| statement :else statement
    (lambda ($1 $2 $3 $4 $5 $6 $7)
      (declare (ignorable $1 $2 $4 $6))
      `(make-tuple :if ,$3 ,$5 ,$7)))
   (:while :|(| expression :|)| statement
    (lambda ($1 $2 $3 $4 $5)
      (declare (ignorable $1 $2 $4))
      `(make-tuple :while ,$3 ,$5)))
   (:return :|;|
    (lambda ($1 $2)
      (declare (ignorable $1 $2))
      `(make-tuple :return)))
   (:return expression :|;|
    (lambda ($1 $2 $3)
      (declare (ignorable $1 $3))
      `(make-tuple :return ,$2))))

  (compound-statement
   (:|{| :|}|
    (lambda ($1 $2)
      (declare (ignorable $1 $2)) nil))
   (:|{| statement-list :|}|
    (lambda ($1 $2 $3)
      (declare (ignorable $1 $3))
      `(progn
         (incf *current-level*)
         (let ((tp (make-tuple :cmpd-stm nil ,$2)))
           (decf *current-level*)
           tp))))
   (:|{| declaration-list :|}|
    (lambda ($1 $2 $3)
     (declare (ignorable $1 $3))
      `(progn
         (incf *current-level*)
         (let ((tp (make-tuple :cmpd-stm ,$2)))
           (decf *current-level*)
           tp))))
   (:|{| declaration-list statement-list :|}|
    (lambda ($1 $2 $3 $4)
      (declare (ignorable $1 $4))
      `(progn
         (incf *current-level*)
         (let ((tp (make-tuple :cmpd-stm ,$2 ,$3)))
           (decf *current-level*)
           tp)))))

  (declaration-list
   (declaration
    (lambda ($1) $1))
   (declaration-list declaration
    (lambda ($1 $2) `(make-tuple :cons ,$1 ,$2))))

  (statement-list
   (statement
    (lambda ($1) $1))
   (statement-list statement
    (lambda ($1 $2) `(make-tuple :cons ,$1 ,$2))))

  (expression
   (assign-expr
    (lambda ($1) $1))
   (expression :|,| assign-expr
    (lambda ($1 $2 $3)
      (declare (ignorable $2))
      `(make-tuple :cons ,$1 ,$3))))

  (assign-expr
   (logical-OR-expr
    (lambda ($1) $1))
   (:identifier := assign-expr
    (lambda ($1 $2 $3)
      (declare (ignorable $2))
      `(make-tuple := (ref-var (push-token ,$1)) ,$3))))

  (logical-OR-expr
   (logical-AND-expr
    (lambda ($1) $1))
   (logical-OR-expr :or-assign logical-AND-expr
    (lambda ($1 $2 $3)
      (declare (ignorable $2))
      `(make-tuple :or-assign ,$1 ,$3))))

  (logical-AND-expr
   (equality-expr
    (lambda ($1) $1))
   (logical-AND-expr :and-assign equality-expr
    (lambda ($1 $2 $3)
      (declare (ignorable $2))
      `(make-tuple :and-assign ,$1 ,$3))))

  (equality-expr
   (relational-expr
    (lambda ($1) $1))
   (equality-expr :eq-assign relational-expr
    (lambda ($1 $2 $3)
      (declare (ignorable $2))
      `(make-tuple :eq-assign ,$1 ,$3)))
   (equality-expr :ne-assign relational-expr
    (lambda ($1 $2 $3)
      (declare (ignorable $2))
      `(make-tuple :ne-assign ,$1 ,$3))))

  (relational-expr
   (add-expr
    (lambda ($1) $1))
   (relational-expr :< add-expr
    (lambda ($1 $2 $3)
      (declare (ignorable $2))
      `(make-tuple :< ,$1 ,$3)))
   (relational-expr :> add-expr
    (lambda ($1 $2 $3)
      (declare (ignorable $2))
      `(make-tuple :> ,$1 ,$3)))
   (relational-expr :le-assign add-expr
    (lambda ($1 $2 $3)
      (declare (ignorable $2))
      `(make-tuple :le-assign ,$1 ,$3)))
   (relational-expr :ge-assign add-expr
    (lambda ($1 $2 $3)
      (declare (ignorable $2))
      `(make-tuple :ge-assign ,$1 ,$3))))

  (add-expr
   (mult-expr
    (lambda ($1) $1))
   (add-expr :+ mult-expr
    (lambda ($1 $2 $3)
      (declare (ignorable $2))
      `(make-tuple :+ ,$1 ,$3)))
   (add-expr :- mult-expr
    (lambda ($1 $2 $3)
      (declare (ignorable $2))
      `(make-tuple :- ,$1 ,$3))))

  (mult-expr
   (unary-expr
    (lambda ($1) $1))
   (mult-expr :* unary-expr
    (lambda ($1 $2 $3)
      (declare (ignorable $2))
      `(make-tuple :* ,$1 ,$3)))
   (mult-expr :/ unary-expr
    (lambda ($1 $2 $3)
      (declare (ignorable $2))
      `(make-tuple :/ ,$1 ,$3)))
   (mult-expr :% unary-expr
    (lambda ($1 $2 $3)
      (declare (ignorable $2))
      `(make-tuple :% ,$1 ,$3))))

  (unary-expr
   (postfix-expr
    (lambda ($1) $1))
   (:- unary-expr
    (lambda ($1 $2)
      (declare (ignorable $1))
      `(make-tuple :- (make-constant-node 0) ,$2))))

  (postfix-expr
   (primary-expr
    (lambda ($1) $1))
   (:identifier :|(| :|)|
    (lambda ($1 $2 $3)
      (declare (ignorable $2 $3))
      `(make-tuple :pstfix (ref-fun (push-token ,$1) :quantity 0))))
   (:identifier :|(| argument-expression-list :|)|
    (lambda ($1 $2 $3 $4)
      (declare (ignorable $2 $4))
      `(let* ((args ,$3)
              (qty (quantity args)))
         (make-tuple :pstfix (ref-fun (push-token ,$1) :quantity qty) args)))))

  (primary-expr
   (:identifier
    (lambda ($1) `(ref-var (push-token ,$1))))
   (:constant
    (lambda ($1) `(make-constant-node ,$1)))
   (:|(| expression :|)|
    (lambda ($1 $2 $3)
      (declare (ignorable $1 $3))
      $2)))

  (argument-expression-list
   (assign-expr
    (lambda ($1)
      `(make-tuple :args ,$1 nil)))
   (argument-expression-list :|,| assign-expr
    (lambda ($1 $2 $3)
      (declare (ignorable $2))
      `(make-tuple :args ,$3 ,$1)))))
