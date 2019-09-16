;;;; cl-chip8.lisp

(in-package #:cl-chip8)

(deftype short () `(integer -32767 32767))

(defvar opcode nil)

;; Memory
(defparameter *Memory* (make-array '(4096) :element-type 'byte))

;; Registers
(defparameter *V* (make-array '(16) :element-type 'byte))

;;; Special Registers
;;; TODO - Force these to 
(defparameter *I* 0)
(defparameter *PC* 0)

;;; GFX
(defparameter *GFX* (make-array `(,(* 64 32)) :element-type 'byte))

;;; Timers
;;; Count down @ 60 Hz when above 0. Buzzer goes off at 0.
(defparameter *Delay-Timer* 0)
(defparameter *Sound-Timer* 0)

;;; Stack
(defparameter *Stack* '())
;;; Stack Pointer
(defparameter *SP* 0)

;;; Key Input (HEX based keypad 0 - F)
;;; TODO: 16 unsigned chars

(defun dec->hex (value)
  (format nil "~4,'0X" value))

(defun hex->dec (value)
  (values (parse-integer value :radix 16 :junk-allowed t)))


(defun init ()
  "Initialize all registers & mem to zero."
  (setf *PC* 0)
  (setf *I* 0)
  )

(defun main ()
  (init)
  (fetch)
  (decode)
  (execute)
  )

(defun read-opcode (location collection)
  "Each opcode is 2 bytes long."
  (dec->hex
   (logior
    (ash (aref collection location) 8)
    (aref collection (1+ location)))))

;;; TODO: Might need more logic for modifying the program counter
;;;; What if there's an issue reading the opcode from mem? etc.
(defun fetch ()
  "Build the next opcode and increment the program counter."
  (setf opcode (read-opcode *PC* *Memory*))
  (incf *PC* 2))

(defmacro opcode-equal (str opcode)
  "Generates forms to compare all noted positions in the provided opcode via str. Ex: str = F_65 generates 3 comparisons against opcode."
  `(and
    ,@(loop
         for i from 0 below (length str)
         for ch = (char str i)
         unless (eq ch #\_) ;; Ignore _'s
         collect `(eql ,ch (char opcode ,i)))))

(defmacro with-opcode (opcode &body body)
  "Generates a temporary variable for each position in the opcode."
  `(let
       (,@(loop
             for i from 0 below (length opcode)
             collect `(p,i (aref opcode ,i))))
     ,@body))

(defmacro cond-opcode (opcode &rest cond-pairs)
  "Creates a cond environment. Choices are based on opcode statements."
  (cond
    ((null cond-pairs) nil)
    ((atom cond-pairs) (error "new-cond: bad syntax!"))
    (t `(if (opcode-equal
             ,(first (first cond-pairs))
             opcode)
            (progn ,@(rest (first cond-pairs)))
            (new-cond ,@(rest cond-pairs))))))

;;; TODO: Write a macro where we can query the value of the opcode... example "F_65" means compare positions 0, 2, 3 and we don't care what 1 is.
(defun decode ()
  "Get any memory related things ready or if we can dip out early (return..jump....call..?)"
  (cond-opcode opcode
    ;; Control Flow
    ("F_65"
     (format nil "LOAD Called V0 - Vx with values."))))

(defun execute ()
  "Do stuff based on the opcode"
  (cond-opcode opcode
    ;;; CONDITIONS
    ("3___" ;; Skip if (Vx == NN)
     (if (=
          (aref *V* 1)
          (parse-integer (subseq opcode 2) :radix 16))
         (incf *PC* 2)))
    ("4___" ;; Skip if (Vx != NN)
     (if (not
          (=
          (aref *V* 1)
          (parse-integer (subseq opcode 2) :radix 16)))
         (incf *PC* 2)))
    ("5__0" ;; Skip if (Vx == Vy)
     (if (=
          (aref *V* 1)
          (aref *V* 2))
         (incf *PC* 2)))
    ("9__0" ;; if (Vx != Vy)
     (if (not
          (=
           (parse-integer (subseq opcode 2) :radix 16)
           (parse-integer (subseq opcode 3) :radix 16)))
         (incf *PC* 2)))

    ;;; CONSTANTS
    ("6___" ;; Vx = NN
     (setf
      (aref *V* (hex->dec (aref opcode 1)))
      (subseq opcode 2)))
    ))
(defun store ()
  ())

