;;;; cl-chip8.lisp

(in-package #:cl-chip8)

(defparameter *opcode* nil)

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

(defun push-stack (x stack)
  (push x stack)
  stack)

(defun pop-stack (stack)
  (let ((top (first stack)))
    (pop stack)
    (values top stack)))

;;; Stack Pointer
;; I dont think we need one?
;;(defparameter *SP* 0)

;;; Key Input (HEX based keypad 0 - F)
;;; TODO: 16 unsigned chars

(defmacro opcode-match (str opcode)
  "Generates forms to compare all noted positions in the provided opcode via str. Ex: str = F_65 generates 3 comparisons against opcode."
  `(and
    ,@(loop
         for i from 0 below (length str)
         for ch = (char str i)
         unless (eq ch #\_) ;; Ignore _'s
         collect `(eql ,ch (char ,opcode ,i)))))

;;; Doesn't work yet....
;; (defmacro with-opcode (opcode &body body)
;;   "Generates a temporary variable for each position in the opcode."
;;   `(let
;;        (,@(loop
;;              for i from 0 below (length opcode)
;;              collect `(p,i (aref opcode ,i))))
;;      ,@body))


(defun read-opcode (location collection)
  "Each opcode is 2 bytes long."
  (dec->hex
   (logior
    (ash (aref collection location) 8)
    (aref collection (1+ location)))))

(defmacro cond-opcode (opcode cond-pairs)
  "Creates a cond environment. Choices are based on opcode statements."
  (cond
    ((null cond-pairs) nil)
    ((atom cond-pairs) (error "cond-opcode2: bad syntax!"))
    (t
     `(if (opcode-match
             ,(first (first cond-pairs))
             ,opcode)
            (progn ,@(rest (first  cond-pairs)))
            (cond-opcode ,opcode ,(rest cond-pairs))))))


(defun dec->hex (value)
  (format nil "~4,'0X" value))

(defun hex->dec (value)
  (values (parse-integer value :radix 16 :junk-allowed t)))

(defun init ()
  "Initialize all registers & mem to zero."
  (setf *PC* 0)
  (setf *I* 0)
  (setf *Stack* '()))

(defun cycle ()
  (fetch)
  (if (not (decode))
      (progn
        (execute)
        (store))))

(defun main ()
  (init)
  (loop
     do (block test
          (fetch)
          (if (decode)
              (return-from test))
          (execute)
          (store))))


;;; TODO: Might need more logic for modifying the program counter
;;;; What if there's an issue reading the opcode from mem? etc.
(defun fetch ()
  "Build the next opcode and increment the program counter."
  (setf opcode (read-opcode *PC* *Memory*))
  (incf *PC* 2))


;;; TODO: Early Exit if control flow is interrupted.
(defun decode ()
  "Get any memory related things ready or if we can dip out early (return..jump....call..?) Early Exit = T, Continue = Nil"
  (cond-opcode *opcode*
   ;;; Control Flow and early exits if possible
   (("00E0"
    (format t "Clearing the screen.")
    t)
   ("00EE"
    (format t "Returning from subroutine.")
    (setf *Stack* (pop-stack *Stack*))
    (incf *PC* 2)
    t)
   ("1___"
    (format t "Jumping to an address....")
    (setf *PC* (parse-integer (subseq *opcode* 1) :radix 16))
    t)
   ("2___" ;; Call Subroutine at NNN
    (format t "Calling subroutine.")
    (setf *Stack* (push-stack *PC* *Stack*))
    (setf *PC* (parse-integer (subseq *opcode* 1) :radix 16))
    t)
   ("F_65"
    (format t "LOAD Called V0 - Vx with values, starting at I, from memory.")
    (loop
       with x = (digit-char-p (char *opcode* 1) 16)
       for i from 0 below x
       for j = *I*
       do (setf (aref *V* i) (aref *Memory* j)))
   t
   ))))

(defun execute ()
  "Do stuff based on the opcode"
  (cond-opcode *opcode*
    ;;; CONDITIONS
   (("3___" ;; Skip if (Vx == NN)
    (if (=
         (aref *V* (hex->dec (subseq *opcode* 1 2)))
         (parse-integer (subseq *opcode* 2) :radix 16))
        (incf *PC* 2)))
   ("4___" ;; Skip if (Vx != NN)
    (if (not
         (=
          (aref *V* (hex->dec (subseq *opcode* 1 2)))
          (parse-integer (subseq *opcode* 2) :radix 16)))
        (incf *PC* 2)))
   ("5__0" ;; Skip if (Vx == Vy)
    (if (not
         (=
          (aref *V* (hex->dec (subseq *opcode* 1 2)))
          (aref *V* (hex->dec (subseq *opcode* 2 3)))))
        (incf *PC* 2)))
   ("9__0" ;; if (Vx != Vy)
    (if (not
         (=
          (parse-integer (subseq *opcode* 2) :radix 16)
          (parse-integer (subseq *opcode* 3) :radix 16)))
        (incf *PC* 2)))
   
    ;;; CONSTANTS
   ("6___" ;; Vx = NN
    (setf
     (aref *V* (hex->dec (subseq *opcode* 1 2)))
     (subseq *opcode* 2)))

   ;;; ASSIGNMENT
   ("8__0" ;; Vx = Vy
    (setf
     (aref *V* (hex->dec (subseq *opcode* 1 2)))
     (aref *V* (hex->dec (subseq *opcode* 2 3)))
     ))

   ;;; MATH
   ("8__1" ;; Vx = Vx | Vy
    (setf
     (aref *V* (hex->dec (subseq *opcode* 1 2)))
     (logior
      (aref *V* (hex->dec (subseq *opcode* 1 2)))
      (aref *V* (hex->dec (subseq *opcode* 2 3))))))
   ("8__2" ;; Vx = Vx & Vy
    (setf
     (aref *V* (hex->dec (subseq *opcode* 1 2)))
     (logand
      (aref *V* (hex->dec (subseq *opcode* 1 2)))
      (aref *V* (hex->dec (subseq *opcode* 2 3))))))
   ("8__3" ;; Vx = Vx ^ Vy
    (setf
     (aref *V* (hex->dec (subseq *opcode* 1 2)))
     (logxor
      (aref *V* (hex->dec (subseq *opcode* 1 2)))
      (aref *V* (hex->dec (subseq *opcode* 2 3))))))
   ("8__4" ;; Vx += Vy (TODO: VF = 1 on carry, 0 if not)
    (setf
     (aref *V* (hex->dec (subseq *opcode* 1 2)))
     (+
      (aref *V* (hex->dec (subseq *opcode* 1 2)))
      (aref *V* (hex->dec (subseq *opcode* 2 3))))))
   ("8__5" ;; Vx -= Vy (TODO: VF = 0 when borrow, 1 if not)
    (setf
     (aref *V* (hex->dec (subseq *opcode* 1 2)))
     (-
      (aref *V* (hex->dec (subseq *opcode* 1 2)))
      (aref *V* (hex->dec (subseq *opcode* 2 3))))))
   )))

(defun store ()
  (cond-opcode *opcode*
               (("F_55"
                 (format t "Store V0 to VX in mem, starting at I. Offset from I increased by 1 for each value written, but I itself is unmodified."
  )))))

