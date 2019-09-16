;;;; cl-chip8.asd

(asdf:defsystem #:cl-chip8
  :description "Describe cl-chip8 here"
  :author "Jason Dempsey"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:sdl2)
  :components ((:file "package")
               (:file "cl-chip8")))
