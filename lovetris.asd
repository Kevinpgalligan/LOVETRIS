(defpackage :lovetris-asd
  (:use :cl :asdf))

(in-package :lovetris-asd)

(defsystem lovetris
  :license "MIT"
  :author "Kevin Galligan"
  :depends-on (:alexandria)
  :pathname "src"
  :serial t
  :components ((:file "package")
               (:file "hatetris")
               (:file "enemy-ai")
               (:file "game")))
