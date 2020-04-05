(defpackage :lovetris-asd
  (:use :cl :asdf))

(in-package :lovetris-asd)

(defsystem lovetris
  :license "MIT"
  :author "Kevin Galligan"
  :depends-on ()
  :pathname "src"
  :serial t
  :components ((:file "package")
               (:file "hatetris")))
