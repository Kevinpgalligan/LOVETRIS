(defpackage :lovetris-asd
  (:use :cl :asdf))

(in-package :lovetris-asd)

(defsystem lovetris
  :license "MIT"
  :author "Kevin Galligan"
  :depends-on (:alexandria
               :trivial-gamekit
               :bordeaux-threads
               :vgplot)
  :pathname "src"
  :serial t
  :components ((:file "package")
               (:file "queue")
               (:file "hatetris")
               (:file "search")
               (:file "brute-search")
               (:file "play")
               (:file "genetic")
               (:file "train")))
