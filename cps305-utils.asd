;;;; cps305-utils.asd

(asdf:defsystem #:cps305-utils
  :description "Basic programming utilities for CPS305 - Data Structures"
  :author "Marcus Santos <m3santos@ryerson.ca>"
  :license  "Specify license here"
  :version "0.0.1"
  :serial t
  :depends-on (#:rutils)
  :components ((:file "package")
               (:file "cps305-utils")))
