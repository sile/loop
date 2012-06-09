(in-package :asdf)

(defsystem loop
  :name "loop"
  :version "0.0.1"
  :description "generic loop package"
  :author "Takeru Ohta"

  :serial t
  :components ((:file "package")
               (:file "generator")
               (:file "joint")
               (:file "terminal")))
