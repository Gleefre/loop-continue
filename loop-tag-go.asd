(defsystem "loop-tag-go"
  :description "Hack that adds TAG/GO keywords to the LOOP macro."
  :version "0.0.1"
  :author "Grolter <varedif.a.s@gmail.com>"
  :license "Apache 2.0"
  :pathname "hack"
  :components ((:file "package")
               (:file "sbcl"  :if-feature :sbcl)
               (:file "cmucl" :if-feature :cmucl)))
