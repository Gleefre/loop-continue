(asdf:defsystem "loop-continue"
  :description "Extension to the CL:LOOP macro: CONTINUE, TAG, GO."
  :version "0.1.0"
  :author "Grolter <varedif.a.s@gmail.com>"
  :license "Apache 2.0"
  :pathname "src"
  :components (#+(or sbcl cmucl ccl allegro clasp abcl ecl) (:file "mit-loop")
               #-(or sbcl cmucl ccl allegro clasp abcl ecl) (:file "fallback")))

(asdf:defsystem "loop-continue/enable"
  :description "Enables CONTINUE, TAG and GO in CL:LOOP."
  :depends-on ("loop-continue")
  :pathname "src"
  :components ((:file "enable")))

(asdf:defsystem "loop-continue/disable"
  :description "Disables CONTINUE, TAG and GO in CL:LOOP."
  :depends-on ("loop-continue")
  :pathname "src"
  :components ((:file "disable")))
