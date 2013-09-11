;;;; cl-poker-2.asd

(asdf:defsystem #:cl-poker-2
  :serial t
  :description "Describe cl-poker-2 here"
  :author "Your Name <your.name@example.com>"
  :license "Specify license here"
  :depends-on (#:fiveam-test
	       #:cl-cards
               #:hand-evaluator)
  :components ((:file "package")
               (:file "cl-poker-2")))
