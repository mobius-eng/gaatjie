#|
  This file is a part of gaatjie project.
|#

(in-package :cl-user)
(defpackage gaatjie-test-asd
  (:use :cl :asdf))
(in-package :gaatjie-test-asd)

(defsystem gaatjie-test
  :author ""
  :license ""
  :depends-on (:gaatjie
               :prove)
  :components ((:module "t"
                :components
                ((:test-file "gaatjie"))))
  :description "Test system for gaatjie"

  :defsystem-depends-on (:prove-asdf)
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run-test-system) :prove-asdf) c)
                    (asdf:clear-system c)))
