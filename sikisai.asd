
(in-package :cl-user)
(defpackage sikisai-asd
  (:use :cl :asdf))
(in-package :sikisai-asd)

(defsystem sikisai
  :depends-on (:cl-opengl :cl-glut :cl-glu)
  :components (
    (:module "src"
      :components (
        (:file "sikisai")))))

