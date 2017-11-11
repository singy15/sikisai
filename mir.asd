
(in-package :cl-user)
(defpackage mir-asd
  (:use :cl :asdf))
(in-package :mir-asd)

(defsystem mir
  :depends-on (:cl-opengl :cl-glut :cl-glu)
  :components (
    (:module "src"
      :components (
        (:file "mir")))))

