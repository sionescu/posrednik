;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(asdf:defsystem :middleman-tests
  :description "MiddleMan test suite"
  :author "Stelian Ionescu <sionescu@cddr.org>"
  :version #.(with-open-file (f (merge-pathnames "version.lisp-expr"
                                                 (or *compile-file-pathname*
                                                     *load-truename*)))
               (read f))
  :licence "GPL-3"
  :depends-on (:fiveam :middleman :iolib.pathnames)
  :components
  ((:file "pkgdcl")))

(defmethod perform ((o test-op)
                    (c (eql (find-system :middleman-tests))))
  (operate 'load-op :middleman-tests)
  (funcall (intern (symbol-name '#:run!) '#:5am) :middleman))

(defmethod operation-done-p ((o test-op)
                             (c (eql (find-system :middleman-tests))))
  nil)
