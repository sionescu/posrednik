;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(asdf:defsystem :middleman
  :description "Network connection manager"
  :author "Stelian Ionescu <sionescu@cddr.org>"
  :version #.(with-open-file (f (merge-pathnames "version.lisp-expr"
                                                 (or *compile-file-pathname*
                                                     *load-truename*)))
               (read f))
  :licence "GPL-3"
  :depends-on (:iolib.base :iolib.syscalls :iolib.os :cffi)
  :pathname "src/"
  :components ((:file "pkgdcl")))

(defmethod perform ((o test-op) (c (eql (find-system :middleman))))
  (oos 'test-op :middleman-tests))

(defmethod operation-done-p ((o test-op) (c (eql (find-system :middleman))))
  nil)
