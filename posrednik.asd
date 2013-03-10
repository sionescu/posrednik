;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-

(unless (or #+asdf3 (asdf/driver:version<= "2.31.1" (asdf-version)))
  (error "You need ASDF >= 2.31.1 to load this system correctly."))

(asdf:defsystem :posrednik
  :description "Network connection manager"
  :author "Stelian Ionescu <sionescu@cddr.org>"
  :version (:read-file-form "version.lisp-expr")
  :licence "GPL-3"
  :depends-on (:iolib/base :cffi :iolib/syscalls :iolib/os :cl-ppcre)
  :pathname "src/"
  :components ((:file "pkgdcl")
               (:file "config" :depends-on ("pkgdcl"))
               (:file "interfaces" :depends-on ("pkgdcl" "config"))))

(defmethod asdf:perform ((o asdf:test-op)
                         (c (eql (asdf:find-system :posrednik))))
  (asdf:load-system :posrednik/tests)
  (asdf/package:symbol-call :5am :run! :posrednik))

(asdf:defsystem :posrednik/tests
  :description "Posrednik test suite"
  :author "Stelian Ionescu <sionescu@cddr.org>"
  :version (:read-file-form "version.lisp-expr")
  :licence "GPL-3"
  :depends-on (:fiveam :posrednik :iolib/pathnames)
  :components
  ((:file "pkgdcl")))
