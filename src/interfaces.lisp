;;;; -*- Mode: Lisp; indent-tabs-mode: nil -*-
;;;
;;; --- Package definition.
;;;

(in-package :middleman)

(defparameter *ipcmd* "/sbin/ip")

(defclass interface ()
  ((id        :initarg :id        :reader interface.id)
   (name      :initarg :name      :reader interface.name)
   (flags     :initarg :flags     :reader interface.flags)
   (props     :initarg :props     :reader interface.props)
   (link      :initarg :link      :reader interface.link)
   (type      :initarg :type      :reader interface.type)
   (address   :initarg :address   :reader interface.address)))

(defmethod print-object (i s)
  (flet ((princ-prop-to-string (p)
           (format nil ":~A ~A" (string-downcase (car p)) (cdr p))))
    (print-unreadable-object (i s :type nil :identity nil)
      (format s "~A: ~A: <~A> ~A :link/~A/~A ~A"
              (interface.id i)
              (interface.name i)
              (join* #\, (mapcar #'string-upcase (interface.flags i)))
              (interface.address i)))))
              (join* #\Space (mapcar #'princ-prop-to-string (interface.props i)))
              (string-downcase (interface.link i))
              (string-downcase (interface.type i))

(defconstant (+ifaceinfo-regexp+ :test #'string=)
  "([0-9]): ([a-z0-9.]+): <([-_,A-Z]+)> ([ _a-zA-Z0-9]+)\\\\ +link/ether ([0-9a-f][:0-9a-f]+)")

(defun list-interfaces ()
  (multiple-value-bind (retcode stdout)
      (run-program (list *ipcmd* "--oneline" "link"))
    (unless (zerop retcode)
      (error "Cannot list interfaces: ~A ~A returned ~A"
             *ipcmd* "link" retcode))
    (let ((interfaces ()))
      (ppcre:do-register-groups (id name flags properties address)
          (+ifaceinfo-regexp+ stdout)
        (push (make-instance 'interface
                             :id id
                             :name name
                             :flags (parse-iflags flags)
                             :props (parse-iproplist properties)
                             :link :ethernet
                             :type (detect-iface-type name)
                             :address address)
              interfaces))
      (reverse interfaces))))

(defun parse-iflags (flags)
  (flet ((intern-flag (flag)
           (eswitch (flag :test #'string=)
             ("LOOPBACK"   :loopback)
             ("BROADCAST"  :broadcast)
             ("MULTICAST"  :multicast)
             ("UP"         :up)
             ("DOWN"       :down)
             ("LOWER_UP"   :lower-up)
             ("NO-CARRIER" :no-carrier)
             ("PROMISC"    :promiscuous)
             ("NOARP"      :noarp))))
    (mapcar #'intern-flag (split-sequence #\, flags))))

(defun parse-iproplist (properties)
  (loop :for (key val) :on (split-sequence #\Space properties) :by #'cddr
        :collect (check-iprop key val)))

(defun check-iprop (key val)
  (eswitch (key :test #'string=)
    ("mtu"
     (cons :mtu (parse-positive-int val)))
    ("qdisc"
     (cons :qdisc val))
    ("state"
     (cons :state (parse-interface-state val)))
    ("qlen"
     (cons :qlen (parse-positive-int val)))))

(defun parse-positive-int (val)
  (let ((val (parse-integer val)))
    (assert (plusp val))
    val))

(defun parse-interface-state (state)
  (eswitch (state :test #'string=)
    ("UNKNOWN" :unknown)
    ("UP"      :up)
    ("DOWN"    :down)
    ("DORMANT" :dormant)))

(defun detect-iface-type (name)
  (if (directory-exists-p (format nil "/sys/class/net/~A/wireless" name))
      :wifi
      :wired))
