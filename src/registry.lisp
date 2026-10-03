(in-package #:setinka)

;;;; -- Setting Registries --

(-> make-setting-registry () setting-registry)
(defun make-setting-registry ()
  "Return an empty setting registry."
  (make-instance 'setting-registry))

(defvar *settings* (make-setting-registry)
  "The registry DEFINE-SETTING uses when no other is named.

An application may keep its settings here. A library should define its own
registry so its setting names cannot collide with an application's.")

(-> register-setting (setting &optional setting-registry) setting)
(defun register-setting (setting &optional (registry *settings*))
  "Record SETTING in REGISTRY, replacing an earlier definition of its name in place."
  (let ((name (setting-name setting)))
    (with-lock-held ((setting-registry-lock registry))
      (unless (nth-value 1 (gethash name (setting-registry-table registry)))
        (vector-push-extend name (setting-registry-order registry)))
      (setf (gethash name (setting-registry-table registry)) setting)))
  setting)

(-> find-setting (symbol &optional setting-registry) setting)
(defun find-setting (name &optional (registry *settings*))
  "Return the setting named NAME in REGISTRY, signaling SETTING-UNKNOWN when absent."
  (or (with-lock-held ((setting-registry-lock registry))
        (values (gethash name (setting-registry-table registry))))
      (error 'setting-unknown
             :name name
             :message (format nil "Unknown setting ~S." name))))

(-> settings-list (&optional setting-registry) list)
(defun settings-list (&optional (registry *settings*))
  "Return the settings in REGISTRY in definition order."
  (with-lock-held ((setting-registry-lock registry))
    (loop for name across (setting-registry-order registry)
          collect (gethash name (setting-registry-table registry)))))

(defmacro define-setting (name (class &key (registry '*settings*)) &rest initargs)
  "Define and register the setting NAME as an instance of CLASS with INITARGS.

NAME is a keyword. REGISTRY is evaluated and defaults to *SETTINGS*. The
expansion evaluates INITARGS when it runs, so option, default, and validator
functions may be quoted symbols or lambda forms. Redefinition replaces the
setting and keeps its position."
  (check-type name keyword)
  `(register-setting (make-instance ',class :name ,name ,@initargs) ,registry))
