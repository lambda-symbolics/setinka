(in-package #:setinka)

;;;; -- Conditions --

(define-condition setting-error (error)
  ((message
    :initarg       :message
    :reader        setting-error-message
    :type          string
    :documentation "A human-readable explanation of the failure."))
  (:report (lambda (condition stream)
             (write-string (setting-error-message condition) stream)))
  (:documentation "The parent of every condition setinka signals."))

(define-condition setting-unknown (setting-error)
  ((name
    :initarg       :name
    :reader        setting-unknown-name
    :type          symbol
    :documentation "The name no setting in the registry carries."))
  (:documentation "A setting was looked up by a name its registry does not know."))

(define-condition setting-invalid (setting-error)
  ((setting
    :initarg       :setting
    :reader        setting-invalid-setting
    :documentation "The setting that refused the value.")
   (value
    :initarg       :value
    :reader        setting-invalid-value
    :documentation "The refused value, as given or as coerced."))
  (:documentation "A value could not be coerced to, or is not acceptable for, a setting."))

(define-condition setting-read-only (setting-error)
  ((setting
    :initarg       :setting
    :reader        setting-read-only-setting
    :documentation "The derived setting that cannot be stored."))
  (:documentation "A value was stored into a derived setting."))

(define-condition configuration-missing (setting-error)
  ()
  (:documentation "CONFIG was called without a configuration while none is current."))
