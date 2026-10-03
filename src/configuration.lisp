(in-package #:setinka)

;;;; -- Configuration Protocol --

(defgeneric configuration-set (configuration setting value &key source)
  (:documentation
   "Coerce, validate, and store VALUE for SETTING in CONFIGURATION.

SOURCE records how the value was chosen. A durable setting stored with the
:SESSION source is written to the configuration's store. Listeners are told
about a change, then every setting depending on SETTING is reconciled.
Returns the stored value."))


;;;; -- Current Configuration --

(defvar *configuration* nil
  "The configuration CONFIG reads and writes when no instance is given.

While it is NIL, CONFIG falls back to *CONFIGURATION-DEFAULT-FUNCTION*.")

(defvar *configuration-default-function* nil
  "A function of no arguments returning the running program's configuration, or NIL.

An application installs it so code that never binds *CONFIGURATION* still
reaches the live configuration.")

(-> configuration-current () (or null configuration))
(defun configuration-current ()
  "Return the bound *CONFIGURATION*, else the program's default configuration."
  (or *configuration*
      (and *configuration-default-function*
           (funcall *configuration-default-function*))))

(defmacro with-configuration ((configuration) &body body)
  "Evaluate BODY with CONFIGURATION as the current configuration for CONFIG.

CONFIGURATION is evaluated once, before BODY."
  `(let ((*configuration* ,configuration))
     ,@body))


;;;; -- Reading Values --

(-> configuration-setting (configuration symbol) setting)
(defun configuration-setting (configuration name)
  "Return the setting NAME known to CONFIGURATION."
  (find-setting name (configuration-registry configuration)))

(-> configuration-setting-list (configuration) list)
(defun configuration-setting-list (configuration)
  "Return CONFIGURATION's settings in definition order."
  (settings-list (configuration-registry configuration)))

(-> configuration-stored-value (configuration setting) (values t boolean))
(defun configuration-stored-value (configuration setting)
  "Return SETTING's stored value in CONFIGURATION and whether one is stored.

Return two values: the stored value or NIL, and true when a value is stored
rather than left to the default."
  (with-lock-held ((configuration-lock configuration))
    (gethash (setting-name setting) (configuration-values configuration))))

(-> configuration-setting-value (configuration setting) t)
(defun configuration-setting-value (configuration setting)
  "Return SETTING's current value in CONFIGURATION, computing derived and default values."
  (if (typep setting 'derived-setting)
      (funcall (setting-function setting) configuration)
      (multiple-value-bind (value present-p)
          (configuration-stored-value configuration setting)
        (if present-p
            value
            (setting-default-value setting configuration)))))

(-> configuration-group-values (configuration keyword) list)
(defun configuration-group-values (configuration group)
  "Return CONFIGURATION's values of GROUP's stored settings as a (name value ...) plist."
  (loop for setting in (configuration-setting-list configuration)
        when (and (eq (setting-group setting) group)
                  (not (eq (setting-scope setting) ':derived)))
          append (list (setting-name setting)
                       (configuration-setting-value configuration setting))))

(-> configuration-setting-source (configuration symbol) (or null configuration-source))
(defun configuration-setting-source (configuration name)
  "Return where CONFIGURATION's value for NAME came from, or NIL for a default."
  (with-lock-held ((configuration-lock configuration))
    (values (gethash name (configuration-sources configuration)))))

(-> config (symbol &optional (or null configuration)) t)
(defun config (name &optional (configuration (configuration-current)))
  "Return the value of setting NAME in CONFIGURATION, by default the current one."
  (let ((configuration (configuration--required configuration)))
    (configuration-setting-value
     configuration (configuration-setting configuration name))))


;;;; -- Changing Values --

(-> (setf config) (t symbol &optional (or null configuration)) t)
(defun (setf config) (value name &optional (configuration (configuration-current)))
  "Store VALUE as setting NAME in CONFIGURATION after coercion and validation."
  (let ((configuration (configuration--required configuration)))
    (configuration-set configuration (configuration-setting configuration name) value
                       :source ':session)))

(defmethod configuration-set
    ((configuration configuration) (setting setting) value &key (source ':session))
  "Store the coerced value, persist a session change, notify, and reconcile dependents."
  (when (eq (setting-scope setting) ':derived)
    (error 'setting-read-only
           :setting setting
           :message (format nil "~A is derived and cannot be set."
                            (setting-label setting))))
  (let ((coerced (setting-coerce setting value configuration)))
    (setting-validate setting coerced configuration)
    (let ((old (configuration-setting-value configuration setting)))
      (with-lock-held ((configuration-lock configuration))
        (setf (gethash (setting-name setting) (configuration-values configuration))
              coerced
              (gethash (setting-name setting) (configuration-sources configuration))
              source))
      (when (and (eq (setting-scope setting) ':durable)
                 (eq source ':session)
                 (configuration-store configuration))
        (store-write-value (configuration-store configuration) configuration
                           (setting-name setting) coerced))
      (configuration--note-change configuration setting old coerced)
      (configuration--reconcile-dependents configuration setting source)
      coerced)))

(-> configuration-persist (configuration symbol) null)
(defun configuration-persist (configuration name)
  "Write CONFIGURATION's current value of durable setting NAME to its store."
  (let ((setting (configuration-setting configuration name)))
    (unless (eq (setting-scope setting) ':durable)
      (error 'setting-invalid
             :setting setting
             :value nil
             :message (format nil "~A is not a durable setting." (setting-label setting))))
    (when (configuration-store configuration)
      (store-write-value (configuration-store configuration) configuration name
                         (configuration-setting-value configuration setting))))
  nil)

(-> configuration-unset (configuration symbol) null)
(defun configuration-unset (configuration name)
  "Forget CONFIGURATION's stored value for NAME so its default applies again."
  (let* ((setting (configuration-setting configuration name))
         (old (configuration-setting-value configuration setting)))
    (with-lock-held ((configuration-lock configuration))
      (remhash name (configuration-values configuration))
      (remhash name (configuration-sources configuration)))
    (configuration--note-change
     configuration setting old (configuration-setting-value configuration setting)))
  nil)

(-> configuration-add-listener (configuration (or symbol function)) null)
(defun configuration-add-listener (configuration listener)
  "Call LISTENER with (configuration setting old new) after each value change."
  (with-lock-held ((configuration-lock configuration))
    (pushnew listener (configuration-listeners configuration)))
  nil)

(-> configuration-remove-listener (configuration (or symbol function)) null)
(defun configuration-remove-listener (configuration listener)
  "Stop calling LISTENER for CONFIGURATION's changes."
  (with-lock-held ((configuration-lock configuration))
    (setf (configuration-listeners configuration)
          (remove listener (configuration-listeners configuration))))
  nil)


;;;; -- Reconciliation --

(defmethod setting-reconcile ((setting setting) configuration source)
  "Leave an ordinary setting's value alone."
  (declare (ignore setting configuration source))
  nil)

(defmethod setting-reconcile ((setting choice-setting) configuration source)
  "Store the first option when the current value is no longer offered.

The option is stored with SOURCE, the source of the change that caused the
reconciliation, so a session change persists the reconciled value of a
durable setting like a user's choice, while loading never writes the store.
Nothing happens while validation is deferred for SETTING or when it offers no
options."
  (unless (setting--deferred-p setting configuration)
    (let ((options (setting-options setting configuration)))
      (unless (setting--option-acceptable-p
               setting (configuration-setting-value configuration setting) options)
        (configuration-set configuration setting (first options) :source source))))
  nil)


;;;; -- Private Functions --

(-> configuration--required ((or null configuration)) configuration)
(defun configuration--required (configuration)
  "Return CONFIGURATION, or signal CONFIGURATION-MISSING when none is current."
  (or configuration
      (error 'configuration-missing
             :message "No configuration is current; pass one or bind *configuration*.")))

(-> configuration--note-change (configuration setting t t) null)
(defun configuration--note-change (configuration setting old new)
  "Tell CONFIGURATION's listeners that SETTING changed from OLD to NEW."
  (unless (equal old new)
    (dolist (listener (with-lock-held ((configuration-lock configuration))
                        (copy-list (configuration-listeners configuration))))
      (funcall listener configuration setting old new)))
  nil)

(-> configuration--reconcile-dependents (configuration setting configuration-source) null)
(defun configuration--reconcile-dependents (configuration setting source)
  "Reconcile every setting of CONFIGURATION depending on SETTING, stored from SOURCE."
  (let ((name (setting-name setting)))
    (dolist (dependent (configuration-setting-list configuration))
      (when (member name (setting-dependencies dependent))
        (setting-reconcile dependent configuration source))))
  nil)
