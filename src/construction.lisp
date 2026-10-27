(in-package #:setinka)

;;;; -- Construction --

(-> make-configuration
    (&key (:registry setting-registry)
          (:store (or null setting-store))
          (:overrides list)
          (:validation-deferred-p boolean))
    configuration)
(defun make-configuration (&key (registry *settings*)
                                (store *configuration-store*)
                                overrides
                                validation-deferred-p)
  "Return a configuration of REGISTRY's settings holding only OVERRIDES over their defaults.

OVERRIDES is a (name value ...) plist applied in setting definition order,
so a setting is stored after those it may depend on. Neither the environment
nor STORE is read, which makes the result deterministic; STORE still receives
later session changes to durable settings. The configuration keeps a
snapshot of REGISTRY, so later definitions do not change it."
  (let ((configuration (make-instance 'configuration
                                      :registry (setting-registry--snapshot registry)
                                      :store store
                                      :validation-deferred-p validation-deferred-p)))
    (configuration--apply-overrides configuration overrides)
    configuration))

(-> configuration-copy
    (configuration &key (:overrides list) (:validation-deferred-p boolean))
    configuration)
(defun configuration-copy
    (configuration &key overrides
                        (validation-deferred-p
                         (configuration-validation-deferred-p configuration)))
  "Return a copy of CONFIGURATION with OVERRIDES applied in setting definition order.

The copy shares CONFIGURATION's registry snapshot and store, starts with its
stored values and their sources, and defers validation like it unless
VALIDATION-DEFERRED-P says otherwise. Listeners stay with the original, so a
copy made for a child or a task reacts only to its own changes."
  (let ((copy (make-instance 'configuration
                             :registry (configuration-registry configuration)
                             :store (configuration-store configuration)
                             :validation-deferred-p validation-deferred-p)))
    (with-lock-held ((configuration-lock configuration))
      (maphash (lambda (name value)
                 (setf (gethash name (configuration-values copy)) value))
               (configuration-values configuration))
      (maphash (lambda (name source)
                 (setf (gethash name (configuration-sources copy)) source))
               (configuration-sources configuration)))
    (configuration--apply-overrides copy overrides)
    copy))

(-> configuration-load
    (&key (:registry setting-registry)
          (:store (or null setting-store))
          (:overrides list)
          (:validation-deferred-p boolean)
          (:environment-p boolean)
          (:durable-p boolean))
    configuration)
(defun configuration-load (&key (registry *settings*)
                                (store *configuration-store*)
                                overrides
                                validation-deferred-p
                                (environment-p t)
                                (durable-p t))
  "Return a configuration whose settings take the first source that supplies them.

The sources are, in order, OVERRIDES, the setting's environment variable when
ENVIRONMENT-P, STORE's durable values for durable settings when DURABLE-P,
and finally the default. Overrides and the environment apply first, in
definition order, so the store can locate itself from the values they chose.
A durable value that no longer validates is dropped. Every process setting
left at its default is then validated, so a default the host cannot honor
fails at load rather than at first use."
  (let ((configuration (make-instance 'configuration
                                      :registry (setting-registry--snapshot registry)
                                      :store store
                                      :validation-deferred-p validation-deferred-p)))
    (configuration--check-override-names configuration overrides)
    (dolist (setting (configuration-setting-list configuration))
      (unless (eq (setting-scope setting) ':derived)
        (let ((override (configuration--plist-cell (setting-name setting) overrides)))
          (multiple-value-bind (environment environment-p)
              (if environment-p
                  (configuration--environment-value setting)
                  (values nil nil))
            (cond
              (override
               (configuration-set configuration setting (second override)
                                  :source ':override))
              (environment-p
               (configuration-set configuration setting environment
                                  :source ':environment)))))))
    (when durable-p
      (let ((durable (configuration-durable-values configuration)))
        (dolist (setting (configuration-setting-list configuration))
          (let ((cell (configuration--plist-cell (setting-name setting) durable)))
            (when (and cell
                       (eq (setting-scope setting) ':durable)
                       (not (nth-value 1 (configuration-stored-value
                                          configuration setting))))
              (handler-case
                  (configuration-set configuration setting (second cell)
                                     :source ':durable)
                (setting-invalid ()
                  nil)))))))
    (configuration--validate-defaults configuration)
    configuration))

(-> configuration-durable-values (configuration) list)
(defun configuration-durable-values (configuration)
  "Return the durable values CONFIGURATION's store holds as a (name value ...) plist."
  (let ((store (configuration-store configuration)))
    (and store (store-read-values store configuration))))

(-> configuration-validate-deferred (configuration) configuration)
(defun configuration-validate-deferred (configuration)
  "Stop deferring validation in CONFIGURATION and check each deferrable setting.

Settings are checked in definition order. A failing durable value is
forgotten so the default applies. A failing default signals unless the
setting has dependencies, whose reconciliation brings it in line when a
dependency is next stored. A failing explicit value always signals."
  (setf (configuration-validation-deferred-p configuration) nil)
  (dolist (setting (configuration-setting-list configuration))
    (when (and (setting-deferrable-p setting)
               (not (eq (setting-scope setting) ':derived)))
      (handler-case
          (setting-validate setting (configuration-setting-value configuration setting)
                            configuration)
        (setting-invalid (condition)
          (case (configuration-setting-source configuration (setting-name setting))
            (:durable
             (configuration-unset configuration (setting-name setting)))
            ((nil)
             (unless (setting-dependencies setting)
               (error condition)))
            (t
             (error condition)))))))
  configuration)


;;;; -- Private Functions --

(-> setting-registry--snapshot (setting-registry) setting-registry)
(defun setting-registry--snapshot (registry)
  "Return a new registry holding REGISTRY's current settings in the same order."
  (let ((snapshot (make-setting-registry)))
    (dolist (setting (settings-list registry))
      (register-setting setting snapshot))
    snapshot))

(-> configuration--plist-cell (symbol list) list)
(defun configuration--plist-cell (name plist)
  "Return the tail of PLIST whose first element is the key NAME, or NIL."
  (loop for tail on plist by (partial #'nthcdr 2)
        when (eq (first tail) name)
          return tail))

(-> configuration--check-override-names (configuration list) null)
(defun configuration--check-override-names (configuration overrides)
  "Signal SETTING-UNKNOWN for an OVERRIDES name CONFIGURATION does not know."
  (loop for (name nil) on overrides by (partial #'nthcdr 2)
        do (configuration-setting configuration name))
  nil)

(-> configuration--apply-overrides (configuration list) null)
(defun configuration--apply-overrides (configuration overrides)
  "Store OVERRIDES in CONFIGURATION with the :OVERRIDE source, in definition order."
  (configuration--check-override-names configuration overrides)
  (dolist (setting (configuration-setting-list configuration))
    (let ((cell (configuration--plist-cell (setting-name setting) overrides)))
      (when cell
        (configuration-set configuration setting (second cell) :source ':override))))
  nil)

(-> configuration--environment-value (setting) (values t boolean))
(defun configuration--environment-value (setting)
  "Return SETTING's environment text and whether it holds more than whitespace."
  (let* ((variable (setting-environment setting))
         (value (and variable (uiop:getenv variable))))
    (if (and value
             (find-if-not (lambda (character)
                            (member character '(#\Space #\Tab #\Newline #\Return #\Page)))
                          value))
        (values value t)
        (values nil nil))))

(-> configuration--validate-defaults (configuration) null)
(defun configuration--validate-defaults (configuration)
  "Validate every process setting of CONFIGURATION still at its default."
  (dolist (setting (configuration-setting-list configuration))
    (when (and (eq (setting-scope setting) ':process)
               (not (nth-value 1 (configuration-stored-value configuration setting))))
      (setting-validate setting (configuration-setting-value configuration setting)
                        configuration)))
  nil)
