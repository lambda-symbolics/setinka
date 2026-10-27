(in-package #:setinka)

;;;; -- Test Fixtures --

(defclass memory-store (setting-store)
  ((values
    :initarg       :values
    :initform      nil
    :accessor      memory-store-values
    :type          list
    :documentation "The durable plist the store holds.")
   (writes
    :initform      nil
    :accessor      memory-store-writes
    :type          list
    :documentation "The (name value) writes received, newest first."))
  (:documentation "A store keeping durable values in memory for tests."))

(defmethod store-read-values ((store memory-store) configuration)
  "Return the held plist."
  (declare (ignore configuration))
  (copy-list (memory-store-values store)))

(defmethod store-write-value ((store memory-store) configuration name &key value)
  "Merge NAME and VALUE into the held plist and record the write."
  (declare (ignore configuration))
  (setf (getf (memory-store-values store) name) value)
  (push (list name value) (memory-store-writes store))
  nil)

(defparameter *configuration-tests-environment-variable* "SETINKA_TEST_LEVEL"
  "The environment variable the configuration tests set and restore.")

(-> configuration-tests--registry () setting-registry)
(defun configuration-tests--registry ()
  "Return a registry exercising every source, scope, and dependency."
  (let ((registry (make-setting-registry)))
    (define-setting :level (integer-setting :registry registry)
      :label               "Level"
      :scope               :durable
      :environment         *configuration-tests-environment-variable*
      :minimum             0
      :default             1)
    (define-setting :colour (choice-setting :registry registry)
      :label                "Colour"
      :scope                :session
      :type                 'keyword
      :options              '(:red :green)
      :default              :red)
    (define-setting :family (choice-setting :registry registry)
      :label                "Family"
      :scope                :durable
      :type                 'string
      :deferrable-p         t
      :options              (lambda (configuration)
                              (declare (ignore configuration))
                              '("small" "large"))
      :default              "small")
    (define-setting :size (choice-setting :registry registry)
      :label              "Size"
      :scope              :durable
      :type               'string
      :deferrable-p       t
      :depends-on         '(:family)
      :options            (lambda (configuration)
                            (if (string= (config :family configuration) "small")
                                '("s" "m")
                                '("l" "xl")))
      :default            "m")
    (define-setting :area (derived-setting :registry registry)
      :label              "Area"
      :function           (lambda (configuration)
                            (* 10 (config :level configuration))))
    registry))

(-> configuration-tests--call-with-environment ((or null string) function) t)
(defun configuration-tests--call-with-environment (value function)
  "Call FUNCTION with the test environment variable set to VALUE, then restore it."
  (let* ((variable *configuration-tests-environment-variable*)
         (previous (uiop:getenv variable)))
    (unwind-protect
         (progn
           (setf (uiop:getenv variable) (or value ""))
           (funcall function))
      (setf (uiop:getenv variable) (or previous "")))))


;;;; -- Configuration Tests --

(define-test test-reading-and-session-changes
  (let* ((store         (make-instance 'memory-store))
         (configuration (make-configuration :registry (configuration-tests--registry)
                                            :store    store))
         (changes       nil)
         (listener      (lambda (configuration setting old new)
                          (declare (ignore configuration))
                          (push (list (setting-name setting) old new) changes))))
    (configuration-add-listener configuration listener)
    (test-equal 1 (config :level configuration))
    (test-equal nil (configuration-setting-source configuration :level))
    (test-equal 10 (config :area configuration))
    (with-configuration (configuration)
      (setf (config :level) "4"))
    (test-equal 4 (config :level configuration))
    (test-equal 40 (config :area configuration))
    (test-equal :session (configuration-setting-source configuration :level))
    (test-equal '((:level 4)) (memory-store-writes store))
    (setf (config :colour configuration) "Green")
    (test-equal :green (config :colour configuration))
    (test-equal 1 (length (memory-store-writes store))
                :description "session settings are not stored")
    (setf (config :colour configuration) :green)
    (test-equal '((:colour :red :green) (:level 1 4)) changes
                :description "listeners hear changes only")
    (configuration-unset configuration :level)
    (test-equal 1 (config :level configuration))
    (configuration-remove-listener configuration listener)
    (setf (config :level configuration) 2)
    (test-equal '(:level 4 1) (first changes))
    (test-equal '(:level :colour :level) (mapcar #'first (reverse changes)))
    (test-equal '(:level 2 :colour :green :family "small" :size "m")
                (loop for setting in (configuration-setting-list configuration)
                      unless (eq (setting-scope setting) ':derived)
                        append (list (setting-name setting)
                                     (config (setting-name setting) configuration))))))

(define-test test-refused-changes
  (let ((configuration (make-configuration :registry (configuration-tests--registry)
                                           :store    nil)))
    (test-signals setting-invalid (setf (config :level configuration) -1))
    (test-signals setting-invalid (setf (config :colour configuration) :blue))
    (test-signals setting-read-only (setf (config :area configuration) 3))
    (test-signals setting-unknown (config :missing configuration))
    (test-signals setting-invalid (configuration-persist configuration :colour))
    (test-equal 1 (config :level configuration))
    (let ((*configuration*                  nil)
          (*configuration-default-function* nil))
      (test-signals configuration-missing (config :level)))
    (let ((*configuration-default-function* (lambda () configuration)))
      (test-equal 1 (config :level)))))

(define-test test-source-precedence
  (let ((registry (configuration-tests--registry)))
    (configuration-tests--call-with-environment
     "7"
     (lambda ()
       (let* ((store          (make-instance 'memory-store :values '(:level 9 :family "large")))
              (loaded         (configuration-load :registry registry :store store))
              (overridden     (configuration-load :registry  registry
                                                  :store     store
                                                  :overrides '(:level 3)))
              (no-environment (configuration-load :registry      registry
                                                  :store         store
                                                  :environment-p nil))
              (no-durable     (configuration-load :registry      registry
                                                  :store         store
                                                  :environment-p nil
                                                  :durable-p     nil)))
         (test-equal 7 (config :level loaded))
         (test-equal :environment (configuration-setting-source loaded :level))
         (test-equal "large" (config :family loaded))
         (test-equal :durable (configuration-setting-source loaded :family))
         (test-equal "l" (config :size loaded)
                     :description "loading reconciles dependents of loaded values")
         (test-equal :durable (configuration-setting-source loaded :size))
         (test-equal 3 (config :level overridden))
         (test-equal :override (configuration-setting-source overridden :level))
         (test-equal 9 (config :level no-environment))
         (test-equal 1 (config :level no-durable))
         (test-equal nil (memory-store-writes store)
                     :description "loading never writes the store"))))))

(define-test test-invalid-durable-values-are-dropped
  (configuration-tests--call-with-environment
   nil
   (lambda ()
     (let* ((store         (make-instance 'memory-store
                                          :values '(:level -5 :family "large" :colour :green)))
            (configuration (configuration-load :registry (configuration-tests--registry)
                                               :store    store)))
       (test-equal 1 (config :level configuration))
       (test-equal nil (configuration-setting-source configuration :level))
       (test-equal "large" (config :family configuration)
                   :description "valid durable values beside a dropped one still apply")
       (test-equal :red         (config :colour configuration)
                   :description "values of settings that are not durable are ignored")))))

(define-test test-invalid-environment-values
  (configuration-tests--call-with-environment
   "many"
   (lambda ()
     (let ((condition (test-signals setting-invalid
                        (configuration-load :registry (configuration-tests--registry)
                                            :store    nil))))
       (test-equal "Level must be an integer, not \"many\"."
                   (princ-to-string condition))))))

(define-test test-unknown-override-names
  (test-signals setting-unknown
    (make-configuration :registry  (configuration-tests--registry)
                        :overrides '(:missing 1)))
  (test-signals setting-unknown
    (configuration-load :registry  (configuration-tests--registry)
                        :store     nil
                        :overrides '(:missing 1))))

(define-test test-process-defaults-are-validated-on-load
  (let ((registry (make-setting-registry)))
    (define-setting :port (integer-setting :registry registry)
      :label              "Port"
      :minimum            1
      :default            0)
    (test-signals setting-invalid (configuration-load :registry registry :store nil))
    (test-equal 0 (config :port (make-configuration :registry registry :store nil))
                :description "constructing from overrides alone trusts defaults")))

(define-test test-copies
  (let* ((store    (make-instance 'memory-store))
         (original (make-configuration :registry  (configuration-tests--registry)
                                       :store     store
                                       :overrides '(:level 5)))
         (heard    nil))
    (configuration-add-listener original (lambda (&rest arguments)
                                           (declare (ignore arguments))
                                           (setf heard t)))
    (let ((copy (configuration-copy original :overrides '(:colour :green))))
      (test-equal 5 (config :level copy))
      (test-equal :override (configuration-setting-source copy :level))
      (test-equal :green (config :colour copy))
      (test-equal :red (config :colour original))
      (setf (config :level copy) 6)
      (test-equal 5 (config :level original))
      (test-assert (not heard) "listeners stay with the original")
      (test-equal '((:level 6)) (memory-store-writes store)
                  :description "copies share the store"))))

(define-test test-dependent-settings-reconcile
  (let ((configuration (make-configuration :registry (configuration-tests--registry)
                                           :store    nil)))
    (test-equal "m" (config :size configuration))
    (setf (config :family configuration) "large")
    (test-equal "l" (config :size configuration)
                :description "a choice no longer offered becomes the first option")
    (test-equal :session (configuration-setting-source configuration :size))
    (setf (config :size configuration) "xl")
    (setf (config :family configuration) "large")
    (test-equal "xl" (config :size configuration)
                :description "a value still offered is kept")))

(define-test test-deferred-validation
  (configuration-tests--call-with-environment
   nil
   (lambda ()
     (let* ((registry      (configuration-tests--registry))
            (store         (make-instance 'memory-store
                                          :values '(:family "huge" :size "tiny")))
            (configuration (configuration-load :registry              registry
                                               :store                 store
                                               :validation-deferred-p t)))
       (test-equal "huge" (config :family configuration)
                   :description "deferred settings accept values outside their options")
       (test-signals setting-invalid (setf (config :level configuration) -1))
       (test-signals setting-invalid (setf (config :family configuration) 42))
       (setf (config :family configuration) "large")
       (test-equal "tiny" (config :size configuration)
                   :description "reconciliation waits while validation is deferred")
       (configuration-validate-deferred configuration)
       (test-assert (not (configuration-validation-deferred-p configuration)))
       (test-equal "large" (config :family configuration))
       (test-equal "m" (config :size configuration)
                   :description "a failing durable value is forgotten")
       (test-signals setting-invalid (setf (config :family configuration) "huge")))))
  (let ((explicit (make-configuration :registry              (configuration-tests--registry)
                                      :store                 nil
                                      :overrides             '(:family "huge")
                                      :validation-deferred-p t)))
    (test-signals setting-invalid (configuration-validate-deferred explicit)))
  (let ((dependent-default (make-configuration :registry              (configuration-tests--registry)
                                               :store                 nil
                                               :overrides             '(:family "large")
                                               :validation-deferred-p t)))
    (configuration-validate-deferred dependent-default)
    (test-equal "m" (config :size dependent-default)
                :description "a dependent default waits for its next reconciliation")))
