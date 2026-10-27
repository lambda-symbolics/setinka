(defpackage #:setinka
  (:documentation "Typed, documented, layered settings with durable stores and dependent values.")
  (:use #:cl)
  (:import-from #:serapeum
                #:->
                #:partial)
  (:import-from #:bordeaux-threads
                #:make-lock
                #:with-lock-held)
  (:export
   ;; Settings
   #:setting
   #:setting-name
   #:setting-type
   #:setting-documentation
   #:setting-label
   #:setting-group
   #:setting-default
   #:setting-options-designator
   #:setting-scope
   #:setting-environment
   #:setting-visible-p
   #:setting-validator
   #:setting-dependencies
   #:setting-deferrable-p
   #:boolean-setting
   #:choice-setting
   #:number-setting
   #:integer-setting
   #:real-setting
   #:setting-minimum
   #:setting-maximum
   #:string-setting
   #:pathname-setting
   #:derived-setting
   #:setting-function
   #:setting-coerce
   #:setting-validate
   #:setting-options
   #:setting-render-value
   #:setting-reconcile
   #:setting-default-value
   #:setting-parse
   #:setting-reject
   ;; Registries
   #:setting-registry
   #:make-setting-registry
   #:*settings*
   #:register-setting
   #:find-setting
   #:settings-list
   #:define-setting
   ;; Stores
   #:setting-store
   #:store-read-values
   #:store-write-value
   #:*configuration-store*
   ;; Configurations
   #:configuration
   #:configuration-registry
   #:configuration-store
   #:configuration-validation-deferred-p
   #:configuration-source
   #:*configuration*
   #:*configuration-default-function*
   #:configuration-current
   #:configuration-setting
   #:configuration-setting-list
   #:configuration-stored-value
   #:configuration-setting-value
   #:configuration-group-values
   #:configuration-setting-source
   #:config
   #:configuration-set
   #:configuration-unset
   #:configuration-persist
   #:configuration-add-listener
   #:configuration-remove-listener
   #:with-configuration
   #:make-configuration
   #:configuration-copy
   #:configuration-load
   #:configuration-durable-values
   #:configuration-validate-deferred
   ;; Conditions
   #:setting-error
   #:setting-error-message
   #:setting-unknown
   #:setting-unknown-name
   #:setting-invalid
   #:setting-invalid-setting
   #:setting-invalid-value
   #:setting-read-only
   #:setting-read-only-setting
   #:configuration-missing))
