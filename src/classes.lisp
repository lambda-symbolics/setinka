(in-package #:setinka)

;;;; -- Setting Classes --

(deftype setting-scope ()
  "Where a setting's value lives and how long it lasts.

:PROCESS values come from overrides, the environment, or defaults and are
not expected to change while the process runs. :DURABLE values persist across
processes through the configuration's store. :SESSION values may change while
the process runs and are forgotten at exit. :DERIVED values are computed from
other settings on every read."
  '(member :process :durable :session :derived))

(defclass setting ()
  ((name
    :initarg :name
    :reader setting-name
    :type keyword
    :documentation "The keyword naming the setting in CONFIG calls.")
   (type
    :initarg :type
    :initform t
    :reader setting-type
    :type t
    :documentation "The type specifier every stored value satisfies.")
   (documentation
    :initarg :documentation
    :initform ""
    :reader setting-documentation
    :type string
    :documentation "The one-sentence explanation shown beside the setting.")
   (label
    :initarg :label
    :reader setting-label
    :type string
    :documentation "The short human name used in listings and error reports.")
   (group
    :initarg :group
    :initform ':general
    :reader setting-group
    :type keyword
    :documentation "The section of a settings listing the setting belongs to.")
   (default
    :initarg :default
    :initform nil
    :reader setting-default
    :type t
    :documentation "The default value, or a function of the configuration returning it.")
   (options
    :initarg :options
    :initform nil
    :reader setting-options-designator
    :type t
    :documentation "The permitted values as a list, or a function of the configuration returning them.")
   (scope
    :initarg :scope
    :initform ':process
    :reader setting-scope
    :type setting-scope
    :documentation "Where the value lives; see SETTING-SCOPE.")
   (environment
    :initarg :environment
    :initform nil
    :reader setting-environment
    :type (or null string)
    :documentation "The environment variable that supplies the value when a configuration loads.")
   (visible-p
    :initarg :visible-p
    :initform t
    :reader setting-visible-p
    :type boolean
    :documentation "Whether settings listings show the setting.")
   (validator
    :initarg :validator
    :initform nil
    :reader setting-validator
    :type (or null symbol function)
    :documentation "A function of a value and the configuration returning NIL when the
value is acceptable, or a message explaining why it is not.")
   (dependencies
    :initarg :depends-on
    :initform nil
    :reader setting-dependencies
    :type list
    :documentation "The names of settings whose changes may invalidate this one's value.

After any of them is stored, SETTING-RECONCILE runs for this setting.")
   (deferrable-p
    :initarg :deferrable-p
    :initform nil
    :reader setting-deferrable-p
    :type boolean
    :documentation "Whether the options and validator wait while a configuration defers validation.

Use it for settings checked against something that is assembled after the
configuration, such as a registry of providers loaded by user code."))
  (:default-initargs :label "")
  (:documentation "One named, typed, documented configuration value."))

(defclass boolean-setting (setting)
  ()
  (:default-initargs :type 'boolean)
  (:documentation "A setting holding T or NIL, written as on or off."))

(defclass choice-setting (setting)
  ()
  (:documentation "A setting whose value is one of its options."))

(defclass integer-setting (setting)
  ((minimum
    :initarg :minimum
    :initform nil
    :reader setting-minimum
    :type (or null integer)
    :documentation "The smallest permitted value, when bounded.")
   (maximum
    :initarg :maximum
    :initform nil
    :reader setting-maximum
    :type (or null integer)
    :documentation "The largest permitted value, when bounded."))
  (:default-initargs :type 'integer)
  (:documentation "A setting holding an optionally bounded integer."))

(defclass string-setting (setting)
  ()
  (:default-initargs :type 'string)
  (:documentation "A setting holding a string."))

(defclass pathname-setting (setting)
  ()
  (:default-initargs :type 'pathname)
  (:documentation "A setting holding a pathname."))

(defclass derived-setting (setting)
  ((function
    :initarg :function
    :reader setting-function
    :type (or symbol function)
    :documentation "The function of the configuration computing the value."))
  (:default-initargs :scope ':derived)
  (:documentation "A read-only setting computed from the configuration on each read."))


;;;; -- Registry and Store Classes --

(defclass setting-registry ()
  ((table
    :initform (make-hash-table :test #'eq)
    :reader setting-registry-table
    :type hash-table
    :documentation "The settings by name.")
   (order
    :initform (make-array 0 :adjustable t :fill-pointer t)
    :reader setting-registry-order
    :type vector
    :documentation "The setting names in definition order.")
   (lock
    :initform (make-lock "setinka registry")
    :reader setting-registry-lock
    :type t
    :documentation "The lock serializing definitions and lookups."))
  (:documentation "Settings by name, remembered in definition order."))

(defclass setting-store ()
  ()
  (:documentation
   "Where durable setting values persist between processes.

Subclasses implement STORE-READ-VALUES and STORE-WRITE-VALUE."))


;;;; -- Configuration Class --

(deftype configuration-source ()
  "How a stored configuration value was chosen."
  '(member :override :environment :durable :session))

(defclass configuration ()
  ((registry
    :initarg :registry
    :reader configuration-registry
    :type setting-registry
    :documentation "The settings this configuration understands.")
   (store
    :initarg :store
    :initform nil
    :reader configuration-store
    :type (or null setting-store)
    :documentation "Where durable values are read and written, or NIL to keep them in memory.")
   (validation-deferred-p
    :initarg :validation-deferred-p
    :initform nil
    :accessor configuration-validation-deferred-p
    :type boolean
    :documentation "Whether deferrable settings skip their options and validator for now.")
   (values
    :initform (make-hash-table :test #'eq)
    :reader configuration-values
    :type hash-table
    :documentation "Stored values by setting name; absent names take their default.")
   (sources
    :initform (make-hash-table :test #'eq)
    :reader configuration-sources
    :type hash-table
    :documentation "The CONFIGURATION-SOURCE of each stored value by setting name.")
   (lock
    :initform (make-lock "setinka configuration")
    :reader configuration-lock
    :type t
    :documentation "The lock serializing value reads and writes.")
   (listeners
    :initform nil
    :accessor configuration-listeners
    :type list
    :documentation "Functions of (configuration setting old new) called after a value changes."))
  (:documentation "The values of one registry's settings for one process, agent, or task."))
