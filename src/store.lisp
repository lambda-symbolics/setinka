(in-package #:setinka)

;;;; -- Durable Stores --

(defgeneric store-read-values (store configuration)
  (:documentation
   "Return the durable values STORE holds for CONFIGURATION as a (name value ...) plist.

CONFIGURATION is passed so a store may locate itself from other settings,
such as a configuration directory. Names the registry does not know, and
settings that are not durable, are ignored by the caller."))

(defgeneric store-write-value (store configuration name &key value)
  (:documentation
   "Persist VALUE under NAME in STORE for CONFIGURATION.

The value is the coerced, validated representation. A store should merge
with what it already holds so concurrent processes keep each other's values."))

(defvar *configuration-store* nil
  "The store configurations are constructed with when none is given, or NIL.")
