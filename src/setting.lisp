(in-package #:setinka)

;;;; -- Setting Protocol --

(defgeneric setting-coerce (setting value configuration)
  (:documentation
   "Return VALUE converted to SETTING's representation for CONFIGURATION.

Environment variables, settings pages, and command arguments supply strings;
coercion turns them into the stored representation without validating them.
Signal SETTING-INVALID, usually through SETTING-REJECT, for text that has no
representation."))

(defgeneric setting-validate (setting value configuration)
  (:documentation
   "Signal SETTING-INVALID unless VALUE is acceptable for SETTING in CONFIGURATION."))

(defgeneric setting-options (setting configuration)
  (:documentation "Return SETTING's permitted values for CONFIGURATION, or NIL when open."))

(defgeneric setting-render-value (setting value)
  (:documentation "Return VALUE as the text a settings listing shows for SETTING."))

(defgeneric setting-reconcile (setting configuration source)
  (:documentation
   "Bring SETTING's value in CONFIGURATION back in line after a dependency changed.

CONFIGURATION-SET calls it for every setting whose dependencies name the
setting it just stored, passing the SOURCE that change was stored with."))


;;;; -- Coercion --

(defmethod setting-coerce ((setting setting) value configuration)
  "Store VALUE as given."
  (declare (ignore setting configuration))
  value)

(defmethod setting-coerce ((setting boolean-setting) (value string) configuration)
  "Read on, true, yes, and 1 as true and off, false, no, 0, and empty as false."
  (declare (ignore configuration))
  (let ((text (string-downcase (string-trim '(#\Space #\Tab) value))))
    (cond
      ((member text '("on" "true" "yes" "1") :test #'string=)
       t)
      ((member text '("off" "false" "no" "0" "") :test #'string=)
       nil)
      (t
       (setting-reject setting value "~A must be on or off, not ~S."
                       (setting-label setting) value)))))

(defmethod setting-coerce ((setting integer-setting) (value string) configuration)
  "Parse VALUE as a decimal integer."
  (declare (ignore configuration))
  (handler-case (parse-integer (string-trim '(#\Space #\Tab) value))
    (error ()
      (setting-reject setting value "~A must be an integer, not ~S."
                      (setting-label setting) value))))

(defmethod setting-coerce ((setting real-setting) (value string) configuration)
  "Parse VALUE as an optionally signed decimal: an integer, else a single float."
  (declare (ignore configuration))
  (or (setting--parse-decimal value)
      (setting-reject setting value "~A must be a decimal number, not ~S."
                      (setting-label setting) value)))

(defmethod setting-coerce ((setting pathname-setting) (value string) configuration)
  "Parse VALUE as a pathname."
  (declare (ignore configuration))
  (handler-case (parse-namestring value)
    (error ()
      (setting-reject setting value "~A must be a pathname, not ~S."
                      (setting-label setting) value))))

(defmethod setting-coerce ((setting choice-setting) (value string) configuration)
  "Match VALUE against keyword options case-insensitively, else keep the string."
  (let ((options (setting-options setting configuration)))
    (or (find-if (lambda (option)
                   (and (keywordp option)
                        (string-equal (symbol-name option) value)))
                 options)
        value)))


;;;; -- Validation --

(defmethod setting-validate ((setting setting) value configuration)
  "Require VALUE to satisfy SETTING's type and, unless deferred, its validator."
  (unless (typep value (setting-type setting))
    (setting-reject setting value "~A does not accept ~S." (setting-label setting) value))
  (let ((validator (setting-validator setting)))
    (when (and validator (not (setting--deferred-p setting configuration)))
      (let ((message (funcall validator value configuration)))
        (when message
          (setting-reject setting value "~A" message)))))
  nil)

(defmethod setting-validate ((setting choice-setting) value configuration)
  "Require VALUE to be one of SETTING's options when it declares any.

NIL stays acceptable when SETTING's type admits it, so an optional choice
can be unset without appearing among the options. A deferrable setting skips
the check while CONFIGURATION defers validation."
  (call-next-method)
  (unless (setting--deferred-p setting configuration)
    (let ((options (setting-options setting configuration)))
      (unless (setting--option-acceptable-p setting value options)
        (setting-reject setting value "~A must be one of ~{~A~^, ~}, not ~A."
                        (setting-label setting)
                        (mapcar (lambda (option)
                                  (setting-render-value setting option))
                                options)
                        (setting-render-value setting value)))))
  nil)

(defmethod setting-validate ((setting number-setting) value configuration)
  "Require VALUE to fall within SETTING's bounds."
  (call-next-method)
  (let ((minimum (setting-minimum setting))
        (maximum (setting-maximum setting)))
    (when (or (and minimum (< value minimum))
              (and maximum (> value maximum)))
      (setting-reject setting value
                      "~A must be ~@[at least ~A~]~@[ and ~*~]~@[at most ~A~], not ~A."
                      (setting-label setting)
                      (and minimum (setting-render-value setting minimum))
                      (and minimum maximum)
                      (and maximum (setting-render-value setting maximum))
                      (setting-render-value setting value))))
  nil)


;;;; -- Options and Presentation --

(defmethod setting-options ((setting setting) configuration)
  "Resolve the options designator: a list as is, a function by calling it."
  (let ((designator (setting-options-designator setting)))
    (cond
      ((null designator)
       nil)
      ((functionp designator)
       (funcall designator configuration))
      ((and (symbolp designator) (fboundp designator))
       (funcall designator configuration))
      (t
       designator))))

(defmethod setting-options ((setting boolean-setting) configuration)
  "Offer on and off."
  (declare (ignore setting configuration))
  (list t nil))

(defmethod setting-render-value ((setting setting) value)
  "Show keywords in lower case, pathnames as namestrings, and NIL as unset."
  (declare (ignore setting))
  (cond
    ((null value)
     "unset")
    ((keywordp value)
     (string-downcase (symbol-name value)))
    ((pathnamep value)
     (namestring value))
    (t
     (princ-to-string value))))

(defmethod setting-render-value ((setting boolean-setting) value)
  "Show true as on and false as off."
  (declare (ignore setting))
  (if value "on" "off"))

(defmethod setting-render-value ((setting real-setting) value)
  "Show a ratio as the decimal a person would type for it."
  (if (typep value 'ratio)
      (call-next-method setting (float value 1.0))
      (call-next-method)))


;;;; -- Public Functions --

(-> setting-default-value (setting t) t)
(defun setting-default-value (setting configuration)
  "Return SETTING's default for CONFIGURATION, calling a function default."
  (let ((default (setting-default setting)))
    (if (or (functionp default)
            (and (symbolp default) (not (keywordp default)) default (fboundp default)))
        (funcall default configuration)
        default)))

(-> setting-parse (setting t t) t)
(defun setting-parse (setting text configuration)
  "Return TEXT coerced and validated as a value for SETTING in CONFIGURATION."
  (let ((value (setting-coerce setting text configuration)))
    (setting-validate setting value configuration)
    value))

(-> setting-reject (setting t string &rest t) nil)
(defun setting-reject (setting value control &rest arguments)
  "Signal SETTING-INVALID for VALUE, explained by CONTROL and ARGUMENTS."
  (error 'setting-invalid
         :setting setting
         :value   value
         :message (apply #'format nil control arguments)))


;;;; -- Private Functions --

(-> setting--deferred-p (setting t) boolean)
(defun setting--deferred-p (setting configuration)
  "Return true when SETTING's options and validator wait for CONFIGURATION."
  (and (setting-deferrable-p setting)
       (typep configuration 'configuration)
       (configuration-validation-deferred-p configuration)))

(-> setting--parse-decimal (string) (or null real))
(defun setting--parse-decimal (text)
  "Return the real TEXT writes as an optionally signed decimal, or NIL.

Digits without a point give an integer; a fraction gives a single float. The
Lisp reader is never involved, so no other syntax is accepted."
  (let* ((trimmed    (string-trim '(#\Space #\Tab) text))
         (negative-p (and (plusp (length trimmed)) (char= (char trimmed 0) #\-)))
         (start      (if (and (plusp (length trimmed)) (find (char trimmed 0) "+-")) 1 0))
         (point      (position #\. trimmed :start start))
         (whole      (subseq trimmed start (or point (length trimmed))))
         (fraction   (if point (subseq trimmed (1+ point)) "")))
    (flet ((digits-p (string)
             (every (lambda (character) (char<= #\0 character #\9)) string)))
      (when (and (digits-p whole)
                 (digits-p fraction)
                 (plusp (length whole))
                 (or (null point) (plusp (length fraction))))
        (let ((magnitude (if point
                             (float (+ (parse-integer whole)
                                       (/ (parse-integer fraction)
                                          (expt 10 (length fraction))))
                                    1.0)
                             (parse-integer whole))))
          (if negative-p (- magnitude) magnitude))))))

(-> setting--option-acceptable-p (setting t list) boolean)
(defun setting--option-acceptable-p (setting value options)
  "Return true when VALUE is among OPTIONS, OPTIONS are open, or an admitted NIL."
  (or (null options)
      (and (null value) (typep nil (setting-type setting)))
      (not (null (member value options :test #'equal)))))
