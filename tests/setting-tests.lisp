(in-package #:setinka)

;;;; -- Setting Tests --

(-> setting-tests--rejects-p (setting t &optional t) boolean)
(defun setting-tests--rejects-p (setting text &optional configuration)
  "Return true when parsing TEXT for SETTING signals SETTING-INVALID."
  (handler-case
      (progn
        (setting-parse setting text configuration)
        nil)
    (setting-invalid ()
      t)))

(define-test test-boolean-settings
  (let ((boolean (make-instance 'boolean-setting :name :flag :label "Flag")))
    (dolist (case '(("on" t) ("TRUE" t) ("yes" t) ("1" t)
                    ("off" nil) ("false" nil) ("" nil) ("0" nil) (t t) (nil nil)))
      (destructuring-bind (text expected) case
        (test-equal expected (setting-parse boolean text nil))))
    (test-assert (setting-tests--rejects-p boolean "maybe"))
    (test-equal '(t nil) (setting-options boolean nil))
    (test-equal "on" (setting-render-value boolean t))
    (test-equal "off" (setting-render-value boolean nil))))

(define-test test-choice-settings
  (let ((keyword-choice (make-instance 'choice-setting
                                       :name :mode
                                       :label "Mode"
                                       :type 'keyword
                                       :options '(:ask :auto)))
        (computed (make-instance 'choice-setting
                                 :name :computed
                                 :label "Computed"
                                 :type 'string
                                 :options (lambda (configuration)
                                            (list "low" configuration))))
        (optional (make-instance 'choice-setting
                                 :name :optional
                                 :label "Optional"
                                 :type '(or null keyword)
                                 :options '(:one))))
    (test-equal :auto (setting-parse keyword-choice "Auto" nil))
    (test-assert (setting-tests--rejects-p keyword-choice "sandbox"))
    (test-equal "ask" (setting-render-value keyword-choice :ask))
    (test-equal '("low" "high") (setting-options computed "high"))
    (test-equal "high" (setting-parse computed "high" "high"))
    (test-assert (setting-tests--rejects-p computed "medium" "high"))
    (test-equal nil (setting-parse optional nil nil))
    (let ((condition (test-signals setting-invalid (setting-parse keyword-choice :x nil))))
      (test-equal "Mode must be one of ask, auto, not x."
                  (princ-to-string condition))
      (test-equal :x (setting-invalid-value condition)))
    (test-equal "Mode does not accept \"x\"."
                (princ-to-string (test-signals setting-invalid
                                   (setting-parse keyword-choice "x" nil))))))

(define-test test-integer-and-pathname-settings
  (let ((bounded (make-instance 'integer-setting
                                :name :percent
                                :label "Percent"
                                :minimum 1
                                :maximum 95))
        (path (make-instance 'pathname-setting :name :path :label "Path")))
    (test-equal 80 (setting-parse bounded " 80 " nil))
    (dolist (text '("0" "96" "abc" "1.5"))
      (test-assert (setting-tests--rejects-p bounded text) text))
    (test-equal "Percent must be at least 1 and at most 95, not 0."
                (princ-to-string (test-signals setting-invalid
                                   (setting-parse bounded "0" nil))))
    (test-equal #p"state/prefs.sexp" (setting-parse path "state/prefs.sexp" nil))
    (test-equal "a/b" (setting-render-value path #p"a/b"))
    (test-equal "unset" (setting-render-value path nil))))

(define-test test-validator-delegates
  (let ((even (make-instance 'integer-setting
                             :name :even
                             :label "Even"
                             :validator (lambda (value configuration)
                                          (declare (ignore configuration))
                                          (and (oddp value)
                                               (format nil "~D is odd." value))))))
    (test-equal 4 (setting-parse even "4" nil))
    (test-equal "3 is odd." (princ-to-string (test-signals setting-invalid
                                               (setting-parse even "3" nil))))))

(define-test test-defaults
  (let ((plain (make-instance 'setting :name :plain :default 7))
        (computed (make-instance 'setting
                                 :name :computed
                                 :default (lambda (configuration) (* 2 configuration))))
        (keyword-default (make-instance 'setting :name :keyword-default :default :auto)))
    (test-equal 7 (setting-default-value plain nil))
    (test-equal 42 (setting-default-value computed 21))
    (test-equal :auto (setting-default-value keyword-default nil))))

(define-test test-registries
  (let ((registry (make-setting-registry)))
    (define-setting :first (boolean-setting :registry registry)
      :label "First"
      :default t)
    (define-setting :second (integer-setting :registry registry)
      :label "Second")
    (test-equal '(:first :second) (mapcar #'setting-name (settings-list registry)))
    (define-setting :first (boolean-setting :registry registry)
      :label "First again")
    (test-equal '(:first :second) (mapcar #'setting-name (settings-list registry)))
    (test-equal "First again" (setting-label (find-setting :first registry)))
    (let ((condition (test-signals setting-unknown (find-setting :missing registry))))
      (test-equal :missing (setting-unknown-name condition)))
    (test-signals setting-unknown (find-setting :first (make-setting-registry)))))
