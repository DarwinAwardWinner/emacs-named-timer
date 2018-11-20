;;; -*- lexical-binding: t -*-

(require 'named-timer)
(require 'buttercup)
(require 'cl-lib)
(require 'with-simulated-input)
(require 's)

(defun cleanup-test-named-timers ()
  (cl-loop for tname in (named-timer-names)
           if (s-matches? "^:?nt-test" (symbol-name tname))
           do (named-timer-cancel tname)))

(describe "The `named-timer' package"

  (before-each (cleanup-test-named-timers))
  (after-each (cleanup-test-named-timers))

  (it "should be able to create and cancel a named timer"
    ;; Create a timer
    (expect
     (timerp (named-timer-run :nt-test 500 nil #'ignore 1 2 3)))
    (expect (timerp (named-timer-get :nt-test)))
    ;; Cancel the timer
    (named-timer-cancel :nt-test)
    (expect (named-timer-get :nt-test) :not :to-be-truthy)))

;;; test-named-timer.el ends here
