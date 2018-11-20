# named-timer.el - Simplified timer management for Emacs Lisp

This library provides functions for managing named timers. The
usual pattern for timer management in Emacs Lisp involves declaring
a timer variable, checking it for an existing timer, cancelling
that timer if it exists, setting the varaible to nil to indicate that
the timer is no longer active, and finally starting a new timer and
setting the variable to that value. For example:

```elisp
(require 'timer)
(defvar my-timer nil)
(defun activate-my-timer ()
  (when my-timer
    (cancel-timer my-timer)
    (setq my-timer nil))
  (setq my-timer
        (run-with-timer 5 nil #'message "My timer ran!")))
```

With named timers, this simplifies to a single line:

```elisp
(require 'named-timer)
(defun activate-my-timer ()
  (named-timer-run :my-timer 5 nil #'message "My timer ran!"))
```

In addition to being shorter, this code is less error prone: since
running a named timer automatically cancels any existing timer with
the same name, there is no chance of accidentally leaving multiple
timers by forgetting to cancel old timers. In short, all the
functions in this library are idempotent, which makes them much
easier to reason about.

The basic functions for managing named timers `named-timer-run`~,
`named-timer-idle-run`, and `named-timer-cancel`, which are,
respectively, analogues of `run-with-timer`, `run-with-idle-timer`,
and `cancel-timer`.
