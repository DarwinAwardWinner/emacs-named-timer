;;; named-timer.el --- Named timers for Emacs Lisp -*- lexical-binding: t -*-

;; Copyright (C) 2018 Ryan C. Thompson

;; Filename: named-timer.el
;; Author: Ryan C. Thompson
;; Created: Sat Nov 10 16:52:30 2018 (-0800)
;; Version: 0.1
;; Package-Requires: ()
;; URL:
;; Keywords:

;; This file is NOT part of GNU Emacs.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:

;; This library provides two new functions, `named-timer-run' and
;; `named-timer-idle-run'. They work like `run-with-timer' and
;; `run-with-idle-timer', except that they take an additional symbol
;; or keyword in front of all other arguments to use as name of the
;; timer. The main benefit of using a named timer is that re-using a
;; name automatically cancels the previous timer that was using that
;; name. This simplifies the common and somewhat error prone pattern
;; of cancelling and recreating the same timer when something changes.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation, either version 3 of the
;; License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs. If not, see <http://www.gnu.org/licenses/>.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(require 'timer)
(require 'cl-lib)
(require 'subr-x)

(defvar named-timer-table (make-hash-table)
  "A hash table mapping symbols to timers.")

(defsubst named-timer-names ()
  "Return a list of all timer names in `named-timer-table'."
  (hash-table-keys named-timer-table))
(defsubst named-timer-timers ()
  "Return a list of all named timers in `named-timer-table'.

Note that this may include both regular and idle timers."
  (hash-table-values named-timer-table))

(defsubst named-timer-get (name)
  "Return the timer named NAME, if any.

If no timer with the specified name exists, returns nil. See
`named-timer-table'."
  (cl-assert (symbolp name))
  (gethash name named-timer-table))

(defun named-timer-cancel (name &optional _noremove)
  "Cancel the timer with name NAME if it exists.

If NAME is the name of an active named timer, it is cancelled and
removed from `named-timer-table'. If no timer with that name
exists, nothing happens.

The second argument, _NOREMOVE, is intended for internal use
only. If non-nil, the entry for the cancelled timer in
`named-timer-table' is not removed. This is used for efficiency
in cases where that hash table entry is about to be replaced
anyway, such that deleting it is pointless."
  (let ((timer (named-timer-get name)))
    (when timer
      (timer--check timer)
      (cancel-timer timer)
      (unless _noremove
        (remhash name named-timer-table)))))

(defun named-timer-run (name &rest args)
  "Set up a named timer.

This creates a new timer by calling `run-with-timer' on ARGS,
then adds that timer to `named-timer-table' with NAME as the key.
Like `run-with-timer', this returns the created timer.

Only one timer with a given name can be active. Calling this
function on a name that is already associated with an existing
timer will cancel the existing timer and replace it with the new
one in `named-timer-table'."
  (cl-assert (symbolp name))
  (named-timer-cancel name t)
  (let ((timer (apply #'run-with-timer args)))
    (timer--check timer)
    (puthash name timer named-timer-table)))

(defun named-timer-idle-run (name &rest args)
  "Set up a named idle timer.

This works like `named-timer-run', but passes ARGS to
`run-with-idle-timer' instead. The timer is added to
`named-timer-table' with NAME as the key."
  (cl-assert (symbolp name))
  (named-timer-cancel name t)
  (let ((timer (apply #'run-with-idle-timer args)))
    (timer--check timer)
    (puthash name timer named-timer-table)))

;; Non-prefixed aliases that match the corresponding function names in
;; `timer.el'. Not sure if they should be kept or removed.
(defalias 'run-with-named-timer 'named-timer-run)
(defalias 'run-with-named-idle-timer 'named-timer-idle-run)
(defalias 'cancel-named-timer 'named-timer-cancel)

(define-advice cancel-timer (:around (orig-fun &rest args) named-timer-cancel)
  "Allow cancelling named timers by name.

This advice allows `cancel-timer' to accept a symbol argument
instead of a timer object. A symbol argument will be passed
instead to `named-timer-cancel' to cancel a timer with that
name."
  (let ((timer-or-name (car args)))
    (if (symbolp timer-or-name)
        (named-timer-cancel timer-or-name)
      (apply orig-fun args))))

(provide 'named-timer)

;;; named-timer.el ends here
