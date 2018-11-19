# named-timer.el - Named timers for Emacs

This library provides two new functions, `named-timer-run` and
`named-timer-idle-run`. They work like `run-with-timer` and
`run-with-idle-timer`, except that they take an additional symbol or
keyword in front of all other arguments to use as name of the timer.
The main benefit of using a named timer is that re-using a name
automatically cancels the previous timer that was using that name.
This simplifies the common and somewhat error-prone pattern of
cancelling and recreating the same timer when something changes.
