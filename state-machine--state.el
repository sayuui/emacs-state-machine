;; --state.el --- State
;;
;; Author: Minae Yui <minae.yui.sain@gmail.com>
;; Version: 0.1
;; URL: todo
;; Keywords:
;; Compatibility:
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Commentary:
;; Package that contains state node for state machine.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(require 'cl-lib)

(defconst --state-machine-state-identifier (cl-gensym "state-machine-state-")
  "Non-interned identifier for state-machine states")

(defun state-machine-state--callable-p (arg)
  "Return t if ARG is a callable"
  (or (null arg)
      (functionp arg)))

(cl-defun state-machine-state-create (&key ((:end end-state)           nil)
                                           (callable                   nil callable-p)
                                           ((:fallback fallback-state) nil))
  "Create new state-machine state.
If END-STATE is t, then it's an `end' state.
Otherwise, it's a `prefix' state."
  (when (and callable-p
             (not (state-machine-state--callable-p callable)))
    (error "state-machine-state-callable: CALLABLE is not valid"))

  `(,--state-machine-state-identifier
    ,(if end-state
         t
       nil)
    ,callable
    ,fallback-state
    ,(list)))

(defun state-machine-state-p (arg)
  "Return t if ARG is a state-machine state"
  (and (listp arg)
       (eq (car arg)
           --state-machine-state-identifier)))

(defun state-machine-state-end-p (arg)
  "Return t if ARG is a end state-machine state"
  (and (state-machine-state-p arg)
       (cadr arg)))

(cl-defun state-machine-state-callable (state &key
                                              ((:set callable) nil callable-p))
  "Get or set callable of the STATE"
  (unless (state-machine-state-p state)
    (error "state-machine-state-callable: STATE is not valid"))

  (when (and callable-p
             (not (state-machine-state--callable-p callable)))
    (error "state-machine-state-callable: CALLABLE is not valid"))

  (if callable-p
      (setf (nth 2 state) callable)
    (nth 2 state)))

(cl-defun state-machine-state-fallback (state &key
                                              ((:set fallback-state) nil fallback-state-p))
  ""
  (unless (state-machine-state-p state)
    (error "state-machine-state-fallback: STATE is not valid"))

  (if (not fallback-state-p)
      (nth 3 state)
    (setf (nth 3 state) fallback-state)
    fallback-state))

(cl-defun state-machine-state-next (state trigger
                                          &key
                                          ((:set next-state) nil)
                                          (test              'eq))
  "If NEXT-STATE is a valid state, set it as next state of STATE
associated with KEYSTROKE.
If NEXT-STATE is nil, return next state of STATE associated with TRIGGER.
STATE must be a valid state.
KEYSTROKE must be a valid keystroke."
  (unless (state-machine-state-p state)
    (error "state-machine-state-next: STATE is not a valid state"))

  (when next-state
    (unless (state-machine-state-p next-state)
      (error "state-machine-state-next: NEXT-STATE is not nil or valid state")))

  (let* ((--next-states (nth 4 state))
         (--length      (length --next-states))
         (--index       0)
         (--found)
         (--temp))
    (while (and (not --found)
                (< --index --length))
      (setq --temp (assoc trigger --next-states))
      (when (funcall test
                     (car --temp)
                     trigger)
        (setq --found --temp))
      (setq --index (1+ --index)))

    (if (not next-state)
        (cdr --found)
      (if --found
          (setcdr --found state)
        (setf (nth 4 state)
              (cons `(,trigger . ,next-state) --next-states)))
      next-state)))

(defun state-machine-state-call (state)
  "Call the callable of the state"
  (let ((--callable (state-machine-state-callable state)))
    (cond
     ((null --callable)
      t)
     ((functionp --callable)
      (funcall --callable))
     (t nil))))

(provide 'state-machine--state)
;;; state-machine--state.el ends here
