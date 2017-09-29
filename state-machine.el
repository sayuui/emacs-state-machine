;; state-machine.el --- State machine for Emacs
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
;;             Simple state machine for Emacs.
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
(require 'state-machine--state)

(defconst --state-machine-identifier (cl-gensym "state-machine-")
  "Non-interned identifier for state machine objects.")

(defun state-machine--callable-p (arg)
  "Return t if ARG is a callable entity"
  (or (functionp arg)))

(cl-defun state-machine-create (&key ((:test test-function) 'eq))
  "Create state machine"
  (let ((--initial-state (state-machine-state-create)))
    `(,--state-machine-identifier
      ,--initial-state
      ,--initial-state
      ,test-function)))

(defun state-machine-initial-state (state-machine)
  "Returns initial state of state machine"
  (unless (state-machine-p state-machine)
    (error "state-machine-initial-state: STATE-MACHINE is not valid"))

  (nth 1 state-machine))

(defun state-machine-current-state (state-machine)
  "Returns state machine"
  (unless (state-machine-p state-machine)
    (error "state-machine-current-state: STATE-MACHINE is not valid"))

  (nth 2 state-machine))

(defun state-machine-test-function (state-machine)
  "Returns test function"
  (unless (state-machine-p state-machine)
    (error "state-machine-test-function: STATE-MACHINE is not valid"))

  (nth 3 state-machine))

(defun state-machine-p (arg)
  "Return t if ARG is a state machine."
  (and (listp arg)
       (eq (car arg)
           --state-machine-identifier)))

(defun state-machine-add (state-machine triggers callable &optional end-state)
  "Associate CALLABLE with TRIGGERS."
  (unless (state-machine-p state-machine)
    (error "state-machine-add: STATE-MACHINE is not valid"))

  (unless (and (listp triggers)
               (> (length triggers) 0))
    (error "state-machine-add: TRIGGERS is not a list of triggers"))

  (let ((--test-function (state-machine-test-function state-machine))
        (--state      (state-machine-initial-state state-machine))
        (--triggers   triggers)
        (--prev-state)
        (--trigger))
    (while --triggers
      (setq --prev-state --state)
      (setq --trigger    (car --triggers))
      (setq --triggers   (cdr --triggers))
      (setq --state      (state-machine-state-next --state
                                                   --trigger
                                                   :test --test-function))
      (unless --state
        (setq --state
              (state-machine-state-next
               --prev-state
               --trigger
               :test --test-function
               :set  (state-machine-state-create :callable (if --triggers
                                                               nil
                                                             callable)
                                                 :end      (and (not --triggers)
                                                                (not (null end-state))))))))))

(defun state-machine-get (state-machine triggers)
  "Get state of STATE-MACHINE associated with TRIGGERS"
  (unless (state-machine-p state-machine)
    (error "state-machine-get: STATE-MACHINE is not valid"))

  (unless (and (listp triggers)
               (> (length triggers) 0))
    (error "state-machine-get: TRIGGERS is not a list of triggers"))

  (let ((--state    (state-machine-initial-state state-machine))
        (--triggers triggers)
        (--trigger))
    (while (and --triggers
                --state)
      (setq --trigger (car --triggers))
      (setq --triggers (cdr --triggers))
      (setq --state
            (state-machine-state-next --state
                                      --trigger
                                      :test 'eq)))
    --state))

(defun state-machine-excite (state-machine trigger)
  "Excite state machine means changing it's current state."
  (unless (state-machine-p state-machine)
    (error "state-machine-excite: STATE-MACHINE is not valid"))

  (let ((--state (state-machine-state-next (state-machine-current-state state-machine)
                                           trigger
                                           :test (state-machine-test-function state-machine)))
        (--initial-state))
    (when --state
      (if (not (state-machine-state-end-p --state))
          (setf (nth 2 state-machine) --state)
        (state-machine-state-call --state)
        (setq --initial-state (state-machine-state-fallback --state))
        (unless --initial-state
          (setq --initial-state (state-machine-initial-state state-machine)))
        (setf (nth 2 state-machine) --initial-state)))))

(provide 'state-machine)
;;; state-machine.el ends here
