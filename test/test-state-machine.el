;; test-state-machine.el --- Tests
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
;; Tests for `state-machine.el'.
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

(require 'buttercup)
(require 'state-machine)
(require 'cl-lib)

(describe "State machine"
  (describe "state-machine-create"
    (it "Creates state machine"
      (let ((sm (state-machine-create)))
        (expect (state-machine-p sm)
                :to-be t)))

    (it "Can set test function"
      (let ((sm (state-machine-create :test 'equal)))
        (expect (state-machine-test-function sm)
                :to-be 'equal))))

  (describe "state-machine-initial-state"
    (it "Raises an error if first arg is not a state machine"
      (expect (state-machine-initial-state 1)
              :to-throw))

    (it "Returns initial state of state machine"
      (let* ((state-machine (state-machine-create))
             (initial-state (nth 1 state-machine)))
        (expect (state-machine-initial-state state-machine)
                :to-be initial-state))))

  (describe "state-machine-test-function"
    (it "Raises an error if first arg is not a state machine"
      (expect (state-machine-test 1)
              :to-throw))

    (it "Returns test function of state machine"
      (let ((state-machine (state-machine-create :test 'eq)))
        (expect (state-machine-test-function state-machine)
                :to-be 'eq)))
    )

  (describe "state-machine-get"
    (describe "Raises an error"
      (it "when first arg is not a state machine"
        (expect (state-machine-add 1
                                   '(1 2 3)
                                   (lambda ()
                                     ()))
                :to-throw))

      (it "when second arg is not a list of triggers"
        (expect (state-machine-add (state-machine-create)
                                   1
                                   (lambda ()
                                     ()))
                :to-throw)
        (expect (state-machine-add (state-machine-create)
                                   '()
                                   (lambda ()
                                     ()))
                :to-throw)))

    (it "Returns state"
      (let* ((--sm            (state-machine-create :test 'eq))
             (--initial-state ))
        (state-machine-add --sm '(1) (lambda ()
                                       ()))
        (setq --initial-state (state-machine-initial-state --sm))
        (expect (state-machine-get --sm '(1))
                :not :to-be nil)
        (expect (state-machine-get --sm '(1))
                :to-be (state-machine-state-next --initial-state 1
                                                 :test 'eq)))))

  (describe "state-machine-add"
    (describe "Raises an error"
      (it "when first arg is not a state machine"
        (expect (state-machine-add 1
                                   '(1 2 3)
                                   (lambda ()
                                     ()))
                :to-throw))

      (it "when second arg is not a list of triggers"
        (expect (state-machine-add (state-machine-create)
                                   1
                                   (lambda ()
                                     ()))
                :to-throw)
        (expect (state-machine-add (state-machine-create)
                                   '()
                                   (lambda ()
                                     ()))
                :to-throw))
      (it "raises"
        (expect (state-machine-add (state-machine-create)
                                   '(1)
                                   1)
                :to-throw))

      (describe "when third arg is not one of:"
        (it "lambda"
          (expect (state-machine-add (state-machine-create)
                                     '(1)
                                     (lambda ()
                                       ()))
                  :not :to-throw))))

    (it "Adds states associated with triggers in state machine"
      (let* ((--sm       (state-machine-create)))
        (state-machine-add --sm '(1 2 3 4) nil)
        (expect (state-machine-state-p
                 (state-machine-get --sm '(1)))
                :to-be t)
        (expect (state-machine-state-p
                 (state-machine-get --sm '(1 2)))
                :to-be t)
        (expect (state-machine-state-p
                 (state-machine-get --sm '(1 2 3)))
                :to-be t)
        (expect (state-machine-state-p
                 (state-machine-get --sm '(1 2 3 4)))
                :to-be t)))

    (it "Can add callable to the last state"
      (let ((--sm (state-machine-create))
            (--lambda (lambda ()
                        ())))
        (state-machine-add --sm '(1 2 3) --lambda)
        (expect (state-machine-state-callable (state-machine-get --sm
                                                                 '(1 2 3)))
                :to-be --lambda)))

    (it "Can set last state as end-state"
      (let ((--sm (state-machine-create)))
        (state-machine-add --sm
                           '(1 2)
                           nil
                           :end-state)
        (expect (state-machine-state-end-p (state-machine-get --sm
                                                              '(1)))
                :to-be nil)
        (expect (state-machine-state-end-p (state-machine-get --sm
                                                              '(1 2)))
                :to-be t)))

    (it "Can replace state if it's already exists"
      (let* ((--sm       (state-machine-create))
             (--sync     0)
             (--lambda-1 (lambda ()
                           (setq --sync 1)))
             (--lambda-2 (lambda ()
                           (setq --sync 2))))
        (state-machine-add --sm
                           '(1 2 3)
                           --lambda-1
                           :end)
        (state-machine-add --sm
                           '(1 2 3)
                           --lambda-2
                           :end)
        (state-machine-excite --sm 1)
        (state-machine-excite --sm 2)
        (state-machine-excite --sm 3)
        (expect --sync
                :to-be 2))))

  (describe "state-machine-current-state"
    (it "Raises an error if first arg is not a state"
      (expect (state-machine-current-state 1)
              :to-throw))

    (it "Returns initial state, when state machine was not excited"
      (let ((sm (state-machine-create)))
        (expect (eq (state-machine-initial-state sm)
                    (state-machine-current-state sm))
                :to-be t))))

  (describe "state-machine-excite"
    (it "Raises an error if first arg is not a state"
      (expect (state-machine-excite 1
                                    1)
              :to-throw))

    (it "Changes current state"
      (let* ((sm       (state-machine-create)))
        (state-machine-add sm '(1 2 3 4) nil)
        (state-machine-excite sm 1)
        (expect (eq (state-machine-current-state sm)
                    (state-machine-get sm '(1)))
                :to-be t)
        (state-machine-excite sm 2)
        (expect (eq (state-machine-current-state sm)
                    (state-machine-get sm '(1 2)))
                :to-be t)
        (state-machine-excite sm 3)
        (expect (eq (state-machine-current-state sm)
                    (state-machine-get sm '(1 2 3)))
                :to-be t)
        (state-machine-excite sm 4)
        (expect (eq (state-machine-current-state sm)
                    (state-machine-get sm '(1 2 3 4)))
                :to-be t)
        ))

    (it "When end state is achieved, calls its' callable."
      (let* ((--sync   nil)
             (--sm     (state-machine-create))
             (--lambda (lambda ()
                         (setq --sync t))))
        (state-machine-add --sm '(1 2) --lambda :end-state)
        (state-machine-excite --sm 1)
        (state-machine-excite --sm 2)
        (expect --sync
                :to-be t)))

    (it "When end state is achieved, changes current state to its' fallback state"
      (let ((--sm (state-machine-create)))
        (state-machine-add --sm '(1 2) nil :end)
        (state-machine-state-fallback (state-machine-get --sm '(1 2))
                                      :set (state-machine-get --sm '(1)))
        (state-machine-excite --sm 1)
        (state-machine-excite --sm 2)
        (expect (state-machine-current-state --sm)
                :to-be (state-machine-get --sm '(1)))))

    (it "When end state is achieved and fallback state is nil, changes current state to initial state"
      (let ((--sm (state-machine-create)))
        (state-machine-add --sm '(1 2) nil :end)
        (state-machine-excite --sm 1)
        (state-machine-excite --sm 2)
        (expect (state-machine-current-state --sm)
                :to-be (state-machine-initial-state --sm))))
    )

  (describe "state-machine-prefix-p"
    (it "Raises an error when first arg is not state machine"
      (expect (state-machine-prefix-p 1
                                      'a)
              :to-throw))

    (it "Return t if trigger leads to a prefix state"
      (let ((--sm (state-machine-create)))
        (state-machine-add --sm
                           '(a r s)
                           nil)
        (state-machine-add --sm
                           '(r s t)
                           nil)
        (expect (state-machine-prefix-p --sm
                                        'a)
                :to-be t)
        (expect (state-machine-prefix-p --sm
                                        'r)
                :to-be t)
        (expect (state-machine-prefix-p --sm
                                        's)
                :to-be nil)
        ))
    )
  )



;; Local Variables:
;; eval: (put 'describe    'lisp-indent-function 'defun)
;; eval: (put 'it          'lisp-indent-function 'defun)
;; eval: (put 'before-each 'lisp-indent-function 'defun)
;; eval: (put 'after-each  'lisp-indent-function 'defun)
;; eval: (put 'before-all  'lisp-indent-function 'defun)
;; eval: (put 'after-all   'lisp-indent-function 'defun)
;; End:
