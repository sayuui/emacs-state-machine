;; test---state.el --- Tests for `--state.el'
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
;; Tests for `--state.el'.
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
(require 'state-machine--state)

(describe "State node"
  (describe "state-machine-state-create"
    (it "Creates a state-machine state"
      (let ((--state (state-machine-state-create)))
        (expect (state-machine-state-p --state)
                :to-be t)))

    (it "Can create end states"
      (let ((--prefix-state (state-machine-state-create))
            (--end-state    (state-machine-state-create :end t)))
        (expect (state-machine-state-end-p --prefix-state)
                :to-be nil)
        (expect (state-machine-state-end-p --end-state)
                :to-be t)))

    (it "Can set callables"
      (let* ((--callable (lambda ()
                           ()))
             (--state (state-machine-state-create :callable --callable)))
        (expect (state-machine-state-callable --state)
                :to-be --callable)))

    (it "Can set fallback state"
      (let* ((--fallback (state-machine-state-create))
             (--state    (state-machine-state-create :fallback --fallback)))
        (expect (state-machine-state-fallback --state)
                :to-be --fallback)))
    )

  (describe "state-machine-state-end-p"
    (it "Can determine between end and prefix states"
      (let ((--end-state    (state-machine-state-create :end t))
            (--prefix-state (state-machine-state-create)))
        (expect (state-machine-state-end-p --prefix-state)
                :to-be nil)
        (expect (state-machine-state-end-p --end-state)
                :to-be t)))
    )

  (describe "state-machine-state-fallback"
    (it "Raises an error if state is not valid"
      (expect (state-machine-state-callable 1)
              :to-throw))

    (it "Can get and set state fallback state"
      (let ((--fallback (state-machine-state-create))
            (--state    (state-machine-state-create)))
        (expect (state-machine-state-fallback --state)
                :to-be nil)
        (expect (state-machine-state-fallback --state
                                              :set --fallback)
                :to-be --fallback)
        (expect (state-machine-state-fallback --state)
                :to-be --fallback)
        ))
    )

  (describe "state-machine-state-next"
    (describe "Raises exceptions"
      (it "when first arg is not a valid state"
        (expect (state-machine-state-next 1
                                          "a")
                :to-throw))
      (it "when third arg is not nil and is not a valid state"
        (expect (state-machine-state-next (state-machine-state-create)
                                          "a"
                                          1)
                :to-throw)))

    (it "Can set and get next state."
      (let ((--state-1 (state-machine-state-create))
            (--state-2 (state-machine-state-create))
            (--state-3 (state-machine-state-create))
            (--state-4 (state-machine-state-create)))
        (state-machine-state-next --state-1 "a"
                                  :set  --state-2
                                  :test 'string=)
        (state-machine-state-next --state-1 "r"
                                  :set  --state-3
                                  :test 'string=)
        (state-machine-state-next --state-1 "s"
                                  :set  --state-4
                                  :test 'string=)

        (expect (state-machine-state-next --state-1 "a"
                                          :test 'string=)
                :to-be --state-2)
        (expect (state-machine-state-next --state-1 "r"
                                          :test 'string=)
                :to-be --state-3)
        (expect (state-machine-state-next --state-1 "s"
                                          :test 'string=)
                :to-be --state-4)))

    (it "Always returns state if any or nil if not found"
      (let ((--state-1 (state-machine-state-create))
            (--state-2 (state-machine-state-create))
            (--state-3 (state-machine-state-create)))
        (expect (state-machine-state-p (state-machine-state-next --state-1
                                                                 1
                                                                 :set --state-2))
                :to-be t)
        (expect (state-machine-state-p (state-machine-state-next --state-1
                                                                 1
                                                                 :set --state-3))
                :to-be t)
        (expect (state-machine-state-p (state-machine-state-next --state-1
                                                                 1))
                :to-be t)
        )))

  (describe "state-machine-state-callable"
    (describe "Raises an error"
      (it "when state is not valid"
        (expect (state-machine-state-callable 1)
                :to-throw))

      (it "when callable provided but invalid"
        (expect (state-machine-state-callable (state-machine-state-create)
                                              :set 1)
                :to-throw)))

    (it "Can get/set callable"
      (let ((--state    (state-machine-state-create))
            (--callable (lambda ()
                          ())))
        (state-machine-state-callable --state
                                      :set --callable)
        (expect (state-machine-state-callable --state)
                :to-be --callable))))

  (describe "state-machine-state-call"
    (it "Raises an error if state is not valid"
      (expect (state-machine-state-callable 1)
              :to-throw))

    (it "Calls the callable of the state"
      (let* ((--sync nil)
             (--lambda (lambda ()
                         (setq --sync t)))
             (--state (state-machine-state-create :callable --lambda)))
        (state-machine-state-call --state)
        (expect --sync
                :to-be t)))
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
