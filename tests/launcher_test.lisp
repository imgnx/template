#!/usr/bin/env -S sbcl --script
;;; Lightweight tests for Taku launcher commands

(require :asdf)

(defmacro with-temp-fdefinition ((symbol new-fn) &body body)
  "Temporarily replace SYMBOL's global function definition with NEW-FN during BODY."
  `(let ((orig (when (fboundp ,symbol) (symbol-function ,symbol))))
     (unwind-protect
          (progn
            (setf (symbol-function ,symbol) ,new-fn)
            ,@body)
       (when orig (setf (symbol-function ,symbol) orig)))))

(defun run-tests ()
  (let ((passes 0)
        (fails 0))
    (labels
        ((ok (name)
           (incf passes)
           (format t "[PASS] ~a~%" name))
         (bad (name why)
           (incf fails)
           (format t "[FAIL] ~a — ~a~%" name why)))

      ;; Load launcher components
      (load "config/taku-core.lisp")
      (load "config/taku-config.lisp")
      (load "config/taku-modules.lisp")
      (load "config/main.lisp")

      ;; Test 1: `./LAUNCHER module bitcrusher` triggers build-frontend-target with "bitcrusher"
      (handler-case
          (let ((called nil)
                (arg nil))
            (with-temp-fdefinition ('taku.modules::build-frontend-target
                                     (lambda (target)
                                       (setf called t arg target)
                                       :ok))
              (taku.launcher::cmd-module '("bitcrusher"))
              (if (and called (string= arg "bitcrusher"))
                  (ok "module builds bitcrusher via build-frontend-target")
                  (bad "module builds bitcrusher via build-frontend-target"
                       (format nil "called=~a arg=~a" called arg)))))
        (error (e) (bad "module command raised error" e)))

      ;; Test 2: `./LAUNCHER dev-module tuner` calls start-frontend with target "tuner" and starts backend
      (handler-case
          (let ((fe-called nil)
                (fe-target nil)
                (be-called nil))
            (with-temp-fdefinition ('taku.modules::start-frontend
                                     (lambda (&key root target)
                                       (declare (ignore root))
                                       (setf fe-called t fe-target target)
                                       :ok))
              (with-temp-fdefinition ('taku.modules::start-backend
                                       (lambda (&key root)
                                         (declare (ignore root))
                                         (setf be-called t)
                                         :ok))
                (taku.launcher::cmd-dev-module '("tuner"))
                (if (and fe-called (string= fe-target "tuner") be-called)
                    (ok "dev-module starts backend and frontend for tuner")
                    (bad "dev-module starts backend and frontend for tuner"
                         (format nil "fe-called=~a fe-target=~a be-called=~a"
                                 fe-called fe-target be-called))))))
        (error (e) (bad "dev-module command raised error" e)))

      (format t "~%Summary: ~d passed, ~d failed~%" passes fails)
      (uiop:quit (if (= fails 0) 0 1)))))

(run-tests)

