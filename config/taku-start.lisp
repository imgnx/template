#!/usr/bin/env -S sbcl --script 
;;; Taku Starter (LISP) v2 — portable bins check (no uiop:find-program)
 
(require :asdf) 

(uiop:define-package :taku.start (:use :cl :uiop)) 

(in-package :taku.start) 

(defun repo-root () 
  (or 
    (and *load-truename* 
      (pathname-directory-pathname *load-truename*)) 
    (ensure-directory-pathname (getcwd)))) 

(defun subdir (root rel) 
  (ensure-directory-pathname 
    (merge-pathnames rel root))) 

(defun echo (&rest parts) 
  (format t "~&[拓] ~{~a~}~%" parts)) 

(defun spawn 
  (argv &key (dir (repo-root))) 
  (format t "~&[拓] spawn: ~{~a~^ ~}~%" argv) 
  (launch-program argv :directory dir :output :interactive :error-output :interactive :search t)) 

(defun wait-any (&rest procs) 
  (loop do (sleep 0.5) do 
    (dolist (p procs) 
      (unless (process-alive-p p) 
        (return-from wait-any p))))) 

(defun terminate-all (&rest procs) 
  (dolist (p procs) 
    (ignore-errors (terminate-process p)))) 

(defun have-bin-p (b) 
  (let* 
    (
      (cmd 
        (list "sh" "-lc" 
          (format nil "command -v ~a >/dev/null 2>&1 && echo OK || echo NO" b))) 
      (out 
        (run-program cmd :ignore-error-status t :output '(:string :stripped t)))) (string= out "OK"))) 

(defun ensure-bins (&rest bins) 
  (dolist (b bins) 
    (unless (have-bin-p b) 
      (error "Required program '~a' not found on PATH." b)))) 

(defun cmd-backend () '("python" "-m" "uvicorn" "app.main:app" "--reload" "--port" "8000"))

(defun cmd-frontend (&optional (target "modular"))
  ;; Run npm dev with specific target, redirect output to frontend.log
  (list "sh" "-c" (format nil "npm run dev:~a > frontend.log 2>&1" target)))

(defun cmd-build-frontend (&optional (target "modular"))
  ;; Build frontend with specific target
  (list "sh" "-c" (format nil "npm run build:~a" target)))

(defun cmd-build-rust () '("cargo" "build")) 

(defun cmd-run-cli (bits rate) 
  (list "cargo" "run" "-p" "taku_cli" "--" "--bits" 
    (princ-to-string bits) "--rate" 
    (princ-to-string rate))) 

(defun start-backend 
  (&key (root (repo-root)))
  (ensure-bins "python")
  ;; Run from repo root so app.main:app resolves to app/main.py
  (spawn (cmd-backend) :dir root))


(defun start-frontend 
  (&key (root (repo-root)) (target "modular"))
  (ensure-bins "npm")
  (spawn (cmd-frontend target) :dir root))

(defun build-frontend
  (&key (root (repo-root)) (target "modular"))
  (ensure-bins "npm")
  (spawn (cmd-build-frontend target) :dir root))

(defun build-rust 
  (&key (root (repo-root))) (ensure-bins "cargo") 
  (spawn (cmd-build-rust) :dir (subdir root "rust/"))) 

(defun run-cli 
  (&key (root (repo-root)) (bits 12) (rate 16000)) (ensure-bins "cargo") 
  (spawn 
    (cmd-run-cli bits rate) :dir (subdir root "rust/"))) 

(defun tmux-dev 
  (&key (root (repo-root)) (target "modular")) 
  (ensure-bins "tmux" "python" "npm") 
  (let 
    (
      (cmd1 '("tmux" "new-session" "-d" "-s" "taku" "-c" "." "python -m uvicorn app.main:app --reload --port 8000")) 
    (cmd2 (list "tmux" "split-window" "-h" "-t" "taku:0" "-c" "frontend" (format nil "npm run dev:~a" target))) 
  (cmd3 '("tmux" "select-layout" "-t" "taku:0" "even-horizontal")) 

(cmd4 '("tmux" "set-option" "-t" "taku" "mouse" "on")) 

(cmd5 '("tmux" "attach-session" "-t" "taku"))) 

(dolist 
  (c 
    (list cmd1 cmd2 cmd3 cmd4 cmd5)) (spawn c :dir root)))) 

(defun usage () 
  (echo "Usage: sbcl --script taku-start.lisp [COMMAND] [OPTIONS]")
  (echo "")
  (echo "Commands:")
  (echo "  dev [--target TARGET]         # dev (frontend+backend)")
  (echo "  backend                       # backend only")
  (echo "  frontend [--target TARGET]    # frontend only")
  (echo "  build-frontend [--target TARGET] # build frontend")
  (echo "  build-rust                    # build rust")
  (echo "  run-cli [--bits N --rate HZ]  # run CLI")
  (echo "  tmux [--target TARGET]        # side-by-side panes")
  (echo "")
  (echo "Targets: bitcrusher, tuner, modular (default)")
  (quit 1)) 

(defun parse-args () 
  (let* 
    (
      (argv #+sbcl 
        (cdr sb-ext:*posix-argv*) #-sbcl nil) 
      (cmd 
        (or (first argv) "dev")) 
      (rest (rest argv)) 
      (bits 12) 
      (rate 16000)
      (target "modular")) 
    (labels 
      (
        (getopt (name default) 
          (let 
            (
              (pos 
                (position name rest :test #'string=))) 
            (if pos 
              (let 
                (
                  (v (nth (1+ pos) rest))) 
                (if v 
                  (if (stringp default) v (parse-integer v :junk-allowed t)) 
                  default)) 
              default))))
      (setf target (getopt "--target" target))
      (when 
        (string= cmd "run-cli") 
        (setf bits 
          (getopt "--bits" bits) rate 
          (getopt "--rate" rate))) 
      (list :cmd 
        (intern (string-upcase cmd) :keyword) :bits bits :rate rate :target target)))) 

(defun main () 
  (let* 
    ((root (repo-root)) (opts (parse-args)) 
      (cmd (getf opts :cmd))
      (target (getf opts :target))) 
    (echo "root: " (namestring root))
    (when target (echo "target: " target))
    (ecase cmd 
      (:DEV 
        (echo "Starting backend + frontend (" target ")…") 
        (let* 
          (
            (p-back 
              (start-backend :root root)) 
            (p-front 
              (start-frontend :root root :target target))) #+sbcl 
          (push 
            (lambda () 
              (terminate-all p-back p-front)) sb-ext:*exit-hooks*) 
          (let 
            (
              (dead 
                (wait-any p-back p-front))) 
            (echo "Process exited: " dead) 
            (terminate-all p-back p-front)))) 
      (:BACKEND 
        (start-backend :root root) (wait-any)) 
      (:FRONTEND 
        (start-frontend :root root :target target) (wait-any)) 
      (:BUILD-FRONTEND
        (let
          (
            (p
              (build-frontend :root root :target target))) (wait-any p)))
      (:BUILD-RUST 
        (let 
          (
            (p 
              (build-rust :root root))) (wait-any p))) 
      (:RUN-CLI 
        (let* 
          (
            (bits (getf opts :bits)) 
            (rate (getf opts :rate)) 
            (p 
              (run-cli :root root :bits bits :rate rate))) (wait-any p))) 
      (:TMUX (tmux-dev :root root :target target)))))

(handler-case (main) 
  (error (e) (echo "ERROR: " e) (usage)))
