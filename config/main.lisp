#!/usr/bin/env -S sbcl --script
;;; Taku Build System LAUNCHER
;;; Main entry point for managing all modules in the Taku audio processing framework

(require :asdf)
(load "taku-core.lisp")

(uiop:define-package :taku.launcher (:use :cl :uiop :taku.core))
(in-package :taku.launcher)

;;; Load global config and modules
(load "taku-config.lisp")
(load "taku-modules.lisp")
(load "taku-codegen.lisp")

;; Import additional packages
(use-package :taku.modules)
(use-package :taku.codegen)

(defun show-banner ()
  (echo "")
  (echo "🎛️  T A K U   B U I L D   S Y S T E M")
  (echo "    Modular Audio Processing Framework")
  (echo "    Version: " (getf taku.core::*taku-config* :version))
  (echo ""))

(defun show-modules ()
  (echo "📦 Available Modules:")
  (dolist (module (getf taku.core::*taku-config* :modules))
    (let ((name (getf module :name))
          (desc (getf module :description))
          (status (or (getf module :status) :active)))
      (echo (format nil "   ~a ~a - ~a ~a" 
                   (if (eq status :active) "✅" "🚧")
                   (string-capitalize (symbol-name name))
                   desc
                   (if (eq status :planned) "(planned)" ""))))))

(defun show-commands ()
  (echo "")
  (echo " Available Commands:")
  (echo "   dev [MODULE...]           - Start development servers")
  (echo "   build [MODULE...]         - Build specified modules") 
  (echo "   deploy MODULE TARGET      - Deploy module to target")
  (echo "   generate MODULE TYPE      - Generate boilerplate code")
  (echo "   validate [MODULE...]      - Validate module configurations")
  (echo "   clean [MODULE...]         - Clean build artifacts")
  (echo "   status                    - Show system status")
  (echo "   tmux [MODULE...]          - Launch in tmux session")
  (echo "")
  (echo "🎯 Module Targets:")
  (echo "   frontend                  - JavaScript/React components")
  (echo "   backend                   - Python/FastAPI endpoints") 
  (echo "   rust                      - Rust DSP libraries")
  (echo "   all                       - All components")
  (echo "")
  (echo "Examples:")
  (echo "   ./LAUNCHER dev bitcrusher tuner")
  (echo "   ./LAUNCHER build frontend")
  (echo "   ./LAUNCHER generate reverb rust-crate")
  (echo "   ./LAUNCHER deploy bitcrusher production"))

(defun launcher-usage ()
  (show-banner)
  (show-modules)
  (show-commands)
  (quit 1))

;;; COMMAND HANDLERS
(defun cmd-dev (modules)
  (echo "🔥 Starting development environment...")
  (cond
    ((null modules)
     (echo "Starting all active modules")
     (dev-all-modules))
    ((= (length modules) 1)
     (dev-single-module (first modules)))
    (t
     (dev-multiple-modules modules))))

(defun cmd-build (targets)
  (echo "🔨 Building components...")
  (if (null targets)
      (build-all-components)
      (dolist (target targets)
        (build-component target))))

;; Build only a single module's frontend bundle (e.g., `./LAUNCHER module bitcrusher`).
(defun cmd-module (args)
  (if (or (null args) (null (first args)))
      (error "module requires: NAME (e.g., bitcrusher)")
      (build-module (first args))))

(defun cmd-generate (module-name type)
  (echo "⚡ Generating code for " module-name "...")
  (case (intern (string-upcase type) :keyword)
    (:RUST-CRATE (generate-rust-crate module-name))
    (:FRONTEND-COMPONENT (generate-frontend-component module-name))
    (:API-ENDPOINTS (generate-api-endpoints module-name))
    (:MODULE-CONFIG (generate-module-config module-name))
    (t (error "Unknown generation type: ~a" type))))

(defun cmd-validate (modules)
  (echo "🔍 Validating modules...")
  (let ((modules-to-check (or modules (mapcar (lambda (m) (getf m :name))
                                             (getf taku.core::*taku-config* :modules)))))
    (dolist (module modules-to-check)
      (validate-module module))))

(defun cmd-status ()
  (echo "📊 System Status:")
  (check-system-dependencies)
  (check-module-health)
  (show-build-status))

(defun cmd-clean (modules)
  (echo "🧹 Cleaning build artifacts...")
  (if modules
      (dolist (module modules) (clean-module module))
      (clean-all-modules)))

;;; ARGUMENT PARSING
(defun parse-launcher-args ()
  (let* ((argv #+sbcl (cdr sb-ext:*posix-argv*) #-sbcl nil)
         (cmd (or (first argv) "status"))
         (args (rest argv)))
    (list :command (intern (string-upcase cmd) :keyword)
          :args args)))

;;; MAIN
(defun main ()
  (let* ((parsed (parse-launcher-args))
         (cmd (getf parsed :command))
         (args (getf parsed :args)))
    (show-banner)
    (case cmd
      (:DEV (cmd-dev args))
      (:BUILD (cmd-build args))
      (:MODULE (cmd-module args))
      (:GENERATE (if (< (length args) 2)
                     (error "generate requires: MODULE TYPE")
                     (cmd-generate (first args) (second args))))
      (:DEPLOY (if (< (length args) 2)
                   (error "deploy requires: MODULE TARGET")
                   (cmd-deploy (first args) (second args))))
      (:VALIDATE (cmd-validate args))
      (:CLEAN (cmd-clean args))
      (:STATUS (cmd-status))
      (:TMUX (cmd-tmux args))
      (:HELP (launcher-usage))
      (t (echo "Unknown command: " cmd) (launcher-usage)))))

(handler-case (main)
  (error (e)
    (echo "❌ ERROR: " e)
    (echo "")
    (launcher-usage)))
