;;; Taku Modules Management
;;; Module-specific build, development, and deployment functions

(require :asdf)
(load "taku-core.lisp")

(uiop:define-package :taku.modules (:use :cl :uiop :taku.core))
(in-package :taku.modules)

;;; =============================================================================
;;; MODULE CONFIGURATION (shared from launcher)
;;; =============================================================================

;; Module configuration is defined centrally in taku-config.lisp
;; and exported from the taku.core package as taku.core::*taku-config*.
;; Do not define a local *taku-config* here; reference the central config instead.

;;; =============================================================================
;;; MODULE DISCOVERY AND VALIDATION
;;; =============================================================================

(defun find-module-config (module-name config)
  "Find module configuration by name"
  (find module-name (getf config :modules) 
        :key (lambda (m) (getf m :name))))

(defun validate-module (module-name)
  "Validate module configuration and dependencies"
  (echo "Validating module: " module-name)
  (let ((module (find-module-config module-name taku.core::*taku-config*)))
    (unless module
      (error "Module not found: ~a" module-name))
    
    ;; Check Rust crate exists
    (when (getf module :rust-crate)
      (let ((crate-path (merge-pathnames 
                         (format nil "crates/~a/" (getf module :rust-crate))
                         (repo-root))))
        (unless (directory-exists-p crate-path)
          (warn-echo "Rust crate directory missing: " crate-path))))
    
    ;; Check frontend component exists  
    (when (getf module :frontend-component)
      (let ((component-path (merge-pathnames 
                            (format nil "src/components/~a.jsx" 
                                   (getf module :frontend-component))
                            (repo-root))))
        (unless (file-exists-p component-path)
          (warn-echo "Frontend component missing: " component-path))))
    
    (success-echo "Module " module-name " validation complete")))

;;; =============================================================================
;;; DEVELOPMENT ENVIRONMENT
;;; =============================================================================

(defun dev-all-modules ()
  "Start development environment for all active modules"
  (echo "Starting full development environment...")
  (let* ((root (repo-root))
         (backend-proc (start-backend :root root))
         (frontend-proc (start-frontend :root root :target "modular")))
    
    #+sbcl (push (lambda () (terminate-all backend-proc frontend-proc)) 
                 sb-ext:*exit-hooks*)
    
    (let ((dead (wait-any backend-proc frontend-proc)))
      (echo "Process exited: " dead)
      (terminate-all backend-proc frontend-proc))))

(defun dev-single-module (module-name)
  "Start development environment for a single module"
  (echo "Starting development for module: " module-name)
  (let ((module-keyword (if (stringp module-name)
                          (intern (string-upcase module-name) :keyword)
                          module-name)))
    (validate-module module-keyword)
    
    (let* ((root (repo-root))
           (backend-proc (start-backend :root root))
           (frontend-proc (start-frontend :root root :target (string-downcase (princ-to-string module-name)))))
      
      #+sbcl (push (lambda () (terminate-all backend-proc frontend-proc)) 
                   sb-ext:*exit-hooks*)
      
      (let ((dead (wait-any backend-proc frontend-proc)))
        (echo "Process exited: " dead)
        (terminate-all backend-proc frontend-proc)))))

(defun dev-multiple-modules (modules)
  "Start development environment for multiple specific modules"
  (echo "Starting development for modules: " modules)
  (dolist (module modules)
    (validate-module (intern (string-upcase (princ-to-string module)) :keyword)))
  
  ;; For now, use modular interface when multiple modules specified
  (dev-all-modules))

;;; =============================================================================
;;; BUILD SYSTEM
;;; =============================================================================

(defun build-all-components ()
  "Build all components of the system"
  (echo "Building all components...")
  (build-component "rust")
  (build-component "backend") 
  (build-component "frontend"))

(defun build-component (component)
  "Build specific component"
  (case (intern (string-upcase component) :keyword)
    (:RUST (build-rust-components))
    (:BACKEND (build-backend-component))
    (:FRONTEND (build-frontend-components))
    (:ALL (build-all-components))
    (t (error "Unknown component: ~a" component))))

(defun build-rust-components ()
  "Build all Rust crates"
  (echo "Building Rust components...")
  (ensure-bins "cargo")
  (let ((proc (spawn '("cargo" "build" "--release") 
                     :dir (subdir (repo-root) "crates/"))))
    (wait-any proc)
    (if (zerop (process-exit-code proc))
      (success-echo "Rust build complete")
      (error-echo "Rust build failed"))))

(defun build-backend-component ()
  "Validate and prepare backend component"
  (echo "Validating backend component...")
  (ensure-bins "python")
  ;; Could add Python packaging, dependency checking, etc.
  (success-echo "Backend validation complete"))

(defun build-frontend-components ()
  "Build all frontend targets"
  (echo "Building frontend components...")
  (ensure-bins "npm")
  (dolist (target '("bitcrusher" "tuner" "modular"))
    (build-frontend-target target)))

(defun build-frontend-target (target)
  "Build specific frontend target"
  (echo "Building frontend target: " target)
  (let ((proc (spawn (list "npm" "run" (format nil "build:~a" target))
                     :dir (repo-root))))
    (wait-any proc)
    (if (zerop (process-exit-code proc))
      (success-echo "Frontend build complete: " target)
      (error-echo "Frontend build failed: " target))))

;;; Build a single named module (frontend-only)
(defun build-module (module-name)
  "Build only the specified module's frontend target.
Encourages router-less, module-scoped apps."
  (let* ((name-str (string-downcase (princ-to-string module-name)))
         (kw (intern (string-upcase name-str) :keyword)))
    (validate-module kw)
    (echo "Building module (frontend only): " name-str)
    (build-frontend-target name-str)))

;;; =============================================================================
;;; BACKEND MANAGEMENT
;;; =============================================================================

(defun start-backend (&key (root (repo-root)))
  "Start FastAPI backend server (retrofinem/app)"
  (ensure-bins "python")
  (spawn '("python" "-m" "uvicorn" "app.main:app" "--reload" "--port" "8000")
         :dir (subdir root "retrofinem/")))

(defun stop-backend ()
  "Stop backend server (if running)"
  ;; Could implement PID tracking for graceful shutdown
  (echo "Backend stop requested"))

;;; =============================================================================
;;; FRONTEND MANAGEMENT
;;; =============================================================================

(defun start-frontend (&key (root (repo-root)) (target "modular"))
  "Start frontend development server"
  (ensure-bins "npm")
  (spawn (list "npm" "run" (format nil "dev:~a" target)) :dir root))

(defun stop-frontend ()
  "Stop frontend development server"
  (echo "Frontend stop requested"))

;;; =============================================================================
;;; RUST SIDECAR MANAGEMENT
;;; =============================================================================

(defun start-rust-sidecar (module-name &key (bits 12) (rate 16000))
  "Start Rust DSP sidecar process"
  (let ((module (find-module-config module-name taku.core::*taku-config*)))
    (unless module
      (error "Module not found: ~a" module-name))
    
  (let ((crate-name (getf module :rust-crate)))
      (unless crate-name
        (error "No Rust crate defined for module: ~a" module-name))
      
      (ensure-bins "cargo")
      (spawn (list "cargo" "run" "-p" (princ-to-string crate-name) "--"
                   "--bits" (princ-to-string bits)
                   "--rate" (princ-to-string rate))
             :dir (subdir (repo-root) "crates/")))))

;;; =============================================================================
;;; TMUX SESSION MANAGEMENT
;;; =============================================================================

(defun cmd-tmux (modules)
  "Launch development environment in tmux"
  (echo "Starting tmux development session...")
  (ensure-bins "tmux" "python" "npm")
  
  (let ((target (if (null modules) "modular" 
                   (if (= (length modules) 1) 
                     (string-downcase (princ-to-string (first modules)))
                     "modular"))))
    
    (tmux-dev :root (repo-root) :target target)))

(defun tmux-dev (&key (root (repo-root)) (target "modular"))
  "Create tmux development session"
  (let ((session-name (format nil "taku-~a" target))
        (cmd1 (list "tmux" "new-session" "-d" "-s" session-name "-c"
                    (namestring (subdir root "retrofinem/"))
                    "python -m uvicorn app.main:app --reload --port 8000"))
        (cmd2 (list "tmux" "split-window" "-h" "-t" (format nil "~a:0" session-name) 
                   "-c" (namestring root) (format nil "npm run dev:~a" target)))
        (cmd3 (list "tmux" "split-window" "-v" "-t" (format nil "~a:0" session-name)
                    "-c" (namestring root)
                    "cargo run -p taku_cli --bin dev_view"))
        (cmd4 (list "tmux" "select-layout" "-t" (format nil "~a:0" session-name) 
                   "tiled"))
        (cmd5 (list "tmux" "set-option" "-t" session-name "mouse" "on"))
        (cmd6 (list "tmux" "attach-session" "-t" session-name)))
    
    (dolist (cmd (list cmd1 cmd2 cmd3 cmd4 cmd5 cmd6))
      (spawn cmd :dir root))))

;;; =============================================================================
;;; WEBVIEW (WRY) LAUNCHER
;;; =============================================================================

(defun start-webview (&key (root (repo-root)) (url "http://127.0.0.1:8080"))
  "Start a Wry-based webview pointed at the frontend URL."
  (ensure-bins "cargo")
  (spawn (list "cargo" "run" "-p" "taku_cli" "--bin" "dev_view" "--" url)
         :dir root))

;;; =============================================================================
;;; CLEANUP UTILITIES
;;; =============================================================================

(defun clean-all-modules ()
  "Clean build artifacts for all modules"
  (echo "Cleaning all build artifacts...")
  (clean-rust-artifacts)
  (clean-frontend-artifacts)
  (clean-temp-files))

(defun clean-module (module-name)
  "Clean build artifacts for specific module"
  (echo "Cleaning module: " module-name)
  ;; Module-specific cleanup
  (clean-temp-files))

(defun clean-rust-artifacts ()
  "Clean Rust build artifacts"
  (let ((target-dir (merge-pathnames "target/" (repo-root))))
    (when (directory-exists-p target-dir)
      (echo "Cleaning Rust target directory...")
      (spawn '("rm" "-rf" "target/") :dir (repo-root)))))

(defun clean-frontend-artifacts ()
  "Clean frontend build artifacts"
  (let ((dist-dir (merge-pathnames "dist/" (repo-root))))
    (when (directory-exists-p dist-dir)
      (echo "Cleaning frontend dist directory...")
      (spawn '("rm" "-rf" "dist/") :dir (repo-root)))))

(defun clean-temp-files ()
  "Clean temporary files"
  (echo "Cleaning temporary files...")
  (let ((temp-files '("frontend.log" "*.tmp" "*.log")))
    (dolist (pattern temp-files)
      (spawn (list "find" "." "-name" pattern "-type" "f" "-delete") 
             :dir (repo-root)))))

;;; =============================================================================
;;; HEALTH MONITORING
;;; =============================================================================

(defun check-module-health ()
  "Check health status of all modules"
  (echo "Checking module health...")
  
  ;; Check backend health
  (check-backend-health)
  
  ;; Check frontend build status
  (check-frontend-health)
  
  ;; Check Rust crates
  (check-rust-health))

(defun check-backend-health ()
  "Check if backend is responding"
  (handler-case
    (multiple-value-bind (output error-output exit-code)
        (run-program '("curl" "-s" "-f" "http://localhost:8000/api/health")
                     :output :string :ignore-error-status t)
      (declare (ignore output error-output))
      (if (zerop exit-code)
        (success-echo "Backend health: OK")
        (warn-echo "Backend health: Not responding")))
    (error (e)
      (warn-echo "Backend health check failed: " e))))

(defun check-frontend-health ()
  "Check frontend build artifacts"
  (let ((dist-dir (merge-pathnames "dist/" (repo-root))))
    (if (directory-exists-p dist-dir)
      (success-echo "Frontend artifacts: OK")
      (warn-echo "Frontend artifacts: Missing"))))

(defun check-rust-health ()
  "Check Rust crate compilation"
  (let ((proc (spawn-silent '("cargo" "check") 
                           :dir (subdir (repo-root) "crates/"))))
    (wait-any proc)
    (if (zerop (process-exit-code proc))
      (success-echo "Rust crates: OK")
      (warn-echo "Rust crates: Compilation issues"))))

(defun show-build-status ()
  "Show current build status"
  (echo "📊 Build Status:")
  (check-module-health))

;;; =============================================================================
;;; DEPLOYMENT UTILITIES
;;; =============================================================================

(defun cmd-deploy (module-name target)
  "Deploy module to specified target"
  (echo "Deploying " module-name " to " target)
  (validate-module (intern (string-upcase module-name) :keyword))
  
  (case (intern (string-upcase target) :keyword)
    (:PRODUCTION (deploy-production module-name))
    (:STAGING (deploy-staging module-name))
    (:LOCAL (deploy-local module-name))
    (t (error "Unknown deployment target: ~a" target))))

(defun deploy-production (module-name)
  "Deploy to production environment"
  (warn-echo "Production deployment not yet implemented for: " module-name))

(defun deploy-staging (module-name)
  "Deploy to staging environment"
  (warn-echo "Staging deployment not yet implemented for: " module-name))

(defun deploy-local (module-name)
  "Deploy to local environment"
  (echo "Local deployment for: " module-name)
  (build-component "all"))

;;; =============================================================================
;;; EXPORT SYMBOLS
;;; =============================================================================

(export '(find-module-config validate-module
          dev-all-modules dev-single-module dev-multiple-modules
          build-all-components build-component
          start-backend stop-backend start-frontend stop-frontend
          start-rust-sidecar cmd-tmux tmux-dev
          clean-all-modules clean-module
          check-module-health show-build-status
          cmd-deploy))
