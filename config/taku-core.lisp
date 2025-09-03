;;; Taku Core Build System
;;; Core utilities and infrastructure for the Taku build system

(require :asdf)

(uiop:define-package :taku.core (:use :cl :uiop))
(in-package :taku.core)

;;; =============================================================================
;;; CORE UTILITIES
;;; =============================================================================

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

(defun warn-echo (&rest parts)
  (format t "~&[拓] ⚠️  ~{~a~}~%" parts))

(defun error-echo (&rest parts)
  (format t "~&[拓] ❌ ~{~a~}~%" parts))

(defun success-echo (&rest parts)
  (format t "~&[拓] ✅ ~{~a~}~%" parts))

;;; =============================================================================
;;; PROCESS MANAGEMENT
;;; =============================================================================

(defun spawn (argv &key (dir (repo-root)) (output :interactive))
  (format t "~&[拓] 🚀 spawn: ~{~a~^ ~}~%" argv)
  (launch-program argv 
    :directory dir 
    :output output
    :error-output output
    :search t))

(defun spawn-silent (argv &key (dir (repo-root)))
  (launch-program argv 
    :directory dir 
    :output nil
    :error-output nil
    :search t))

(defun wait-any (&rest procs)
  (loop do (sleep 0.5) do
    (dolist (p procs)
      (unless (process-alive-p p)
        (return-from wait-any p)))))

(defun terminate-all (&rest procs)
  (dolist (p procs)
    (ignore-errors (terminate-process p))))

;;; =============================================================================
;;; DEPENDENCY CHECKING
;;; =============================================================================

(defun have-bin-p (b)
  (let* ((cmd (list "sh" "-lc" 
                   (format nil "command -v ~a >/dev/null 2>&1 && echo OK || echo NO" b)))
         (out (run-program cmd :ignore-error-status t :output '(:string :stripped t))))
    (string= out "OK")))

(defun ensure-bins (&rest bins)
  (dolist (b bins)
    (unless (have-bin-p b)
      (error "Required program '~a' not found on PATH." b))))

(defun check-system-dependencies ()
  (echo "Checking system dependencies...")
  (let ((required-bins '("python" "npm" "cargo" "node" "sbcl"))
        (missing '()))
    (dolist (bin required-bins)
      (if (have-bin-p bin)
        (success-echo bin " ✓")
        (progn 
          (error-echo bin " ✗")
          (push bin missing))))
    (if missing
      (error-echo "Missing dependencies: " missing)
      (success-echo "All dependencies satisfied"))))

;;; =============================================================================
;;; FILE SYSTEM UTILITIES
;;; =============================================================================

(defun ensure-directory (path)
  "Ensure directory exists, create if necessary"
  (ensure-directories-exist path))

(defun file-exists-p (path)
  "Check if file exists"
  (probe-file path))

(defun directory-exists-p (path)
  "Check if directory exists"
  (and (probe-file path) (directory-pathname-p path)))

(defun copy-file-safe (from to)
  "Copy file with error handling"
  (handler-case
    (progn
      (ensure-directory (directory-namestring to))
      (copy-file from to)
      (success-echo "Copied: " from " → " to))
    (error (e)
      (error-echo "Failed to copy " from ": " e))))

(defun write-file-safe (path content)
  "Write file with error handling"
  (handler-case
    (progn
      (ensure-directory (directory-namestring path))
      (with-open-file (stream path :direction :output :if-exists :supersede)
        (write-string content stream))
      (success-echo "Created: " path))
    (error (e)
      (error-echo "Failed to write " path ": " e))))

;;; =============================================================================
;;; CONFIGURATION MANAGEMENT
;;; =============================================================================

(defun read-config-file (path)
  "Read and parse configuration file"
  (when (file-exists-p path)
    (handler-case
      (with-open-file (stream path)
        (read stream))
      (error (e)
        (warn-echo "Failed to read config " path ": " e)
        nil))))

(defun write-config-file (path config)
  "Write configuration to file"
  (write-file-safe path (format nil "~s~%" config)))

;;; =============================================================================
;;; TEMPLATE SYSTEM
;;; =============================================================================

(defun substitute-template (template-string substitutions)
  "Replace {{KEY}} patterns in template with values from substitutions alist"
  (let ((result template-string))
    (dolist (sub substitutions)
      (let ((key (format nil "{{~a}}" (car sub)))
            (value (cdr sub)))
        (setf result (regex-replace-all key result (princ-to-string value)))))
    result))

(defun regex-replace-all (pattern string replacement)
  "Simple pattern replacement (fallback implementation)"
  (let ((pos 0)
        (result ""))
    (loop
      (let ((found (search pattern string :start2 pos)))
        (if found
          (progn
            (setf result (concatenate 'string result 
                                    (subseq string pos found)
                                    replacement))
            (setf pos (+ found (length pattern))))
          (progn
            (setf result (concatenate 'string result (subseq string pos)))
            (return result)))))))

;;; =============================================================================
;;; EXPORT SYMBOLS
;;; =============================================================================

(export '(repo-root subdir echo warn-echo error-echo success-echo
          spawn spawn-silent wait-any terminate-all
          have-bin-p ensure-bins check-system-dependencies
          ensure-directory file-exists-p directory-exists-p
          copy-file-safe write-file-safe
          read-config-file write-config-file
          substitute-template))
