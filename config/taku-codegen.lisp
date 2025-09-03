;;; Taku Code Generation
;;; Automated code generation for new modules and components

(require :asdf)
(load "taku-core.lisp")

(uiop:define-package :taku.codegen (:use :cl :uiop :taku.core))
(in-package :taku.codegen)

;;; =============================================================================
;;; TEMPLATES
;;; =============================================================================

(defparameter *rust-crate-cargo-toml-template*
  "[package]
name = \"{{CRATE_NAME}}\"
version = \"0.1.0\"
edition = \"2021\"
description = \"{{DESCRIPTION}}\"

[dependencies]
clap = { version = \"4\", features = [\"derive\"] }
serde = { version = \"1.0\", features = [\"derive\"] }

[lib]
name = \"{{CRATE_NAME}}\"
path = \"src/lib.rs\"
")

(defparameter *rust-lib-template*
  "/// {{DESCRIPTION}}
/// Part of the Taku modular audio processing framework

#[derive(Clone, Copy, Debug)]
pub struct {{STRUCT_NAME}} {
    // TODO: Add module-specific parameters
    pub enabled: bool,
}

impl Default for {{STRUCT_NAME}} {
    fn default() -> Self {
        Self {
            enabled: true,
        }
    }
}

impl {{STRUCT_NAME}} {
    /// Process a single audio sample
    pub fn process_sample(&self, input: f32) -> f32 {
        if self.enabled {
            // TODO: Implement {{MODULE_NAME}} processing
            input
        } else {
            input
        }
    }

    /// Process a buffer of audio samples in-place
    pub fn process_buffer_inplace(&self, buffer: &mut [f32]) {
        if self.enabled {
            for sample in buffer.iter_mut() {
                *sample = self.process_sample(*sample);
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_{{MODULE_NAME}}_default() {
        let processor = {{STRUCT_NAME}}::default();
        assert!(processor.enabled);
    }

    #[test]
    fn test_{{MODULE_NAME}}_process_sample() {
        let processor = {{STRUCT_NAME}}::default();
        let input = 0.5;
        let output = processor.process_sample(input);
        // TODO: Add meaningful assertions
        assert!((output - input).abs() < f32::EPSILON);
    }
}
")

(defparameter *frontend-component-template*
  "import React, { useState } from 'react';

export default function {{COMPONENT_NAME}}() {
  const [enabled, setEnabled] = useState(true);
  const [status, setStatus] = useState('');

  const startProcessing = async () => {
    setStatus('Starting...');
    try {
      const response = await fetch('{{API_ENDPOINT}}', {
        method: 'POST',
        headers: { 'Content-Type': 'application/json' },
        body: JSON.stringify({ enabled })
      });
      
      if (!response.ok) throw new Error('Failed to start processing');
      
      const result = await response.json();
      setStatus(result.status || 'Processing started!');
    } catch (error) {
      setStatus('Error: ' + error.message);
    }
  };

  return (
    <div style={{ border: '2px solid {{BORDER_COLOR}}', borderRadius: 8, padding: 16, margin: 8 }}>
      <h2>{{ICON}} {{TITLE}}</h2>
      <p>{{DESCRIPTION}}</p>

      <section style={{ marginBottom: 12 }}>
        <label style={{ display: 'flex', alignItems: 'center', gap: 8 }}>
          <input 
            type=\"checkbox\" 
            checked={enabled}
            onChange={e => setEnabled(e.target.checked)}
          />
          Enable {{TITLE}}
        </label>
      </section>

      {/* TODO: Add module-specific controls */}

      <div style={{ marginTop: 16 }}>
        <button 
          onClick={startProcessing}
          style={{ 
            padding: '8px 16px', 
            backgroundColor: '{{BORDER_COLOR}}', 
            color: 'white', 
            border: 'none', 
            borderRadius: 4,
            cursor: 'pointer'
          }}
        >
          Start {{TITLE}}
        </button>
        {status && (
          <span style={{ marginLeft: 12, opacity: 0.8 }}>{status}</span>
        )}
      </div>
    </div>
  );
}
")

(defparameter *api-endpoints-template*
  "# API Endpoints for {{MODULE_NAME}}

from fastapi import APIRouter, HTTPException
from pydantic import BaseModel
from typing import Optional

router = APIRouter(prefix=\"/api/{{MODULE_NAME_LOWER}}\", tags=[\"{{MODULE_NAME}}\"])

class {{MODULE_NAME}}Request(BaseModel):
    enabled: bool = True
    # TODO: Add module-specific parameters

class {{MODULE_NAME}}Response(BaseModel):
    status: str
    message: Optional[str] = None

@router.post(\"/start\", response_model={{MODULE_NAME}}Response)
async def start_{{MODULE_NAME_LOWER}}(request: {{MODULE_NAME}}Request):
    \"\"\"Start {{MODULE_NAME}} processing\"\"\"
    try:
        # TODO: Implement {{MODULE_NAME}} startup logic
        if request.enabled:
            return {{MODULE_NAME}}Response(
                status=\"started\",
                message=\"{{MODULE_NAME}} processing started\"
            )
        else:
            return {{MODULE_NAME}}Response(
                status=\"disabled\",
                message=\"{{MODULE_NAME}} is disabled\"
            )
    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))

@router.post(\"/stop\", response_model={{MODULE_NAME}}Response)
async def stop_{{MODULE_NAME_LOWER}}():
    \"\"\"Stop {{MODULE_NAME}} processing\"\"\"
    try:
        # TODO: Implement {{MODULE_NAME}} shutdown logic
        return {{MODULE_NAME}}Response(
            status=\"stopped\",
            message=\"{{MODULE_NAME}} processing stopped\"
        )
    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))

@router.get(\"/status\", response_model={{MODULE_NAME}}Response)
async def get_{{MODULE_NAME_LOWER}}_status():
    \"\"\"Get {{MODULE_NAME}} status\"\"\"
    try:
        # TODO: Implement {{MODULE_NAME}} status checking
        return {{MODULE_NAME}}Response(
            status=\"unknown\",
            message=\"Status checking not implemented\"
        )
    except Exception as e:
        raise HTTPException(status_code=500, detail=str(e))
")

;;; =============================================================================
;;; CODE GENERATION FUNCTIONS
;;; =============================================================================

(defun generate-rust-crate (module-name)
  "Generate a new Rust crate for an audio module"
  (let* ((module-str (string-downcase (princ-to-string module-name)))
         (crate-name (format nil "taku_~a" module-str))
         (struct-name (string-capitalize module-str))
         (crate-dir (subdir (repo-root) (format nil "crates/~a/" crate-name)))
         (src-dir (subdir crate-dir "src/"))
         (description (format nil "~a audio processing module" (string-capitalize module-str))))
    
    (echo "Generating Rust crate: " crate-name)
    
    ;; Create directories
    (ensure-directory crate-dir)
    (ensure-directory src-dir)
    
    ;; Generate Cargo.toml
    (let ((cargo-content (substitute-template *rust-crate-cargo-toml-template*
                           `(("CRATE_NAME" . ,crate-name)
                             ("DESCRIPTION" . ,description)))))
      (write-file-safe (merge-pathnames "Cargo.toml" crate-dir) cargo-content))
    
    ;; Generate lib.rs
    (let ((lib-content (substitute-template *rust-lib-template*
                         `(("DESCRIPTION" . ,description)
                           ("STRUCT_NAME" . ,struct-name)
                           ("MODULE_NAME" . ,module-str)))))
      (write-file-safe (merge-pathnames "lib.rs" src-dir) lib-content))
    
    (success-echo "Rust crate generated: " crate-name)))

(defun generate-frontend-component (module-name)
  "Generate a new React component for an audio module"
  (let* ((module-str (string-downcase (princ-to-string module-name)))
         (component-name (format nil "~aModule" (string-capitalize module-str)))
         (title (string-capitalize module-str))
         (component-dir (subdir (repo-root) "src/components/"))
         (api-endpoint (format nil "/api/~a" module-str))
         (description (format nil "~a audio processing controls" title))
         (icon (get-module-icon module-str))
         (border-color (get-module-color module-str)))
    
    (echo "Generating frontend component: " component-name)
    
    ;; Create components directory if it doesn't exist
    (ensure-directory component-dir)
    
    ;; Generate React component
    (let ((component-content (substitute-template *frontend-component-template*
                               `(("COMPONENT_NAME" . ,component-name)
                                 ("TITLE" . ,title)
                                 ("DESCRIPTION" . ,description)
                                 ("API_ENDPOINT" . ,api-endpoint)
                                 ("ICON" . ,icon)
                                 ("BORDER_COLOR" . ,border-color)))))
      (write-file-safe (merge-pathnames (format nil "~a.jsx" component-name) component-dir) 
                       component-content))
    
    (success-echo "Frontend component generated: " component-name)))

(defun generate-api-endpoints (module-name)
  "Generate FastAPI endpoints for an audio module"
  (let* ((module-str (string-downcase (princ-to-string module-name)))
         (module-title (string-capitalize module-str))
         (api-dir (subdir (repo-root) "app/api/"))
         (filename (format nil "~a.py" module-str)))
    
    (echo "Generating API endpoints: " filename)
    
    ;; Create API directory if it doesn't exist
    (ensure-directory api-dir)
    
    ;; Generate API module
    (let ((api-content (substitute-template *api-endpoints-template*
                         `(("MODULE_NAME" . ,module-title)
                           ("MODULE_NAME_LOWER" . ,module-str)))))
      (write-file-safe (merge-pathnames filename api-dir) api-content))
    
    (success-echo "API endpoints generated: " filename)))

(defun generate-module-config (module-name)
  "Generate module configuration entry"
  (let* ((module-str (string-downcase (princ-to-string module-name)))
         (module-title (string-capitalize module-str))
         (crate-name (format nil "taku_~a" module-str))
         (component-name (format nil "~aModule" module-title))
         (api-endpoints (list (format nil "/api/~a/start" module-str)
                             (format nil "/api/~a/stop" module-str)
                             (format nil "/api/~a/status" module-str)))
         (description (format nil "~a audio processing" module-title)))
    
    (echo "Module configuration for: " module-name)
    (echo "Add this to your *taku-config* :modules list:")
    (echo "")
    (format t "      (:name :~a~%" module-str)
    (format t "       :type :audio-effect~%")
    (format t "       :rust-crate \"~a\"~%" crate-name)
    (format t "       :api-endpoints ~s~%" api-endpoints)
    (format t "       :frontend-component \"~a\"~%" component-name)
    (format t "       :description \"~a\")~%" description)
    (echo "")))

;;; =============================================================================
;;; HELPER FUNCTIONS
;;; =============================================================================

(defun get-module-icon (module-name)
  "Get appropriate icon for module"
  (cond 
    ((string= module-name "reverb") "🌊")
    ((string= module-name "eq") "🎚️")
    ((string= module-name "compressor") "🗜️")
    ((string= module-name "delay") "🔁")
    ((string= module-name "filter") "🔍")
    ((string= module-name "distortion") "⚡")
    ((string= module-name "chorus") "🌀")
    ((string= module-name "flanger") "💫")
    ((string= module-name "phaser") "🌙")
    (t "🎵")))

(defun get-module-color (module-name)
  "Get appropriate color for module"
  (cond 
    ((string= module-name "reverb") "#6A5ACD")
    ((string= module-name "eq") "#20B2AA")
    ((string= module-name "compressor") "#FF6347")
    ((string= module-name "delay") "#32CD32")
    ((string= module-name "filter") "#FFD700")
    ((string= module-name "distortion") "#FF4500")
    ((string= module-name "chorus") "#9370DB")
    ((string= module-name "flanger") "#FF69B4")
    ((string= module-name "phaser") "#40E0D0")
    (t "#708090")))

;;; =============================================================================
;;; WORKSPACE GENERATION
;;; =============================================================================

(defun generate-full-module (module-name)
  "Generate complete module (Rust crate + frontend component + API endpoints)"
  (echo "Generating complete module: " module-name)
  (generate-rust-crate module-name)
  (generate-frontend-component module-name)
  (generate-api-endpoints module-name)
  (generate-module-config module-name)
  (success-echo "Complete module generated: " module-name))

(defun generate-workspace-structure ()
  "Generate initial workspace structure"
  (echo "Generating workspace structure...")
  
  (let ((directories '("src/components/"
                      "app/api/"
                      "crates/"
                      "docs/"
                      "scripts/"
                      "tests/")))
    (dolist (dir directories)
      (ensure-directory (subdir (repo-root) dir))))
  
  (success-echo "Workspace structure generated"))

;;; =============================================================================
;;; EXPORT SYMBOLS
;;; =============================================================================

(export '(generate-rust-crate generate-frontend-component 
          generate-api-endpoints generate-module-config
          generate-full-module generate-workspace-structure))
