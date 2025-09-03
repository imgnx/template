;;; Taku configuration
;;; Defines *taku-config* in the taku.core package so all modules can access it

(in-package :taku.core)

(defparameter *taku-config*
  '(:project-name "Taku Audio Processing"
    :version "0.2.0"
    :modules (
      (:name :bitcrusher
       :type :audio-effect
       :rust-crate "taku_dsp"
       :api-endpoints ("/api/bitcrush")
       :frontend-component "BitcrusherModule"
       :description "Lo-fi audio degradation effects")
      
      (:name :tuner
       :type :audio-tool
       :rust-crate "taku_tuner"
       :api-endpoints ("/api/tuner/start" "/api/tuner/stop" "/api/tuner/status")
       :frontend-component "TunerModule"
       :description "Real-time pitch detection and tuning")
      
      (:name :reverb
       :type :audio-effect
       :rust-crate "taku_reverb"
       :api-endpoints ("/api/reverb")
       :frontend-component "ReverbModule"
       :description "Space and ambience effects"
       :status :planned)
      
      (:name :eq
       :type :audio-effect
       :rust-crate "taku_eq"
       :api-endpoints ("/api/eq")
       :frontend-component "EQModule"
       :description "Parametric equalizer"
       :status :planned))))

(export '(*taku-config*))
