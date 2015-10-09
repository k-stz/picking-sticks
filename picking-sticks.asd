;; This command is package indepedent. (debug 3) will show you local variables
;; of a function and even it's lexically scoped once (if they use a *special-variable*
;; This pertains functions involved in a signalling error
(declaim (optimize (speed 0) (safety 3) (debug 3)))

;; remember this is quickloadable when in quickloads path: (ql:quickload :picking-sticks)

(defsystem #:picking-sticks
  :description "Picking sticks - 'Hello world' of game programming"
  :version "0.0.1"
  ;; system-name != package-name   i.e. :sdl2kit != sdl2.kit !
  :depends-on (:cl-opengl :sdl2kit :glkit :cepl :sdl2-image :opticl :texatl)
  :author "k-stz"
  :license "MIT"
  :serial t
  :components ((:file "tex")
	       (:file "math")
	       (:file "gl-utils")
	       (:file "opticl-utils")
	       (:file "game-objects")
	       (:file "game-collision")
	       (:file "game")))
