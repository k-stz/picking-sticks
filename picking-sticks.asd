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
