(defsystem #:picking-sticks
  :description "Picking sticks - 'Hello world' of game programming"
  :version "0.0.1"
  ;; system-name != package-name   i.e. :sdl2kit != sdl2.kit !
  :depends-on (:cl-opengl :sdl2kit :glkit :cepl :sdl2-image :opticl)
  :author "k-stz"
  :license "MIT"
  :serial t
  :components ((:file "rectangle-objects")
	       (:file "game")))
