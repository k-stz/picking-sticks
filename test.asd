(asdf:defsystem #:picking-sticks
  :description "Picking sticks - 'Hello world' of game programming"
  :version "0.0.1"
  ;; system-name != package-name   i.e. :sdl2kit != sdl2.kit !
  :depends-on (:sdl2kit)
  :author "k-stz"
  :license "MIT"
  :serial t
  :components ((:file "game")))

;; load system immediately
;(asdf:load-system "picking-sticks")

