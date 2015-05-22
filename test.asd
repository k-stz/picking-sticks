(asdf:defsystem #:picking-sticks
  :description "Picking sticks - 'Hello world' of game programming"
  :version "0.0.1"
  :depends-on (:sdl2.kit)
  :author "k-stz"
  :license "MIT"
  :serial t
  :components ((:file "game")))

;; load system immediately
(asdf:load-system "picking-sticks")
