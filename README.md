# picking-sticks
A "Hello world" of game programming. Inspired by Moosader's "Beginner's Guide to Game Programming" video.



How to use

Because the project is mainly a sandbox for testing multiple collision detection schemes,
I will give a short overview covering the fun effective parts.

Run the Program:

- First load the *.asd system. 
- Move to (in-package :game) in your REPL
- run (main) in the repl

Interaction:

The code in game.lisp under the comment ;;Events (...)  contains all the callbacks that
take care of the mouse keyboard interaction with the demo. The keys w,a,s,d move the main
game-object called "Nyo", q,e to rotate it and r and f to scale it. You can place more
rectangles by clicking or holding the mouse somewhere in the sdl window.  Currently Nyo
will make any rectangles it touches disappear.

Game-objects are stored in a global hash-table that can be read and added to, in a
sequential way -- the SEQ-HASH-TABLE, implemented as a DEFCLASS in the game-object.lisp
file


Every rectangle in game-object::*dynamic-rectangles* will be rendered on the screen.

Here is how it's done:

(make-rectangle-c (vec3 200.0 300.0))  ;; rectangle's center at pixel 200 300
                                       ;; the "c" implies center-radius representation


(add-rectangle-as :nyo *) ;; adds the rectangle to, by default, the *dynamic-rectangles*
                          ;; sequential hash-table


Now the rectangle will appear on the screen centered at point(200 300 0).

The "name" provided to ADD-RECTANGLE-AS is the hash-key. The use of a hash-table is
primarily for fun use in the interactive REPL. There are a set of functions that will work
directly, by just using the name of the rectangle. They have an &option argument that
defaults to operate on rectangles in the *dynamic-rectangle* seq-hash-table. Such as

(move :nyo (vec2 50.0 50.0))
(scale :nyo 2.0)
(rotate :nyo 180.0)


useful functions:

(get-rectangle <name>)                   ;; the GETHASH for the seq-hash-table
(remove-rectangle <name>)                ;; again, <name> is just the hash-key e.g. :nyo
                                         ;; the current default effect of collision of the 
                                         ;; default demo!

(print-rectangles *dynamic-rectangles*)  ;; rectangles have a useful print representation

(call-do-seq-hash (seq-hash function result))  ;; the DOLIST for seq-hash-tables



Animation,

changing the pixels displayed on the rectangle. 

The animation state is stored in each game-object::RECTANGLE as a slot containing an
animation class object.

The implementation uses a big texture and all we do is change which region of the texture
will be read by a game-object rectangle. Check out an actual use case in the file
game.lisp in the method USING-KEYBOARD-STATE.

The system TEXATL helps to access certain sub-pictures in the "big texture" with meaningful
names. Such that, if the big texture is just a spritesheet, and you want to read out the
walking animation for "down" you'd write (game-objects:set-animation :nyo :walk :down)
while (game-objects:next-animation-frame :nyo) would cycle through the animation frames
for walking down. The scheme of adding new pictures and subdividing it in proper names
and reading them out, and putting them together into an texture atlas etc. is described
in the tex.lisp file, follow the nyo.png example. It is a bit involved to add your own 
animation with animation frames.


Outlook:
The project will move towards implementing various collision detection schemes and possibly
extrapolating into collision response.

The file "note-collision-detection" is following the, so far, excellent book: "real-time
collision detection" by Christer Ericson.

The project has a fork working on a smaller, quick and dirty, core
picture-language-package that is used to implement SICP's picture-language. A DSL for
creating sub pictures in patterns inside a general parallelogram.