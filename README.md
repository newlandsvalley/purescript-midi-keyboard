purescript-midi-keyboard
========================

work in progress

An experiment to build a MIDI keyboard using the new web-midi interface from purescript-midi that uses signals and the new purescript-soundfont library to play the notes that are selected on the keyboard.


## to build

From the current directory

   $ bower install
   $ ./build.sh
   
The code is built as midi-keyboard.js in the dist directory. This is runnable if you open the corresponding index.html.  Host this on a web server and navigate to dist/index.html.  You need a MIDI keyboard or other device to use it.