purescript-midi-keyboard
========================

work in progress

A playable MIDI keyboard using the new web-midi interface from purescript-midi that uses signals and the new purescript-soundfont library to respond to key presses.

It still doesn't work reliably if a keyboard is already connected on startup.


## to build

From the current directory

     $ bower install
     $ ./build.sh

The code is built as midi-keyboard.js in the dist directory. Host dist/index.html on a web server and navigate to it to try it out.  You need a MIDI keyboard or other device to use it and also a browser that supports web-midi.
