halogen-midi-keyboard
=====================

This is a version of purescript-midi-keyboard but one which uses Halogen 5.0.0 rather than Pux. This project illustrates how it is possible to adapt [Signals](https://pursuit.purescript.org/packages/purescript-signal/10.1.0) for use as Halogen Events.


## to build

From the current directory

     $ spago install
     $ npm run build

or

     $ bower install
     $ npm run pulp-build

The code is built as midi-keyboard.js in the dist directory. Host dist/index.html on a web server and navigate to it to try it out.  You need a MIDI keyboard or other device to use it and also a browser that supports web-midi.
