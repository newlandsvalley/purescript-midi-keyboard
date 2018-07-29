purescript-midi-keyboard
========================

Play music through the browser by attaching a MIDI keyboard or other device.  This program loads up a grand piano soundfont and then detects MIDI devices as they connect or disconnect and also events as keys are pressed.  It responds only to NoteON messages (which it plays through the soundfont instrument) and volume change messages. If you attach more than one device, they all play through the same instrument.  You can change the selected instrument at any time.

If you have such a device, then you can try it [here](http://www.tradtunedb.org.uk:8601/).

Note that you will need to use a browser that supports web-midi (at the time of writing, Chrome or Opera).

This version for PureScript 12.0, Pux 13.0

## to build

From the current directory

     $ bower install
     $ ./build.sh

The code is built as midi-keyboard.js in the dist directory. Host dist/index.html on a web server and navigate to it to try it out.  You need a MIDI keyboard or other device to use it and also a browser that supports web-midi.

## halogen version

The Halogen subdirectory contains an equivalent MIDI keyboard but one which uses Halogen rather than Pux.
