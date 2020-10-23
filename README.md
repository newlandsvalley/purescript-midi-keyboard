purescript-midi-keyboard
========================

Play music through the browser by attaching a MIDI keyboard or other device.  This program loads up a grand piano soundfont and then detects MIDI devices as they connect or disconnect and also events as keys are pressed.  It responds only to NoteON messages (which it plays through the soundfont instrument) and volume change messages. If you attach more than one device, they all play through the same instrument.  You can change the selected instrument at any time.

If you have such a device, and if you are using an old version of a browser, then you can try it [here](http://www.tradtunedb.org.uk:8601/).  (This server no longer works with modern browsers such as Chrome or Mozilla because they now only support web-MIDI on 'untrusted' servers through an HTTPS connection.) The midi-keyboard will, however, run quite happily under localhost.

This version for PureScript 12.0 and Pux 13.0.  It seems as if Pux is no longer regularly maintained. If you need to use the latest PureScript compiler, you should instead use the Halogen version (see below).

## to build

From the current directory

     $ bower install
     $ ./build.sh

The code is built as midi-keyboard.js in the dist directory. Host dist/index.html on a web server and navigate to it to try it out.  You need a MIDI keyboard or other device to use it and also a browser that supports web-midi.

## Halogen version

The Halogen subdirectory contains an equivalent MIDI keyboard but one which uses Halogen 5.0.0 rather than Pux together with purs 0.13.8.
