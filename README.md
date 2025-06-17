purescript-midi-keyboard
========================

Play music through the browser by attaching a MIDI keyboard or other device.  This program loads up a grand piano soundfont and then detects MIDI devices as they connect or disconnect and also events as keys are pressed.  It responds only to NoteON messages (which it plays through the soundfont instrument) and volume change messages. If you attach more than one device, they all play through the same instrument.  You can change the selected instrument at any time.

If you have such a device then you can try it [here](https://www.tradtunedb.org.uk:8601/).  

Two versions are provided.  The first is for Halogen 5 using ps 13.8. and the other is an older, deprecated version for Pux and ps 12.0.

## to build the Halogen version

cd to the Halogen directory

     $ spago install
     $ npm run build

The code is built as midi-keyboard.js in the Halogen/dist directory. Host Halogen/dist/index.html on a web server and navigate to it to try it out.  You need a MIDI keyboard or other device to use it and also a browser that supports web-midi.

## Pux version

It seems as if Pux is no longer regularly maintained. If you need to use the latest PureScript compiler, you should instead use the Halogen version (see above).
