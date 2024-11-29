# rigctl

SPert 1000 ([https://rjk.com.pl/spert1000/](https://rjk.com.pl/spert1000/)) + radio CAT control application.

**Supported radios:**

* FTdx10
* FT-991A (limited, w/o split)

## Features:

Two separate windows, each can work independently, i.e., you can use the application only for SPert control.

### SPert 1000 HF power amplifier:

* Serial port auto-discover: automatically find proper serial port name
* Monitor TX forwarded and reflected power, temperature
* Control ATU: on/off and tuning
* Control fan mode
* Software QRO on/off
* Control output power
* Keyboard shortcuts

### Radio:

* Serial port auto-discover: automatically find proper serial port name
* Expose FLRIG server for integration with WSJT-X/Logbooks, etc, just configure other applications to use FLRIG interface on 12345 port
* Monitor current VFO frequency, mode
* S-meter
* Monitor TX forwarded and reflected power
* Band switch
* Filters DNR, NB
* Split operations: on/off, up 5, up 10 with single click, TXW (listen on TX frequency)
* Control output power and digital gain
* Keyboard shortcuts

### Integrations available, when both devices connected

* SPert reflected power is saved for a particular frequency obtained from the radio, and you get a prompt to switch on ATU or even QRO can be blocked if the saved SWR is too high. Please note: you must "learn" the application first.
* SPert ATU settings L/C are saved for a particular frequency obtained from the radio, and you get a prompt to re-tune ATU if the frequency changes and previous L/C for the new frequency are different.
* When the radio band is changed, SPert ATU is automatically turned off.

## Dependencies

Application is using patched LazSynaSer from https://github.com/JurassicPork/TLazSerial

Application is using icons from following authors:
* https://www.iconfinder.com/Allfreeicons
* https://www.iconfinder.com/iconsets/developerkit

For license see LICENSE

## MacOS release process

```
iconutil --convert icns icon.iconset
mv ./icon.icns binaries/darwin_x86_64/RadioShackCtl.app/Contents/Resources/RadioShackCtl.icns
cp -f ./Info.plist binaries/darwin_x86_64/RadioShackCtl.app/Contents/Info.plist
rm -f binaries/darwin_x86_64/RadioShackCtl.app/Contents/MacOS/RadioShackCtl
mv binaries/darwin_x86_64/RadioShackCtl binaries/darwin_x86_64/RadioShackCtl.app/Contents/MacOS/
touch binaries/darwin_x86_64/RadioShackCtl.app
ln -s /Applications binaries/darwin_x86_64/Applications
```

Run disk util (Narzędzie dyskowe) and use menu: File -> New image -> Image from folder.

