
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
