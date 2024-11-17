# rigctl
SPert 1000 + FTdx10 control application

# MacOS

## Preparing icon
All icon files in directory: icon.iconset - file name is crucial.

```
iconutil --convert icns icon.iconset
mv ./icon.icns binaries/darwin_x86_64/RadioShackCtl.app/Contents/Resources/RadioShackCtl.icns
cp -f ./Info.plist binaries/darwin_x86_64/RadioShackCtl.app/Contents/Info.plist
```

## Preparing install package
