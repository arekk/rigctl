#!/usr/bin/env bash
#
# Release build for RadioShackCtl
#
# Output: build/RadioShackCtl_<version>_<cpu>.dmg containing RadioShackCtl.app
#
# Usage:
#   ./build.sh                              # default arm64 build, version=YYYYMMDD
#   VERSION=0.3 ./build.sh                  # explicit version in DMG filename
#   CODESIGN_IDENTITY="Developer ID Application: ..." ./build.sh
#   TARGET_CPU=x86_64 ./build.sh            # Intel build (requires x86_64 FPC units)
#
# Requirements:
#   - Lazarus + FPC at /Applications/fpc/Lazarus/ (fpcupdeluxe install)
#   - iconutil, hdiutil, codesign (ship with macOS / Xcode CLT)
#   - Project compiles cleanly via lazbuild
#
# Note on compile-time optimizations:
#   This script applies post-build optimizations (symbol strip, UDZO compression).
#   For -O3 compile-time optimization, in Lazarus IDE:
#     Project Options -> Compiler Options -> Custom Options -> add: -O3 -XX -CX
#   (kept out of .lpi here to avoid affecting Default debug builds.)
#

set -euo pipefail

PROJECT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$PROJECT_DIR"

APP_NAME="${APP_NAME:-RadioShackCtl}"
VERSION="${VERSION:-$(date +%Y%m%d)}"
LAZBUILD="${LAZBUILD:-/Applications/fpc/Lazarus/lazarus/lazbuild}"
TARGET_CPU="${TARGET_CPU:-aarch64}"
TARGET_OS="${TARGET_OS:-darwin}"

BIN_DIR="binaries/${TARGET_OS}_${TARGET_CPU}"
BIN="${BIN_DIR}/${APP_NAME}"
APP_BUNDLE="${BIN_DIR}/${APP_NAME}.app"
STAGING="build/dmg-staging"
DMG="build/${APP_NAME}_${VERSION}_${TARGET_CPU}.dmg"

echo "==> ${APP_NAME} release build (${TARGET_CPU}-${TARGET_OS}, version=${VERSION})"

# 1. Sanity check on inputs
[ -f rshackctr.lpi ] || { echo "ERROR: rshackctr.lpi not found in $PROJECT_DIR" >&2; exit 1; }
[ -f Info.plist ]    || { echo "ERROR: Info.plist not found in $PROJECT_DIR" >&2; exit 1; }
[ -x "$LAZBUILD" ]   || { echo "ERROR: lazbuild not executable at $LAZBUILD" >&2; exit 1; }

# 2. Clean previous artifacts (force fresh full rebuild)
echo "==> Cleaning lib/, previous .app and .dmg..."
rm -rf "lib/${TARGET_CPU}-${TARGET_OS}" "$APP_BUNDLE" "$STAGING" "$DMG"
mkdir -p build

# 3. Compile (full rebuild via -B)
echo "==> Compiling via lazbuild..."
"$LAZBUILD" --cpu="$TARGET_CPU" --os="$TARGET_OS" -B rshackctr.lpi

[ -x "$BIN" ] || { echo "ERROR: $BIN not produced" >&2; exit 1; }
SIZE_BEFORE=$(stat -f%z "$BIN")

# 4. Strip symbols (~10-15% size reduction on this project — modest because most
#    of the binary is LCL/Indy code, not debug symbols).
echo "==> Stripping symbols..."
strip -x "$BIN"
SIZE_AFTER=$(stat -f%z "$BIN")
echo "    binary: $((SIZE_BEFORE / 1024)) KB -> $((SIZE_AFTER / 1024)) KB"

# 5. Assemble .app bundle
echo "==> Assembling $APP_BUNDLE..."
mkdir -p "${APP_BUNDLE}/Contents/MacOS" "${APP_BUNDLE}/Contents/Resources"
cp -f Info.plist "${APP_BUNDLE}/Contents/Info.plist"
mv -f "$BIN" "${APP_BUNDLE}/Contents/MacOS/${APP_NAME}"

# 6. Generate icon from iconset (if present)
if [ -d "icon.iconset" ]; then
  echo "==> Generating .icns from icon.iconset..."
  iconutil --convert icns icon.iconset \
    -o "${APP_BUNDLE}/Contents/Resources/${APP_NAME}.icns"
else
  echo "    (icon.iconset not found, skipping icon generation)"
fi

# Touch so Finder/Launch Services refreshes the bundle cache
touch "$APP_BUNDLE"

# 7. Optional code signing (set CODESIGN_IDENTITY to enable)
if [ -n "${CODESIGN_IDENTITY:-}" ]; then
  echo "==> Code-signing as: $CODESIGN_IDENTITY"
  codesign --force --options runtime --timestamp \
    --sign "$CODESIGN_IDENTITY" \
    "$APP_BUNDLE"
  codesign --verify --deep --strict --verbose=2 "$APP_BUNDLE"
else
  echo "    (CODESIGN_IDENTITY not set, app bundle will be unsigned)"
fi

# 8. Stage for DMG (.app + Applications symlink for drag-to-install UX)
echo "==> Staging DMG contents..."
mkdir -p "$STAGING"
cp -R "$APP_BUNDLE" "$STAGING/"
ln -s /Applications "$STAGING/Applications"

# 9. Create compressed read-only DMG
echo "==> Creating $DMG..."
hdiutil create \
  -volname "$APP_NAME" \
  -srcfolder "$STAGING" \
  -ov \
  -format UDZO \
  -imagekey zlib-level=9 \
  "$DMG" >/dev/null

# 10. Cleanup staging (DMG already created)
rm -rf "$STAGING"

# 11. Report
DMG_SIZE=$(stat -f%z "$DMG")
echo ""
echo "==> Done"
echo "    DMG:    $DMG ($((DMG_SIZE / 1024)) KB)"
echo "    Bundle: $APP_BUNDLE"
[ -n "${CODESIGN_IDENTITY:-}" ] && echo "    Signed: yes"
echo ""
echo "Next steps for distribution:"
echo "  - Notarize:  xcrun notarytool submit '$DMG' --apple-id <id> --team-id <team> --password <pw> --wait"
echo "  - Staple:    xcrun stapler staple '$DMG'"
