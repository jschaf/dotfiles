#!/bin/sh

if [ "$#" -lt 2 ]; then
  echo "Too few arguments\n"
  echo "Usage: $0 'PATH TO INTELLIJ IDEA APP' 'PATH_VALUE'" >&2
  exit 1
fi
if [ "$#" -gt 2 ]; then
  echo "Too many arguments\n"
  echo "Usage: $0 'PATH TO INTELLIJ IDEA APP' 'PATH_VALUE'" >&2
  exit 1
fi
if ! [ -e "$1" ]; then
  echo "'$1' not found\n" >&2
  echo "Usage: $0 'PATH TO INTELLIJ IDEA APP' 'PATH_VALUE'" >&2
  exit 1
fi

# add shell script to app package
cat > "$1/Contents/MacOS/idea.sh" <<- EOF
#!/bin/sh
export PATH="$2"
logger "\`dirname \"\$0\"\`/idea"
exec "\`dirname \"\$0\"\`/idea" \$@
EOF
# make shell script executable
chmod +x "$1/Contents/MacOS/idea.sh"
# run shell script when starting app
defaults write "$1/Contents/Info" CFBundleExecutable idea.sh
# writing the plist converts it to binary format, convert it back to xml
plutil -convert xml1 "$1/Contents/Info.plist"
# re-register to launch services database
/System/Library/Frameworks/CoreServices.framework/Frameworks/LaunchServices.framework/Support/lsregister -v -f "$1"
