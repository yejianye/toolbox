#! /bin/bash
/opt/homebrew/bin/emacsclient -e "(ry/alfred-command $1 \"$2\")" >& ~/ry-alfred.stdout
cat ~/ry-alfred.msg
