#! /bin/bash
/usr/local/bin/emacsclient -e "(ry/alfred-command $1 \"$2\")" >& ~/ry-alfred.stdout
cat ~/ry-alfred.msg
