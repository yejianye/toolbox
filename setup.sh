#!/bin/bash
which pip > /dev/null
if [ $? -eq 1 ]; then
    sudo easy_install pip
fi 
sudo pip install fabric
fab custom_fabfile
echo "Toolbox initialized"
echo "To install all essential software and packages, run" 
echo "   fab -I all" 
echo 'Note, make sure `ssh localhost` is working before executing the command.'
