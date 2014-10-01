#!/bin/bash
which pip > /dev/null
if [ $? -eq 1 ]; then
    sudo easy_install pip
fi 
sudo pip install fabric
fab custom_fabfile
