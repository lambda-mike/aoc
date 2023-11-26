#!/usr/bin/env bash
add-apt-repository ppa:eiffelstudio-team/ppa 
apt-get -y update
apt-get -y install git build-essential castxml libgc-dev
DEBIAN_FRONTEND=noninteractive apt-get -y --force-yes install eiffelstudio
# git clone git://git.sv.gnu.org/liberty-eiffel.git
# cd liberty-eiffel

# wget 'https://quantum-mirror.hu/mirrors/pub/gnusavannah/liberty-eiffel/bell.tar.gz'
# tar -zxvf bell.tar.gz
# cd bell
# ./install.sh -bootstrap
# PATH=$PATH:~/bell/target/bin
# export PATH
