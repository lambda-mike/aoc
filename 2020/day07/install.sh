#!/usr/bin/env bash
apt-get update
apt-get install git build-essential castxml libgc-dev
git clone git://git.sv.gnu.org/liberty-eiffel.git
cd liberty-eiffel
# wget 'https://quantum-mirror.hu/mirrors/pub/gnusavannah/liberty-eiffel/bell.tar.gz'
# tar -zxvf bell.tar.gz
# cd bell
./install.sh -bootstrap
PATH=$PATH:~/bell/target/bin
export PATH
