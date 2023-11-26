#!/usr/bin/env bash
wget 'https://quantum-mirror.hu/mirrors/pub/gnusavannah/liberty-eiffel/bell.tar.gz'
tar -zxvf bell.tar.gz
cd bell
./install.sh -bootstrap
PATH=$PATH:~/bell/target/bin
export PATH
