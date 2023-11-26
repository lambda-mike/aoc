#!/usr/bin/env bash

echo 'deb [trusted=yes] http://apt.liberty-eiffel.org/ release main' >> /etc/apt/sources.list
apt-get --yes update
apt-get --yes install liberty-eiffel-all

se compile day07.e -o day07 -boost -O2
./day07
