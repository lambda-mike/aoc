#!/usr/bin/env bash
echo 'deb [trusted=yes] http://apt.liberty-eiffel.org/ release main' >> /etc/apt/sources.list
apt-get update
apt-get install liberty-eiffel-all
