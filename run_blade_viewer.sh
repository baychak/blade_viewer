#! /bin/bash

cd build &&
make &&
./bin/blade_viewer $1
# eog ../result/suction-side.jpg
