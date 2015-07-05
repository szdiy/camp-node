#!/bin/bash

pid=`ps aux | grep python | grep vd.py | awk '{print $2}'`

if [[ !pid ]]; then
   cd /home/pi/camp-node/
   python vd.py 1> vd.log 2> vd.err &
fi
