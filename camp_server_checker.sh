#!/bin/bash

pid=`ps aux | grep guile | grep vd.scm | awk '{print $2}'`

if [[ -z pid ]]; then
   exec guile /var/www/vd.scm 1> /var/www/vd.log 2> /var/www/vd.err &
fi
