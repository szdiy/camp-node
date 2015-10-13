#!/bin/bash

git pull
sudo service cron stop
kill `ps aux | grep "[v]d\.py" | awk '{print $2}'`
sudo service cron start
