#!/bin/bash
ps aux | grep /varwww/positionsites | awk '{print $2}' | xargs kill
mv /var/www/positionsites /var/www/positionsites-old
mv /var/www/positionsites-new /var/www/positionsites
echo 0
