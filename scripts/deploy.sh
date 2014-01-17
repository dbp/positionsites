#!/bin/bash
ps aux | grep [p]ositionsites | awk '{print $2}' | xargs kill
mv /var/www/positionsites /var/www/positionsites-old
mv /var/www/positionsites-new /var/www/positionsites
echo 0
