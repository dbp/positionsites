#!/bin/bash
ps aux | grep [p]ositionsites | awk '{print $2}' | xargs kill
cp /var/www/positionsites-old /var/www/positionsites
