#!/bin/bash
ps aux | grep [p]ositionsites | awk '{print $2}' | xargs kill
echo 0
