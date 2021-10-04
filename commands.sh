#!/bin/sh

echo "starting SM ITVC_ALL"
cd service-manager-config
git pull
sm --start ITVC_ALL --appendArgs '{"CITIZEN_DETAILS":["-Dmongodb.cid-sautr-cache.enabled=false"]}' -r
tail -f /dev/null