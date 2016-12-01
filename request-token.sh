#!/bin/sh

# Usage: ./request-token code
#   where <code> is the code from request-pin.sh response

CLID="fJOE8mFJLpUMmNdBoebNd1oD17opxZD3"

curl -X POST "https://api.ecobee.com/token?grant_type=ecobeePin&code=$1&client_id=$CLID"
