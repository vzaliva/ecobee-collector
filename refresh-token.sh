#!/bin/sh

# Usage: ./refresh-token code
#   where <code> is the 'refresh_token' from request-token.sh or refresh-token.sh response

CLID="fJOE8mFJLpUMmNdBoebNd1oD17opxZD3"

curl -v -X POST "https://api.ecobee.com/token?grant_type=refresh_token&refresh_token=$1&client_id=$CLID"

                
