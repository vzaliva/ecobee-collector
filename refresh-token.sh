#!/bin/sh

# Usage: ./refresh-token client_id refresh_token 

curl -X POST "https://api.ecobee.com/token?grant_type=refresh_token&refresh_token=$2&client_id=$1"

                
