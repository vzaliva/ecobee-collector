#!/bin/sh

#param client API key

curl -X GET "https://api.ecobee.com/authorize?response_type=ecobeePin&client_id=$1&scope=smartRead"

