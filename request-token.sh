#!/bin/sh

# Usage: ./request-token code pin


curl -X POST "https://api.ecobee.com/token?grant_type=ecobeePin&code=$1&client_id=$2"
