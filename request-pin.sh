#!/bin/sh

CLID="fJOE8mFJLpUMmNdBoebNd1oD17opxZD3"

curl -X GET "https://api.ecobee.com/authorize?response_type=ecobeePin&client_id=$CLID&scope=smartRead"

