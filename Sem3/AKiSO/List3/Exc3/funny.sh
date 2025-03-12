#!/bin/bash

cat_image_url=$(curl -s https://api.thecatapi.com/v1/images/search | jq -r '.[0].url')

if [[ -z "$cat_image_url" ]]; then
    echo "Failed to fetch a cat image URL."
    exit 1
fi

quote=$(curl -s https://api.chucknorris.io/jokes/random | jq -r '.value')

curl -s "$cat_image_url" | catimg -
echo -e "\n$quote"
