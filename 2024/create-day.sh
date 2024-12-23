#!/bin/bash

set -e
number=$1

# Check if argument is specified
if [ -z "$number" ]; then
    echo "Correct usage: $0 <day-number>"
    exit 1
fi

# Create data/day-{number}.txt file if it doesn't exist
if [ ! -f "data/day-${number}.txt" ]; then
    touch "data/day-${number}.txt"
    echo "Created data/day-${number}.txt"
else
    echo "data/day-${number}.txt already exists"
fi

# Create src/day-{number}.ts file
if [ ! -f "src/day-${number}.ts" ]; then
    touch "src/day-${number}.ts"
    sed "s/dayNumber/$number/g" day-template.ts > "src/day-${number}.ts"
    echo "Created src/day-${number}.ts"
else
    echo "src/day-${number}.ts already exists"
fi
