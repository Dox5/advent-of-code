#!/usr/bin/env bash

YEAR=${YEAR:-2024}

if [[ -z "$DAY" ]] ; then
    echo "Please set DAY"
fi

echo "Fetching input for ${YEAR}-${DAY}"

AOC_DAY=$(printf "%d" ${DAY})
OUTPUT=$(printf "day%0.2d.txt" ${DAY})

exec curl -o "inputs/${OUTPUT}" --cookie cookie-store "https://adventofcode.com/${YEAR}/day/${DAY}/input"
