#!/bin/bash

files=($(ls -v ./test/test_files/))
answer=($(ls -v ./test/answer_files/))
nb_test_files=${#files[@]}

GREEN='\033[0;32m'
RED='\033[0;31m'


for ((i=0; i < $nb_test_files; i++))
do
echo "-----------------------------------------------"

    test_result=$(./glados ./test/test_files/${files[i]})
    ret=$?
    if [ -z "$ret" ]
    then
        continue

    else
        make -S rebin
        ./bin
        test=$(echo $?)
    fi

    real_result=$(cat ./test/answer_files/${answer[i]})
    if [ "$test" == "$real_result" ]
    then
        echo -e "${GREEN}Test $i passed${GREEN}"
    else
        echo -e "${RED}Test $i failed${RED}"
        echo -e "${RED}RES:\n${test}{RED}"
        echo -e "${RED}EXPECTED:\n${real_result}${RED}"
    fi

done