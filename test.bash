#!/bin/bash

files=($(ls -v ./test/test_files/))
answer=($(ls -v ./test/answer_files/))
nb_test_files=${#files[@]}

GREEN='\033[0;32m'
RED='\033[0;31m'

for ((i=0; i < $nb_test_files; i++))
do
    test_result=$(./glados ./test/test_files/${files[i]})
    if [ "$test_result" == "./LLVM.IR Builded" ]
    then
        test=$(make bin; ./a.out)
    else
        continue
    fi

    real_result=$(cat ./test/answer_files/${answer[i]})
    if [ "$test" == "$real_result" ]
    then
        echo -e "${GREEN}Test $i passed${GREEN}"
    else
        echo -e "${RED}Test $i failed${RED}"
        echo -e "${RED}RES:\n$test_result${RED}"
        echo -e "${RED}EXPECTED:\n$real_result${RED}"
    fi

done