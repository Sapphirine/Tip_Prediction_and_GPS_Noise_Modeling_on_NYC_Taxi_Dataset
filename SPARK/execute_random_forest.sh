#!/usr/bin/env bash

EXE=spark-submit
OPT=" --deploy-mode cluster --executor-memory 30g"
FILENAME=apply-random-forest.py
NTREE=1000
TYPE1=before
TYPE2=during
TYPE3=finished

if [ "$1" = "before" ]; then
    echo 1
    nohup ${EXE} ${FILENAME} ${TYPE1} ${NTREE} > ${FILENAME}.${TYPE1}.out 2>&1&
elif [ "$1" = "during" ]; then
    echo 2
    nohup ${EXE} ${FILENAME} ${TYPE2} ${NTREE} > ${FILENAME}.${TYPE2}.out 2>&1&
else
    echo 3
    nohup ${EXE} ${FILENAME} ${TYPE3} ${NTREE} > ${FILENAME}.${TYPE3}.out 2>&1&
fi
