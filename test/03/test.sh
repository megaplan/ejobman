#!/bin/sh
tr_sub(){
    date
}
export LANG=C
trap tr_sub INT QUIT HUP
TAG="test.sh"
#while(read vvv)
while(/bin/true)
do
    #vvv=`od -v -t x1`
    read vvv
    res=$?
    if [ $res = 0 ] ; then
        logger -t "$TAG" "read res true: $res"
    else
        logger -t "$TAG" "read res false: $res"
        break
    fi
    logger -t "$TAG" "read var: $vvv"
    echo "read var1: $vvv on `date`"
    echo "read var2: $vvv on `date`"
    echo "read var3: $vvv on `date`"
    sleep 1
done
logger -t "$TAG" "exit"
