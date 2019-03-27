#!/bin/bash

# for i in 1 2 3 4 5
# do
#   echo "Looping ... number $i"
# done

prog1=CRANK
prog2=APP
prog3=BIT

for i in 1 2 3 4 5
         # for i in 1
do
    kdrp-test-1 $prog1 &
    pid1=$!
    kdrp-test-1 $prog2 &
    pid2=$!

    wait -n $pid1 $pid2

    es=$?

    pid=$(ps --no-header -p $pid1,$pid2 -o pid)

    if [ $pid -eq $pid1 ]
    then
        # APP killed
        echo $prog2 has terminated with status $es
        echo killing $prog1 ...

        if [ $es -eq 0 ] || [ $es -eq 10 ]
        then
            echo SUCCESSFUL or BIT exit
            kill -9 $pid >>/dev/null

            if [ $es -eq 0 ]
            then
                echo Exiting normally ...
                exit
            else
                echo Starting BIT...
                kill -9 $pid
                sleep 10

                kdrp-test-1 $prog1 &
                pid1=$!
                kdrp-test-1 $prog3 &
                pid3=$!

                wait -n $pid1 $pid3

                echo Exiting BIT ...

                pid=$(ps --no-header -p $pid1,$pid3 -o pid)

                kill -9 $pid >>/dev/null
                exit
            fi
        fi
    else
        echo $prog1 has terminated with status $es
        echo killing $prog2 ...
    fi
    kill -9 $pid >>/dev/null

    echo Clearing queues...
    sleep 1
    echo Restarting...
    sleep 20
done
