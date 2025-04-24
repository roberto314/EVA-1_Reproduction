#!/bin/bash
#
NAME2=monitor_142

ASS=./as1h
#DIR="/home/rob/Projects/mc-6502_mini/"
PWD=`pwd`
echo $PWD

${ASS} ${NAME2}.asm -l > ${NAME2}.lst
#srec_cat ${NAME2}.s19 -motorola -o ${NAME2}.bin -binary
srec_cat ${NAME2}.s19 -motorola -offset -0xe000 -o ${NAME2}.bin -binary # strip first 0xbfff bytes
