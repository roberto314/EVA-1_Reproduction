#!/bin/bash

NAME=eva_final2.bin
./a09 -Beva1.bin -Leva.lst -OM03 -OFBG -DFILCHR=$FF eva_rob.asm
dd if=eva1.bin of=tmp.bin bs=1 count=4078
dd if=../monitor/monitor_142.bin of=mon.bin bs=1024 count=4
dd if=../monitor/monitor_142.bin of=mtmp.bin bs=1 count=18 skip=8174 iflag=skip_bytes,count_bytes
# use this for start to monitor
#cat mon.bin tmp.bin mtmp.bin > $NAME
# or this for start to EVA1
cat mon.bin eva1.bin > $NAME
rm tmp.bin
rm mtmp.bin
