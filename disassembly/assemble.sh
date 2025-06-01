#!/bin/bash

#VERSION=eva_enh              # This is the Name of the assembly, uncomment only one!
#VERSION=eva_ext             # This is the Name of the assembly, uncomment only one!
VERSION=eva_orig            # This is the Name of the assembly, uncomment only one!

echo Changing to Directory: $VERSION
cd $VERSION
../a09 -B${VERSION}.bin -L${VERSION}.lst -OM03 -OFBG -DFILCHR=$FF ${VERSION}.asm

dd if=${VERSION}.bin of=tmp.bin bs=1 count=4078 # Get the Binary without Vectors
dd if=../../monitor/monitor_142.bin of=mon.bin bs=1024 count=4 # Get only the monitor without Vectors
dd if=../../monitor/monitor_142.bin of=mtmp.bin bs=1 count=18 skip=8174 iflag=skip_bytes,count_bytes # Get only the Vectors

# use this for reset-start to monitor
#cat mon.bin tmp.bin mtmp.bin > rom.bin # Build | Monitor | EVA Firmware | Monitor Vectors

# or this for start to EVA1
cat mon.bin ${VERSION}.bin > rom.bin # Build | Monitor | EVA Firmware | EVA Vectors
rm tmp.bin
rm mtmp.bin
rm mon.bin
cd ../