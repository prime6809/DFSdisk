dfsdisk create test.ssd -t 40 -q $20 -L 1770DOS
dfsdisk write test.ssd -f ../System-ROM/FORM40 -q $20 -l $2800 -e $2800
dfsdisk write test.ssd -f ../System-ROM/FORM80 -q $20 -l $2800 -e $2800
dfsdisk write test.ssd -f ../System-ROM/VERIFY4 -q $20 -l $2800 -e $2800
dfsdisk write test.ssd -f ../System-ROM/VERIFY8 -q $20 -l $2800 -e $2800

copy test.ssd d:\emulate\software\atom\disks\TEST.DSK
