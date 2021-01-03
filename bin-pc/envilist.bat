REM generate ENVI load list (Fit*Depth)
echo ENVI File List > case.ep-cal-chl\aaa-enviload.txt
dir /B case.ep-cal-chl\*.fd.gz >> case.ep-cal-chl\aaa-enviload.txt
echo ENVI File List > case.red-edge\aaa-enviload.txt
dir /B case.red-edge\*.fd.gz >> case.red-edge\aaa-enviload.txt
echo ENVI File List > case.veg.type\enviload.txt
dir /B case.veg.type\*.fd.gz >> case.veg.type\aaa-enviload.txt
echo ENVI File List > group.1um\aaa-enviload.txt
dir /B group.1um\*.fd.gz >> group.1um\aaa-enviload.txt
echo ENVI File List > group.2um\aaa-enviload.txt
dir /B group.2um\*.fd.gz >> group.2um\aaa-enviload.txt
echo ENVI File List > group.2.5um-broad\aaa-enviload.txt
dir /B group.2.5um\*.fd.gz >> group.2.5um\aaa-enviload.txt
echo ENVI File List > group.2um-broad\aaa-enviload.txt
dir /B group.2um-broad\*.fd.gz >> group.2um-broad\aaa-enviload.txt
echo ENVI File List > group.ree\aaa-enviload.txt
dir /B group.ree\*.fd.gz >> group.ree\aaa-enviload.txt
echo ENVI File List > group.veg\aaa-enviload.txt
dir /B group.veg\*.fd.gz >> group.veg\aaa-enviload.txt