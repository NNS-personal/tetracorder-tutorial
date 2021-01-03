@echo off

REM set path variable for SPECPR and Tetracorder
if "%TETRACORDER_BIN%" == "" call :settetracorder
echo TETRACORDER_BIN defined as: %TETRACORDER_BIN%

REM start Tetracorder
REM ..\bin-pc\dtetracorder %1 | tee tetracorder.out
REM ..\bin-pc\dtetracorder r1 <cmds.start.t4.2

rem dtetracorder %1
rem very simple Win64 identifier (not for Win 32bit OS's w/ 64bit CPUs)
IF EXIST "%PROGRAMFILES(X86)%" (
   echo starting 64-Bit Tetracorder
   dtetracorder_x86_64-w64-mingw32_64-bit %1
) ELSE (
   echo starting 32-Bit Tetracorder
   dtetracorder_i686-w64-mingw32_32-bit %1
)

REM lists all subdirectories in CWD, then call subroutine to gzip and
REM    create ENVI batch image load file
for /d %%i in (case.*,group.*) do (call :zip-n-enviload %%i)

REM terminate batch execution
GOTO :batch-end


REM subroutine ----------------------------------
REM subroutine to set bin directory path and variable
:settetracorder
set TETRACORDER_BIN=%~dp0
PATH=%PATH%;%TETRACORDER_BIN%
GOTO :eof



REM subroutine ----------------------------------
REM subroutine to call Davinci and perform cluster analysis
:zip-n-enviload

REM compress mineral maps with gzip
echo "zipping *.fd in %1 subdirectory"
gzip %1\*.fit
gzip %1\*.depth
gzip %1\*.fd

REM generate ENVI load list (Fit*Depth)
echo ENVI File List > %1\aaa-enviload.txt
dir /B %1\*.fd.gz >> %1\aaa-enviload.txt
GOTO :eof



:batch-end
echo "Program Finished"
rem exit

:eof