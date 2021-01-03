@echo off

REM set DISPLAY variable for use with X-Window
if "%DISPLAY%" == "" call :setdisplay
echo DISPLAY defined as: %DISPLAY%

REM set path variable for SpecPR and Tetracorder
if "%TETRACORDER_BIN%" == "" call :settetracorder
echo TETRACORDER_BIN defined as: %TETRACORDER_BIN%

REM start SpecPR
dspecpr.exe %1 -gxterm -

REM terminate batch execution
GOTO :batch-end



REM subroutine ----------------------------------
REM subroutine to set DISPLAY variable for use with X-Window
:setdisplay

set DISPLAY=localhost:0.0
REM set DISPLAY=127.0.0.1:0.0

GOTO :eof



REM subroutine ----------------------------------
REM subroutine to set bin directory path and variable
:settetracorder
set TETRACORDER_BIN=%~dp0
PATH=%PATH%;%TETRACORDER_BIN%
GOTO :eof



:batch-end
echo "Program Finished"
rem exit

:eof