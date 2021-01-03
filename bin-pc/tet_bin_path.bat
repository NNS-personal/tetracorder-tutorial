@echo off
REM appends current working directory (bin-pc) to PATH environmental variable
if "%TETRACORDER_BIN%" == "" goto :settetracorder
goto :eof

:settetracorder
set TETRACORDER_BIN=%~dp0
PATH=%PATH%;%TETRACORDER_BIN%

:eof