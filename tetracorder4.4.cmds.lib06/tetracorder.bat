@echo off

REM ***** REPLACE the 10 Ps' below with the Windows path to .\bin-pc *****
set binpc="PPPPPPPPPP"

REM call Tetracorder using batch file in bin directory
%binpc%\tetracorder.bat %1

:eof