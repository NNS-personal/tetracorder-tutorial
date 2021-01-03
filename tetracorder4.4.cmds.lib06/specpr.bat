@echo off

REM ***** REPLACE the 10 Ps' below with the Windows path to .\bin-pc *****
set binpc="PPPPPPPPPP"

REM call specpr using batch file in bin directory
%binpc%\specpr.bat %1

:eof