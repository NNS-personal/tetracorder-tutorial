@echo off

SETLOCAL ENABLEEXTENSIONS
SETLOCAL DISABLEDELAYEDEXPANSION

REM invoke:  tetsetup  sub_directory  data_set  image_cube

REM script to make sub-directories and install all needed tetracorder command files
REM to perform a standard mapping run.
REM
REM                  Dec. 12, 1994, 3.7 installation May 11, 2001
REM                  t4.4 fall 2012.
REM                  Major updates 2012 for t4.4 and multiple instruments

REM make a sub directory and install all commands needed to run tetracorder.

REM ####################################################################
REM #### NOTE: THINGS TO CHANGE IF THIS SCRIPT IS MOVED:          ######
REM                                                               ######
   set source=c:\info\tetracorder\tetracorder4.4.cmds.lib06
   set binpc=c:\usr\local\bin-pc
   set speclib-path=c:\speclib\library06.conv
REM                                                               ######
REM After program startup, 'source' will be set to the called     ######
REM   directory of tetsetup (where tetsetup resides, see below),  ######
REM   if original defined path above is not used (does not exist) ######
REM                                                               ######
REM   also, for 'binpc', it's parent directory will be set to     ######
REM   the parent directory of \tetracorder4.4.cmds.lib06          ######
REM                                                               ######
REM   also, for 'speclib', it's parent directory will be set to   ######
REM   the parent directory of \tetracorder4.4.cmds.lib06          ######
REM                                                               ######
REM ####################################################################
REM #### NOTE: THINGS TO CHANGE FOR NEW VERSIONS                  ######
REM                                                               ######
   set lib=cmd.lib.setup.t4.4a5s6
   set start=cmds.start.t4.4a
   set starts=cmds.start.t4.4a.single
REM                                                               ######
REM ### the following instrument specific libraries get set to    ######
REM ### the command library $lib in the $start and $starts        ######
REM ### libraries (see case statement where restart is defined.   ######
REM ### Future tetracorder will not need custom libraries.        ######
REM                                                               ######
rem MMM targeted a
   set libmma=cmd.lib.setup.t4.4a4_mm260as6
rem MMM targeted a
   set libmm09c=cmd.lib.setup.t4.4a4_mm85cs6
rem Hymap_2007a 124 channels
   set libhy07a=cmd.lib.setup.t4.4a4_hy07as6
REM                                                               ######
REM                                                               ######
REM ####################################################################

REM  variables for the three command arguments:
set dir=
set data_set=
set cube=

REM  environmental variables:
REM  'data-set' defined, 'restart-file' used, and 'deleted-points-set' to use
set data=
set restart=
set dpt=
set a1=
set dpt-line=
set a2=
set cube-line=
set a3=
set cmdlib-line=
set cmdlib-line_tmp=
set calling_cwd=

REM check for 3 arguments following batch command
   if [%3]==[] (
      call :usage
         echo .
         echo .
         echo .   Argument input ERROR, three arguments required, see above !!!
         echo .
            rem argument 3 is not present: terminate batch file
         GOTO :eof
   )

   rem NOTE: if "%1"=="" call :usage  (also works)
   rem or:   if DEFINED MyVar  (requires ENABLEEXTENSIONS)


REM assign SYSTEM VARIABLES ********************************************
REM ----- assign SYSTEM VARIABLES from command-line argument list ------

   rem see above for assigning variables: (cmd.) lib, (cmds.) start, and (cmds.) starts

   rem assign batchfile variables
      if exist %1 (
         call :usage
         echo .
         echo .
         echo .   Argument input ERROR, SUB-DIRECTORY:  %1  already EXISTS !!!
         echo .
            rem directory exists: terminate batch file
         echo Terminating batch program execution
         GOTO :eof
      ) else (
         rem variable 'dir' set to 'a new' directory (arg 1), strip off any double quotes
            set dir=%~1
      )

   rem assign 'data_set' (arg 2), strip off any double quotes
      set data_set=%~2

   rem assign 'image_cube' (arg 3), strip off any double quotes
      set cube=%~3

   rem format 'image_cube' for UNIX format forward-slashes
   rem   (substitute forward-slashes for any (all) back-slashes)
      set cube=%cube:\=/%

      set drive-dos=C:
      set cube-dos=%cube%

   rem remove <drive> from cube string if present
      if %cube:~1,1%==: (
         set drive-dos=%cube:~0,2%
         set cube=%cube:~2%
      )

REM Kludge for now; Cygwin requires either backslashes or c: at start of DOS path
REM      (for Windows versions of Tetracorder & SpecPR; Davinci [MinGW] uses unix form) 
      if %cube-dos:~0,1%==/ set cube-dos=%drive-dos%%cube%


REM ----- assign SYSTEM VARIABLES from relative and absolute paths -----

   rem set 'source' to present tetsetup path if original defined path is not used
   rem   The tetsetup path (containing the file 'DELETED.channels') will be assumed
   rem   %source% path will have a trailing '\' if set using  %~dp0  below
      if NOT exist %source%\DELETED.channels set source=%~dp0

   rem set calling directory path
      set calling_cwd=%cd%

REM  *** rewrite this short section if tetsetup directory is write protected ***
   rem make mapping directory
      mkdir %dir%

   rem set mapping directory path
      cd %dir%
      set mapping-path=%cd%

   rem set mapping directory parent path
      cd ..
      set mapping-parent-path=%cd%

   rem set absolute 'source' directory path
      cd %source%
      set source-path=%cd%

   rem set absolute 'source' parent-directory path
      cd ..
      set parent-source-path=%cd%


   rem set 'binpc' to present tetsetup (absolute) grandparent + \bin.pc path if original
   rem   defined path is not used
   rem   The tetsetup path (containing the file 'dtetracorder.exe') will be assumed
      if NOT exist %binpc%\dtetracorder.exe set binpc=%parent-source-path%\bin-pc

   rem return to calling directory
      cd %calling_cwd%


REM halt tetsetup if 'dir' = 'source' directory (do not want mapping dir there)

    if %mapping-parent-path%==%source-path% (
      echo .
      echo .
      echo .
      echo .
      echo ERROR: current mapping parent directory: 
      echo        %mapping-parent-path% is invalid.
      echo .
      echo .
      echo .
      echo Selecting a mapping directory within the Tetracorder commands directory
      echo                           is PROHIBITED.
      echo .
      echo .
      echo .
      echo .
      echo mapping dir  = %mapping-path%
      echo commands dir = %source-path%
      echo .
      echo .
      echo .
      echo Select another subdirectory parent for the mapping directory, then
      echo                          run tetsetup again!
      echo .
         rem cwd = 'source' directory: terminate batch file
      echo Terminating batch program execution

      rmdir %dir%
      GOTO :eof
   )


REM ----- assign remaining SYSTEM VARIABLES ----------------------------

   rem assign 'data' and 'restart' variables in subroutines below
      call :%data_set%

   rem check for valid 'data' assignment
      if [%data%]==[] (
         call :usage
         echo .
         echo .
         echo .   Argument input ERROR, DATA_SET:  %data_set%  UNRECOGNIZED !!!
         echo .
            rem non-existant data_set: terminate batch file
         GOTO :eof
      )

   rem assign variable (deleted points): dpt
      if exist %source%\DELETED.channels (
         for /F "delims=" %%a in ('findstr/C:%data% %source%\DELETED.channels') do set dpt=%%a
      ) else (
         echo .
         echo Required file 'DELETED.channels' does NOT exist in this directory
         echo .
         echo %source%\DELETED.channels
         echo .
         echo Terminating batch program execution
         echo .
         rem DELETED.channels file not found: terminate batch file
         GOTO :eof
      )


   rem copy and configure restart (r1) file

      rem r1 expects spectral library root-directory to be: /speclib/library06.conv
      rem    test for root-directory existance,
      rem    if not, then test for root-directory to be relative to
      rem    parent of tetracorder4.4.cmds.lib06, ie. %parent-source-path%
      rem    then full path would be: %parent-source-path%/speclib/library06.conv
      rem       remember to remove c: & change back-slashes to forward-slashes in: %parent-source-path%


      rem set speclib-path=c:\speclib\library06.conv
         set speclib-path-c=%speclib-path:~2%
         if NOT exist %speclib-path% set speclib-path=%parent-source-path%%speclib-path-c%
      rem then, test for new path:
         if NOT exist %speclib-path% (
            echo Spectral Library can not be found!, terminating Tetracorder Setup
            goto :eof
         )

      rem find spectral library path and file name
         set search-str=iyfl=
      rem look-up r1 'iyfl=' variable line
         for /F "delims=" %%a in ('findstr/C:%search-str% %source%\restart_files\%restart%') do set iyfl_org=%%a
            rem should yield string like:  iyfl=/speclib/librar06.conv/s06av05a
            rem skip the first 5 characters (iyfl=)
         set iyfl_org-subset=%iyfl_org:~5%

            rem define 'iyfl_new' using 'speclib' path and skip the first 2 characters (c:)
         set iyfl_new-subset=%parent-source-path:~2%

      rem format 'image_cube' for UNIX format forward-slashes
      rem   (substitute forward-slashes for any (all) back-slashes)
         set iyfl_new-subset=%iyfl_new-subset:\=/%
      rem append old path to new
         set iyfl_new-subset=%iyfl_new-subset%%iyfl_org-subset%
      rem trim iyfl string to a 77 character line as found in the 'r1' file
         set iyfl_new-subset=%iyfl_new-subset:~0,77%

REM Kludge for now; Cygwin requires either backslashes or c: at start of DOS path
         if %iyfl_new-subset:~0,1%==/ set iyfl_new-subset=c:%iyfl_new-subset%
      rem trim iyfl string to a 77 character line as found in the 'r1' file
         set iyfl_new-subset=%iyfl_new-subset:~0,77%



REM end, variable assignments ******************************************



REM show variable values to screen

   echo source directory set to:   %source%
   echo source-path directory  :   %source-path%
   echo parent-source-path is  :   %parent-source-path%
   echo bin-pc directory set to:   %binpc%
   echo present PATH (CWD) set to: %calling_cwd%
   echo mapping-path set to:       %mapping-path%
   echo mapping-parent-path to:    %mapping-parent-path%
   echo mapping directory (dir) set to: ........ %dir%
   echo dataset (data_set) set to: ............. %data_set%
   echo data (data) set to: .................... %data%
   echo restart (restart) set to: .............. %restart%
   echo image spectrometer data-cube (cube): ... %cube%
   echo image spectrometer data-cube-DOS (cube): %cube-dos%
   echo cmd library (lib) set to: .............. %lib%
   echo cmd start (start) set to: .............. %start%
   echo cmd start (starts) single set to: ...... %starts%
   echo r1 iyfl (org) set to: .................. %iyfl_org-subset%
   echo r1 iyfl (new) set to: .................. %iyfl_new-subset%
   echo deleted points (dpt) set to: ........... %dpt%



REM check that critical files exist:

   if exist %source%\%lib% (
      echo .
      echo VERIFIED: %lib% found
      echo .
   ) else (
      echo .
      echo ERROR: command library %source%\%lib% NOT found
      echo .
      GOTO :eof
   )

   if exist %source%\restart_files\%restart% (
      echo .
      echo VERIFIED: restart_files\%restart% found
      echo .
   ) else (
      echo .
      echo ERROR: %source%\restart_files\%restart% NOT found
      echo .
      GOTO :eof
   )

REM check that cmds.start file is O'K:

set a1="==[DELETPTS]DDDDDDDDDD"
set a2="cube: FFFFFFFFFF"
set a3="AAAAAACMDLIBRARY"


      rem check deleted points variable line
      for /F "delims=" %%a in ('findstr/C:%a1% %source%\%start%') do set dpt-line=%%a

      if [%dpt-line%]==[] (
         echo .
         echo ERROR: %source%\%start% file has no %a1% line
         echo .
         GOTO :eof
      ) else (
         echo .
         echo CHECK VERIFIED: %source%\%start% file o'k with '%dpt-line%' line
         echo .
      )

      rem check imaging spectrometer data cube variable line
      for /F "delims=" %%a in ('findstr/C:%a2% %source%\%start%') do set cube-line=%%a

      if ["%cube-line%"]==[] (
         echo .
         echo ERROR: %source%\%start% file has no %a2% line
         echo .
         GOTO :eof
      ) else (
         echo .
         echo CHECK VERIFIED: %source%\%start% file o'k with: '%cube-line%' line
         echo .
      )

      rem check command library variable line
      for /F "delims=" %%a in ('findstr/C:%a3% %source%\%start%') do set cmdlib-line=%%a

      if "%cmdlib-line%"=="" (
         echo .
         echo ERROR: %source%\%start% file has no %a3% line
         echo .
         GOTO :eof
      ) else (
         echo .
            rem echo starting with the second character in 'cmdlib-line' string
            rem    to skip the less-than character
            rem    could also use: '%cmdlib-line:~1,16%' start char=2, length=16 (chars 2-17)
         echo CHECK VERIFIED: %source%\%start% file o'k with '%cmdlib-line:~1%' line
         echo .
      )

REM tetrun file not checked (it's optional): 'cube=FFFFFFFFFF' not tested

REM mapping directory (dir) checked so as not to equal 'source' directory


REM *****  Make mapping directory, copy files, and config  *****

   rem make mapping directories

      rem mkdir %dir%

      rem mkdir %dir%\cmds.all.support
      mkdir %dir%\Band.images
      mkdir %dir%\group.1um
      mkdir %dir%\group.2um
      mkdir %dir%\group.veg
      mkdir %dir%\group.ree
      mkdir %dir%\group.2um-broad
      mkdir %dir%\group.2.5um
      mkdir %dir%\group.1.4um
      mkdir %dir%\group.3um
      mkdir %dir%\group.1.5um-broad
      mkdir %dir%\group.ree_neod
      mkdir %dir%\group.ree_samar
      mkdir %dir%\case.red-edge
      mkdir %dir%\case.veg.type
      mkdir %dir%\case.ep-cal-chl

   rem copy cmd.library file
      copy %source%\%lib% %dir% /v

   rem the following takes a few seconds -let the user know that the batch is still running
      echo .
      echo .
      echo .
      echo STILL PROCESSING - this will take a few seconds
      echo .
      echo .

   rem copy and configure cmds.start file, cmds.start substitution (datacube input)

      rem quotes required around dpt because of spaces in string
      if exist %dir%\cmds.start.t4.4a_assign-delpts del %dir%\cmds.start.t4.4a_assign-delpts
      call :str_replace DDDDDDDDDD "%dpt%" %source%\%start% %dir%\cmds.start.t4.4a_assign-delpts

      if exist %dir%\cmds.start.t4.4a_assign-lib del %dir%\cmds.start.t4.4a_assign-lib
      call :str_replace AAAAAACMDLIBRARY %lib% %dir%\cmds.start.t4.4a_assign-delpts %dir%\cmds.start.t4.4a_assign-lib
      if exist %dir%\cmds.start.t4.4a_assign-delpts del %dir%\cmds.start.t4.4a_assign-delpts

REM Kluge - use DOS drive if cube path started with '/' for Cygwin Tetracorder version
      if exist %dir%\%start% del %dir%\%start%
      call :str_replace FFFFFFFFFF %cube-dos% %dir%\cmds.start.t4.4a_assign-lib %dir%\%start%
      if exist %dir%\cmds.start.t4.4a_assign-lib del %dir%\cmds.start.t4.4a_assign-lib


   rem copy and configure cmds.start file, cmds.start substitution (single spectrum input)

      rem quotes required around dpt because of spaces in string
      if exist %dir%\cmds.start.t4.4a_assign-delpts del %dir%\cmds.start.t4.4a_assign-delpts
      call :str_replace DDDDDDDDDD "%dpt%" %source%\%starts% %dir%\cmds.start.t4.4a_assign-delpts

      if exist %dir%\%starts% del %dir%\%starts%
      call :str_replace AAAAAACMDLIBRARY %lib% %dir%\cmds.start.t4.4a_assign-delpts %dir%\%starts%
      if exist %dir%\cmds.start.t4.4a_assign-delpts del %dir%\cmds.start.t4.4a_assign-delpts


   rem copy and configure tetrun.bat file, tetrun substitution

REM use unix version of Cube path for Davinci (MinGW) program
      if exist %dir%\tetrun_assign-cube del %dir%\tetrun_assign-cube
      call :str_replace FFFFFFFFFF %cube% %source%\tetrun.bat %dir%\tetrun_assign-cube

      if exist %dir%\tetrun_assign-cmdstart del %dir%\tetrun_assign-cmdstart
      call :str_replace PPPPPPPPPP %binpc% %dir%\tetrun_assign-cube %dir%\tetrun_assign-cmdstart
      if exist %dir%\tetrun_assign-cube del %dir%\tetrun_assign-cube

      if exist %dir%\tetrun.bat del %dir%\tetrun.bat
      call :str_replace CMDSSTART %start% %dir%\tetrun_assign-cmdstart %dir%\tetrun.bat
      if exist %dir%\tetrun_assign-cmdstart del %dir%\tetrun_assign-cmdstart

   rem copy and configure tetracorder.bat file, tetracorder substitution
      call :str_replace PPPPPPPPPP %binpc% %source%\tetracorder.bat %dir%\tetracorder.bat

   rem copy and configure specpr.bat file, specpr substitution
      call :str_replace PPPPPPPPPP %binpc% %source%\specpr.bat %dir%\specpr.bat

   rem copy and configure restart_files\r1 file, mapping\r1 substitution
      set restart-file_in=%source%restart_files\%restart%
      call :str_replace %iyfl_org-subset% %iyfl_new-subset% %restart-file_in% %dir%\r1


   rem copy davinci scripts
      rem   copy %source%\cmds.all.support\* %dir%\cmds.all.support /v
      xcopy %source%\cmds.all.support %dir%\cmds.all.support\ /S /E /V /Q

      call :str_replace  .fd.gz  .fd  %dir%\cmds.all.support\davinci.add.iron.oxides  %dir%\cmds.all.support\davinci.add.iron.oxides-pc
      call :str_replace  .fd.gz  .fd  %dir%\cmds.all.support\davinci.add.kaol+smect   %dir%\cmds.all.support\davinci.add.kaol+smect-pc
      call :str_replace  .fd.gz  .fd  %dir%\cmds.all.support\davinci.alunite+kaolinite.muscovite %dir%\cmds.all.support\davinci.alunite+kaolinite.muscovite-pc
      call :str_replace  .fd.gz  .fd  %dir%\cmds.all.support\davinci.add.ALUNITE_COMP %dir%\cmds.all.support\davinci.add.ALUNITE_COMP-pc
      call :str_replace  .fd.gz  .fd  %dir%\cmds.all.support\davinci.add.ALUN+PYROPH  %dir%\cmds.all.support\davinci.add.ALUN+PYROPH-pc
      call :str_replace  .fd.gz  .fd  %dir%\cmds.all.support\davinci.add.KAOL+MUSC    %dir%\cmds.all.support\davinci.add.KAOL+MUSC-pc
      call :str_replace  .fd.gz  .fd  %dir%\cmds.all.support\davinci.add.PYROPH+MUSC  %dir%\cmds.all.support\davinci.add.PYROPH+MUSC-pc
      call :str_replace  .fd.gz  .fd  %dir%\cmds.all.support\davinci.add.TALCS        %dir%\cmds.all.support\davinci.add.TALCS-pc
      call :str_replace  .fd.gz  .fd  %dir%\cmds.all.support\davinci.add.generic_Fe2+ %dir%\cmds.all.support\davinci.add.generic_Fe2+-pc


REM terminate batch file
GOTO :batch-end


REM ------------------------------------------------------------------
REM ------------------- Subroutines below ----------------------------
REM ------------------------------------------------------------------

REM subroutine -------------------------------------------------------
REM subroutine to display tetsetup usage with no arguments
:usage
	echo ""
	echo ""
	echo ""
	echo "Usage: tetsetup   sub_directory   data_set   image_cube"
	echo ""
	echo "sub_directory is the directory where the commands go,"
	echo "			and it must not already exist."
	echo ""
	echo "Valid data sets are:"
	echo "             AVIRIS:"
        echo "                          aviris_2010"
	echo "				aviris_2006"
	echo "				aviris_2005"
	echo "				aviris_2001"
	echo "				aviris_2000"
	echo "				aviris_1999"
	echo "				aviris_1998"
	echo "				aviris_1997"
	echo "				aviris_1996"
	echo "				aviris_1995"
	echo "				aviris_1994"
	echo "				aviris_1994"
	echo "				Sept_1993"
	echo "				May_1993"
	echo "				bodie_1992"
	echo "				1992_no_shift"
	echo "				CRISM:"
	echo "				CRISM_438a"
	echo "				CRISM_72b"
	echo "				Moon Mineralogy Mapper"
        echo "				MMM_09c   # 2009 global mode 2009 85 channels"
	echo "				VIMS:"
	echo "				VIMS2007"
	echo "				HYMAP:"
	echo "				HyMap_1998"
        echo "				HyMap_1999.1 (cuprite)"
	echo "				HyMap_1999.2 (australia)"
	echo "				HyMap_2000"
	echo "				HyMap_2001"
	echo "	                        HyMap_2002"
	echo "	                        HyMap_2007a"
	echo "				Hyperion:"
        echo "				Hyperion_2006"
	echo ""
	echo "				ASDFS"
        echo "				ASDFS"
	echo ""
	echo "image_cube is the path to the Imaging Spectrometer data set."
GOTO :eof


REM subroutine -------------------------------------------------------
REM subroutine to replace KEYWORDS with string assignment in text file
:str_replace

REM invoke str_replace (WinPC):  str_replace  KEYWORD str_replacement  file_in  file_out

rem for /f "tokens=1,* delims=]" %%A in ('"type %3|find /n /v """') do (
rem for /f "tokens=1,* delims=:" %%A in ('"type %3|findstr /n /v "no+~~~+find-this_string""') do (

set line-num_str=*:

rem use for to loop through all lines of text file (line delimited by non-existent `)
rem use findstr to read one line at a time from file (type %3)
rem parse line and echo looking for non-existent string
rem append line & colon to start of line string or empty lines

for /f "delims=`" %%A in ('"type %3|findstr /n /v "no+~~~+find-this_string""') do (
   set "line=%%A"
 rem substitute new substring for all originals
   call set "line=echo.%%line:%~1=%~2%%"
 rem remove line number colon, ie. 55:
   call set "line=echo.%%line:%line-num_str%=%%"
   for /f "delims=" %%X in ('"echo."%%line%%""') do set s=!s!%%~X>>%4
)

GOTO :eof


REM ------------------------------------------------------------------
REM subroutines data and restart (+/- lib) variable assignment set ---
REM ########## restart file name length max = 77 characters ##########

:aviris_2010
   set data=aviris_2010
   set restart=r1-av10a
GOTO :eof

:aviris_2006
   set data=aviris_2006
   set restart=r1-av06a
GOTO :eof

:aviris_2005
   set data=aviris_2005
   set restart=r1-av05a
GOTO :eof

:aviris_2001
   set data=aviris_2001
   set restart=r1-av01a
GOTO :eof

:aviris_2000
   set data=aviris_2000
   set restart=r1-av00a
GOTO :eof

:aviris_1999
   set data=aviris_1999
   set restart=r1-av99a
GOTO :eof

:aviris_1998
   set data=aviris_1998
   set restart=r1-av98a
GOTO :eof

:aviris_1997
   set data=aviris_1997
   set restart=r1-av97a
GOTO :eof

:aviris_1996
   set data=aviris_1996
   set restart=r1-av96a
GOTO :eof

:aviris_1995
   set data=aviris_1995
   set restart=r1-av95a
GOTO :eof

:aviris_1994
   set data=aviris_1994
   set restart=r1-av94a
GOTO :eof

:Sept_1993
   set data=aviris_Sept_1993
   set restart=r1-av9.93
GOTO :eof

:May_1993
   set data=aviris_May_1993
   set restart=r1-av5.93
GOTO :eof

:bodie_1992
   set data=aviris_bodie_1992
   set restart=r1-avbodie.92
GOTO :eof

:1992_no_shift
   set data=aviris_1992_no_shift
   set restart=r1-av1992
GOTO :eof

:MMM_09c
   set data=MMM_09c
   set lib=%libmm09c%
   set restart=r1-06mm09c
GOTO :eof

:VIMS2007
   set data=VIMS2007
   set restart=r1-vims2007
GOTO :eof

:FS358
   set data=FS358
   set restart=r1-fs358
GOTO :eof

:HyMap_1998
   set data=Hymap_1998
   set restart=r1-hymap98
GOTO :eof

:HyMap_1999.1
   set data=Hymap_1999.1
   set restart=r1-hymap99.1
GOTO :eof

:HyMap_1999.2
   set data=Hymap_1999.2
   set restart=r1-hymap99.2
GOTO :eof

:HyMap_2000
   set data=Hymap_2000
   set restart=r1-hymap00
GOTO :eof

:HyMap_2001
   set data=Hymap_2001
   set restart=r1-hymap01
GOTO :eof

:HyMap_2002
   set data=Hymap_2002
   set restart=r1-hymap02
GOTO :eof

:HyMap_2007a
   set data=Hymap_2007a
   set lib=%libhy07a%
   set restart=r1-s06hy07a
GOTO :eof

:Hyperion_2006
   set data=Hyperion_2006
   set restart=r1-hyperion06
GOTO :eof

:ASDFS
   set data=ASDFS
   set restart=r1-asdfs
GOTO :eof

REM END subroutines data and restart (+/- lib) variable assignment set
REM ------------------------------------------------------------------



REM batch end --------------------------------------------------------
:batch-end

echo .
echo .
echo "tetsetup Finished"
echo .
echo setup of %dir% complete
echo .
echo .
echo .
echo To run Tetracorder, cd to:   %dir%   and execute the command:   tetrun
echo .
echo .
echo .







rem exit

:eof