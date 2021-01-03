To compile Tetracorder:


compile Specpr, then Tetracorder  (Tetracorder uses Specpr Libraries)

1)  source the environmental variables file for desired system
2)  compile Specpr
    2a)  run make really_clean
    2b)  run make   (using the supplied makefile for LINUX, Windows, and/or CYGWIN)
3)  compile Tetracorder
    2a)  delete all old object files and tetracorder (or tetracorder.exe)
    2b)  run make

Note: Use Cygwin on a Windows-PC (here using ver. 1.7.9.1)
      to make Windows or Cygwin executables.
      the compiler will cross-link using the proper libraries.
---------------------------------------------------------
example:

cd /src/local/specpr/config
source ./cshenv.linux64.gfortran

cd /src/local/specpr/src.specpr
make really_clean
make

cd /src/local/tetracorder4.4
rm *.obj
rm tetracorder (or on Windows-PCs:  del tetracorder.exe)
make

---------------------------------------------------------