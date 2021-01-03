	subroutine getcmdargs
	implicit integer*4 (i-n)

#ccc  name: getcmdargs
#ccc  version date: 12/2/88
#ccc  author(s): Roger N. Clark
#ccc  language: Ratfor
#ccc
#ccc  short description: get the command line arguments and put them
#ccc			into /cmdarg/ common block
#ccc
#ccc  algorithm description: non hpux: use iargc, getarc sys calls,
#ccc                         hpux: use fortran "program" statement feature
#ccc  system requirements: unix: iargc, getarg
#ccc  subroutines called: unix: iargc, getarg
#ccc  argument list description: none
#ccc  parameter description: none
#ccc  common description: cmdarg
#ccc  message files referenced: none
#ccc  internal variables:
#ccc  file description: none
#ccc  user command lines: none
#ccc  update information:
#ccc  NOTES:
#ccc
#ccc  02/28/2002
#ccc      Modified to accommodate Fortran 90,etc on HPUX (IA64HPUX)

	include "../common/cmdarg"

	integer*4	iargc

	ncmdarg = 0

# make sure arrays are blank
#NONHPUX	charg1 = ' '
#NONHPUX	charg2 = ' '
#NONHPUX	charg3 = ' '

#SUNOS		charg1 = ' '
#SUNOS		charg2 = ' '
#SUNOS		charg3 = ' '

#SOLARIS	charg1 = ' '
#SOLARIS	charg2 = ' '
#SOLARIS	charg3 = ' '

#LINUX		charg1 = ' '
#LINUX		charg2 = ' '
#LINUX		charg3 = ' '

#IA64HPUX	charg1 = ' '
#IA64HPUX	charg2 = ' '
#IA64HPUX	charg3 = ' '

######### linux no dummy in iargc call:
#NONHPUX	ncmdarg = iargc(dummy)
#SUNOS		ncmdarg = iargc(dummy)
#SOLARIS	ncmdarg = iargc(dummy)
#LINUX		ncmdarg = iargc()
#IA64HPUX	ncmdarg = iargc()

#NONHPUX	if (ncmdarg >= 1) call getarg(1,charg1)
#NONHPUX	if (ncmdarg >= 2) call getarg(2,charg2)
#NONHPUX	if (ncmdarg >= 3) call getarg(3,charg3)

#SUNOS		if (ncmdarg >= 1) call getarg(1,charg1)
#SUNOS		if (ncmdarg >= 2) call getarg(2,charg2)
#SUNOS		if (ncmdarg >= 3) call getarg(3,charg3)

#SOLARIS	if (ncmdarg >= 1) call getarg(1,charg1)
#SOLARIS	if (ncmdarg >= 2) call getarg(2,charg2)
#SOLARIS	if (ncmdarg >= 3) call getarg(3,charg3)

#LINUX		if (ncmdarg >= 1) call getarg(1,charg1)
#LINUX		if (ncmdarg >= 2) call getarg(2,charg2)
#LINUX		if (ncmdarg >= 3) call getarg(3,charg3)

#IA64HPUX	if (ncmdarg >= 1) call getarg(1,charg1)
#IA64HPUX	if (ncmdarg >= 2) call getarg(2,charg2)
#IA64HPUX	if (ncmdarg >= 3) call getarg(3,charg3)

# For HPUX, the arguments are already installed into the arrays
#     from the program statement in the main routine.

# now make sure there are no embedded nulls

	do i = 1, 80 {
		if (charg1(i:i) == char(0)) charg1(i:i) = ' '
		if (charg2(i:i) == char(0)) charg2(i:i) = ' '
		if (charg3(i:i) == char(0)) charg3(i:i) = ' '
	}

# now find out how many arguments are non-blank and ncmdarg
#	if ncmdarg is zero (mainly for HPUX)
# note: commands must start with a non-blank

	if (ncmdarg == 0) {
		if (charg1(1:1) != ' ') ncmdarg = 1
		if (charg2(1:1) != ' ') ncmdarg = 3
		if (charg3(1:1) != ' ') ncmdarg = 3
	}

	return
	end
