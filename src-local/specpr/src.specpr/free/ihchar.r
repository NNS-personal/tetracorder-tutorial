	integer*4 function ihchar(c)
	implicit integer*4 (i-n)
	character*1 c

	icharb=ichar(' ')
#ccc

# byte swapped machines:

#DEC            ihchar=((256*icharb + icharb)*256 + icharb)*256 +ichar(c)
#LINUX		ihchar=((256*icharb + icharb)*256 + icharb)*256 +ichar(c)
#INTEL          ihchar=((256*icharb + icharb)*256 + icharb)*256 +ichar(c)

# IEEE byte order machinez:

#HPUX           ihchar=((256*ichar(c) + icharb)*256 + icharb)*256 +icharb
#IA64HPUX       ihchar=((256*ichar(c) + icharb)*256 + icharb)*256 +icharb
#SUNOS          ihchar=((256*ichar(c) + icharb)*256 + icharb)*256 +icharb
#SOLARIS        ihchar=((256*ichar(c) + icharb)*256 + icharb)*256 +icharb
#

                return
                end
