	subroutine gwrite(line)
	
	implicit integer*4 (i-n)

	character*(*) line

	include "../common/hptrm"
	include "../common/xf"
	include "../common/lundefs"
#RED
	integer*4 lnb       # function lnb

#
# yagtos
# Yet another graphic text output subroutine.  This one was designed 
# to output write strings to the appropriate window under X.
#
# NOTE: Strings are limited to 80 characters, one line ONLY.
# anything else confuses the cursors..
# 

	if ((igrmod >= 50 && igrmod <= 53 ) && (xfwin == 1)) {
#XWIN		call xwrite(ixlast, iylast,line)
	} else {
		ie = lnb(line)
		if (ie < 1) ie = 1
		write(ttyout,10) line(1:ie)
	}
10	format(a)
	return
	end
