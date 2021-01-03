	subroutine savdta(pltnum,nam,datoff,datmul,xoffset)
	implicit integer*4 (i-n)
#ccc  name: savdta
#ccc  version date: %W% %G% %U%
#ccc  author(s): J.A.Hoover
#ccc  language: RATFOR
#ccc
#ccc  short description: 
#ccc			this routine reads the wavelengths,data,and errors
#ccc            and writes them to the plotting scratch file.
#ccc
#ccc  algorithm description:
#ccc  system requirements:
#ccc  subroutines called: finfil,wavlng
#ccc  argument list description:
#ccc  parameter description:
#ccc  common description:
#ccc  message files referenced:
#ccc  internal variables:
#ccc  file description:
#ccc  user command lines:
#ccc  update information:
#ccc  NOTES:
#ccc

	include "../common/pltcnt"
	include "../common/blank"
	include "../common/lblg"
	include "../common/lundefs"
	include "../common/label1"

	integer*4	pltnum
	real*4 datoff     # data offset
        real*4 datmul     # data multiplier
        real*4 xoffset    # x-axis offset offset


	i = recno
	iefil = i
	call finfil(i,filid,3,ier)
	write(ttyout,100) pltnum,ititle,nam,recno, nchans
	if (errbar != 0) {
#
#  do redfil first to position record number (iefil) in case of
#   multiple segment records (continuation records.
#
		call redfil(iefil,filid,ier)
		iefil = iefil +1
		call finfil(iefil,filid,2,ier)
	}

	call wavlng(wavid,wavrec,ier)
	chans = nchans
	wvmn = 1.0e34
	wvmx = -1.0e34
	do i=1,nchans {
		if (dataa(i)==-1.23e34) next
		if (dataa(i)<wvmn) wvmn = dataa(i)
		if (dataa(i)>wvmx) wvmx = dataa(i)
	}

	do i=1,nchans {
		if (gdel(i)>0 && gdel(i)<=nchans) dataa(gdel(i)) = -1.23e34
		if (ldel(i)>0 && ldel(i)<=nchans) dataa(ldel(i)) = -1.23e34
	}
	if (abs(datoff) > 0.1e-34 | abs(datmul-1.0) > 0.1e-34) {    # offset and multiply the data (in datac)
		do i = 1, nchans {
			if (dataa(i) == -1.23e34 ||
				datac(i) == -1.23e34) next
			datac(i) = datac(i)*datmul + datoff
		}
		write (ttyout, 110) datmul, datoff
	}
	if (abs(xoffset) > 0.1e-34 ) {    # offset and multiply the data (in datac)
		do i = 1, nchans {
			if (dataa(i) == -1.23e34 ) next
			dataa(i) = dataa(i) + xoffset
		}
		write (ttyout, 120) xoffset
	}
	if (ilog) {  # convert to log
			# added 8/15/2008, do not know why it was not here before.  RNC

		do i=1,nchans {
			if (datac(i) < 0.1e-33) {
				datac(i) = -1.23e34
			} else {

				datac(i) = alog10(datac(i))
			}

		}
	}
	write(pltlun,rec=nplots) dataa,datab,datac,opt
	return
100	format(i5,': ',a,'  file ',a1,i5, 'chans=', i5)
110	format (29x, 'Data multiplied by',f14.5,' and offset by',f14.5)
120	format (29x, 'x-axis offset by',f14.5)
	end
