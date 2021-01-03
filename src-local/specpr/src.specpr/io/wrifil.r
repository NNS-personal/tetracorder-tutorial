	subroutine wrifil (ifiln, idev, ier)
	implicit integer*4 (i-n)

#ccc  version date: 06/01/83
#ccc  author(s): Roger Clark & Jeff Hoover
#ccc  language:  Ratfor
#ccc
#ccc  short description:
#ccc         This subroutine writes the file
#ccc  algorithm description: none
#ccc  system requirements:   none
#ccc  subroutines called:
#ccc         devsta,crtin,posfil,ertyp,x2ti,drite,newsta
#ccc  argument list description:
#ccc         arguments: ifiln,idev,ier
#ccc  parameter description:
#ccc  common description:
#ccc  message files referenced:
#ccc  internal variables:
#ccc  file description:
#ccc  user command lines:
#ccc  update information:
#ccc  NOTES:
#ccc
#################################################################
#                                                               #
#       write file routine                                      #
#                                                               #
#       arguments                                               #
#         ifiln         record number to write                  #
#         idev          logical unit to write to                #
#         ier           error flag to calling routine           #
#                                                               #
#################################################################

	include "../common/label1"
	include "../common/lblprt"
	include "../common/lundefs"
	include "../common/lblg"

	integer*4 wrttap
	integer*4 ier
	logical tape

	integer*4 input(384)
	equivalence (input,icflag)

	real*4 codata(383)
	character*1532 tdata
	equivalence (codata, iobuff(2))
	equivalence (tdata, iobuff(2))

	integer*2 chkbit, ibit

	integer*4	revbytes, isav
	
# revbytes =1 reverses bytes on the binary data.  By default it is zero
# byte order is that on HP, Sun sparc machines.
#  - Roger N. Clark 2/18/2001

		revbytes = 0
#LINUX		revbytes = 1

	
	ibit = 1
	if (chkbit(icflag,ibit) == 0) {
		call pad(ititl)
		call pad(ihist)
		call pad(mhist)

#		for (i=1; i<=3; i=i+1) {
#			call pad(cta(i))
#			call pad(ctb(i))
#			call pad(sta(i))
#			call pad(stb(i))
#			call pad(datea(i))
#			call pad(dateb(i))
#		}
	}

# convert times to seconds, dates to julian day, ra, dec (long, lat)
#         to seconds

# code to be written

	ier= 0
	call devsta (idev,ista,0,iprt)
	if (ista.eq.-4) {
		ier= 4
		return
	}
	if (ifiln<1) ifiln=1
	if (ifiln>maxrec) {
		write(ttyout, 11) ifiln, maxrec
11		format (' ERROR: Record Request', i5,
			' GREATER THAN MAX:', i6, /)
		ifiln=1
		call crtin
		ier=4
		return
	}
	if (iprt<-1 | (iprt!=-1 & ifiln!=iprt+1)) {
		write(ttyout,16) ifiln, iprt
16		format (' File WRITE PROTECTION ERROR: record=',i5,
			'Protection=',i5,/)
		ier=4
		return
	}
	call posfil (idev, ifiln, tape, ier)
	if (ier!=0) {
		call ertyp('posfil',idev,ier)
		return
	}
	ibit = 1
	if (chkbit(icflag,ibit) == 0) {
		filno=ifiln
	}

# load io buffer

	do i = 1, 384
		iobuff(i) = input(i)

# clear continuation bit

	ibit = 0
	call clrbit (iobuff(1),ibit)

# write buffer

	if (tape) {
		key=ifiln-1
		call x2ti
		ier = wrttap(idev,iobuff)
	} else {
		key=ifiln

		isav=iobuff(1)        # save bitflags
		if (revbytes == 1) {
			call bytorder (iobuff,2)
		}
		write(idev,rec=key+1,iostat=ier)iobuff
		iobuff(1)=isav        # restore iobuff so bit flags can be tested
	}

# check for io error

	if (ier!=0) {
		call ertyp('wrifil',idev,ier)
		ier=4
		return
	}

	call newsta (idev, 11, ifiln)

#     update protection

	ier = 2
	if (iprt>=0) {

		if (idev==3) iprtu= iprtu+1
		if (idev==4) iprty= iprty+1
		if (idev==7) iprtd= iprtd+1
		if (idev==8) iprtv= iprtv+1
		if (idev==9) iprtw= iprtw+1
	}


# if itchan > 256 write additional segments  (if data)
# if itxtch > 1476 write additional segments (if text)

	ibit = 1
	if (chkbit(icflag,ibit) == 0) {
		if (itchan > maxchn) itchan = maxchn
		if (itchan < 0) itchan = 0
		irect = int((float(itchan) - 256.0)/383.0 +0.999)
	} else {
		if (itxtch > maxtxt) itxt = maxtxt
		if (itxtch < 0) itxtch = 0
		irect = int((float(itxtch) - 1476.0)/1532.0 + 0.999)
	}

	if (irect >= 1) {
		do j = 1, irect {

			ifiln = ifiln +1

# load next segment of data into io buffer

			ibit = 1
			if (chkbit(icflag,ibit) == 0) {    # values = floating poitn data
				isegm = 256 + (j-1)*383
				do m = 1, 383
					codata(m) = data(isegm +m)
			} else {                           # ascii text
				isegm = 1476 + (j-1)*1532
				tdata = itext(isegm:isegm+1532)
			}

# set continuation bit

			ibit = 0
			call setbit(iobuff(1), ibit)

			ier= 0
			call devsta (idev,ista,0,iprt)
			if (ista.eq.-4) {
				ier= 4
				return
			}
			if (ifiln>maxrec) {
				write(ttyout, 11) ifiln, maxrec
				ifiln=1
				call crtin
				ier=4
				return
			}
			if (iprt<-1 | (iprt!=-1 & ifiln!=iprt+1)) {
				write(ttyout,16) ifiln, iprt
				ier=4
				return
			}
			call posfil (idev, ifiln, tape, ier)
			if (ier!=0) {
				call ertyp('posfil',idev,ier)
				return
			}
			filno=ifiln

# write buffer

			if (tape) {
				key=ifiln-1
				call x2ti
				ier = wrttap(idev,iobuff)
			} else {
# debug:
	jseg = j+1
#debug:	write (ttyout,401)ifiln,jseg
401	format(1x,'writing record',i6,' segment=',i4)

				key=ifiln

				isav=iobuff(1)        # save bitflags
				if (revbytes == 1) {
					call bytorder (iobuff,2)
				}

				write(idev,rec=key+1,iostat=ier)iobuff

				iobuff(1)=isav        # restore iobuff so bit flags can be tested

			}
# check for io error
			if (ier!=0) {
				call ertyp('wrifil',idev,ier)
				ier=4
				return
			}
		
			call newsta (idev, 11, ifiln)
		
#     update protection
			ier = 2
			if (iprt>=0) {
				if (idev==3) iprtu= iprtu+1
				if (idev==4) iprty= iprty+1
				if (idev==7) iprtd= iprtd+1
				if (idev==8) iprtv= iprtv+1
				if (idev==9) iprtw= iprtw+1
			}
		}
	}

#**********
	return
	end
