	subroutine getoverly(i,iov,iero)
	implicit none

# read  overlay spectra from specpr file and put in overlay
#       arrays

        include "../common/blank"
        include "../common/lbl7"
        include "../common/lbl6"
        include "../common/lbl4"
        include "../common/label1"
        include "../common/labl2"
        include "../common/lbl3"
        include "../common/label3"
        include "../common/labelf"
        include "../common/info"
        include "../common/lblg"
        include "../common/lundefs"
	include "../common/dscrch"
        include "../common/hptrm"
        include "../common/alphabet"
        include "../common/ioftyp"
        include "../common/overlys"

# the overlys common block has:
#       ovrflg  = -1 not defined  
#               = 0 do not overlay
#               = 1 overlay
#               = 2 overlay with autoscale 2% margin
#               = 3 overlay with autoscale to min max
#               = 3 overlay with autoscale to max, min stays as original

#       ovrflgb = stored setting of ovrflg when the overlay gets turned off
#                 when turned back on, ovrflg(n) = ovrflgb(n)

#       ovrclr  =     color (to be defined, future)

#       ovrchn  = channels in spectrum

#       ovtitle = titles
#       ovfil   = file ID letter for overlsy spectrum
#       ovrec   = record number for overlay
#       ovwfil  = file ID letter for wavelengths
#       ovwrec  = record number for overlay for wavelengths
#       ovops   = overlay options
 
#       ovrdat  = original data for overlay
#       ovrwav  = wavelengths
#       ovrdsc  = auto-scaled data

	integer*4 i, ii, iov, iero, j, ic2
	integer*4 ifilnu, ifilid, lun
	integer*4 idlt(4852), ipts, itmpch
	real*4    xfilb, x1, x

	iero=0   # error value

	#write (*,*) "DEBUG: getoverly pos1"

#       i = index in the iopcon command line array
#       iov = which array to put specturm into (1, 2, or 3)

	call wjfren(i,x1,ifilid)  # file ID letter
	call wjfren(i,xfilb,ic2)
	ifilnu=xfilb              # file record number

        if (ifilid==ihe || ic2==ihe) {
            iero = ihe
            return
        } else if (ifilid==ihx || ic2==ihx) {
            iero = ihx
            return

#        *** check for invalid input ***
        } else if (xfilb<=0.0) {
	    call what(i)
            write(ttyout,100)
100         format (' ERROR: ILLEGAL RECORD NUMBER FOUND. reenter line.',/)
	    return
	}

	if (ic2 != 0) i=i-1  # this character may be wavelength file ID


	#write (*,*) "DEBUG: getoverly pos2 called"

	if ( ifilnu == 0 || ifilid == 0) {
		iero =1
		write (*,*) "overlay: file ID or record number ERROR"
		write (ttyout,9924) "         file ID=", ifilid
		write (ttyout,9924) "         record#=", ifilnu
9924		format (' ', a, a)
		call crtin
		return
	}
	if ( iov > 0 ) {

		ovfil(iov)  = ifilid
		ovrec(iov)  = ifilnu
	}

	call devlun (0,ifilid,lun)
	call redfil (ifilnu, lun, iero)

	#write (*,*) "DEBUG: getoverly pos3 redfil called"

	if ( iov > 0 ) {

		ovrflg(iov) = 1
		ovrflgb(iov) = 1
		ovrchn(iov) = itchan
		ovtitle(iov)= ititl
		#write (*,*) "DEBUG: getoverly pos3a"
		do j = 1, 4852 {
			ovrdat(j,iov) = data(j)
		}
		#write (*,*) "DEBUG: getoverly pos3b"

	} 

# Now get wavelengths
# We don't want to write into the wavelength in use array, so we
# need to read the wavelengths as more data.

	#write (*,*) "DEBUG: getoverly pos4a calling filidw"

	#call filidw (i, ifilnu, ifilid)  # file record number and file ID letter

	call wjfren(i,x1,ifilid)  # file ID letter
	call wjfren(i,xfilb,ic2)
	ifilnu=xfilb              # file record number

        if (ifilid==ihe || ic2==ihe) {
            iero = ihe
            return
        } else if (ifilid==ihx || ic2==ihx) {
            iero = ihx
            return

#        *** check for invalid input ***
        } else if (xfilb<=0.0) {
	    call what(i)
            write(ttyout,101)
101         format (' ERROR: ILLEGAL RECORD NUMBER FOUND. reenter line.',/)
	    return
	}

	#write (*,*) "DEBUG: getoverly pos2 called"

	if ( ifilnu == 0 || ifilid == 0) {
		iero =1
		write (ttyout,*) "overlay: WAVELENGTH ID or record number ERROR"
		write (ttyout,9924) "         file ID=", ifilid
		write (ttyout,9924) "         record#=", ifilnu
		call crtin
		return
	}

#	write (ttyout,9923) ifilnu, ifilid
#9923    format (" DEBUG: filidw:", i6, a)

	#write (*,*) "DEBUG: getoverly pos4b filidw called"

	if ( ifilnu == 0 || ifilid == 0) {
		iero =1
		return
	}
	if ( iov > 0 ) {

		ovwfil(iov)  = ifilid
		ovwrec(iov)  = ifilnu
	}

	call devlun (0,ifilid,lun)
	call redfil (ifilnu, lun, iero)

	if ( iov > 0 ) {

		do j = 1, 4852 {
			ovrwav(j,iov) = data(j)
		}
	} 

# now check for autoscaling
# set ovrflg* to:
#               = 1 overlay with no autoscale (this is the default and was set above)
#               = 2 overlay with autoscale 2% margin
#               = 3 overlay with autoscale to min max
#               = 4 overlay with autoscale to max, min stays as original

	
	if (ic2 != ihca) {
		call wjfren(i,x,ic2)
	}

        if (ic2==ihe) {
            iero = ihe
            return
        } else if (ic2==ihx) {
            iero = ihx
            return
	}
	if (i >= 80) return

	ovops(iov) = 'N   '
	if (ic2 == ihca) {    # A = autoscale

		ovrflg(iov) = 3   # overlay with autoscale to min max
		ovrflgb(iov) = 3
		ovops(iov) = 'A   '
		if (iopcon(i:i) == "2") {   # overlay with autoscale 2% margin

			ovops(iov) = 'A2  '
			ovrflg(iov) = 2
			ovrflgb(iov) = 2
		}
		if (iopcon(i:i) == "0") {   # overlay with autoscale to max, min stays as original

			ovops(iov) = 'A0  '
			ovrflg(iov) = 4
			ovrflgb(iov) = 4
		}
		if (ovrflg(iov) == 2 || ovrflg(iov) == 4) {  # increment position
			i=i+1
		}
	} else if (ic2 == ihd) {  # delete points

		itmpch=ovrchn(iov)
		call dltpts(i, ipts, idlt, itmpch, ic2)
		write (*,*) "DEBUG: deleting ", ipts, "  overlay channels"
		write (*,*) "DEBUG:  i=", i
        	if (ic2==ihe) {
			iero = ihe
			return
		} else if (ic2==ihx) {
			iero = ihx
			return
		}
		if (ipts > 0 && iov > 0) {
			for(ii=1; ii<=ipts; ii=ii+1) {
				ovrdat(idlt(ii),iov) = -1.23e34   # deleted point
			}
		}
		write (*,*) "DEBUG: deleted ", ipts, "  overlay channels"
	}

	
	write (*,*) "DEBUG2:  i=", i
	call wjfren(i,x,ic2)

        if (ic2==ihe) {
            iero = ihe
            return
        } else if (ic2==ihx) {
            iero = ihx
            return
	}
	if (i >= 80) return

	write (*,*) "DEBUG3:  i=", i
	if (ic2 == ihd) {  # delete points

		write (*,*) "DEBUG: deleting overlay channels"
		itmpch=ovrchn(iov)
		call dltpts(i, ipts, idlt, itmpch, ic2)
		write (*,*) "DEBUG: deleting ", ipts, "  overlay channels"
        	if (ic2==ihe) {
			iero = ihe
		return
		} else if (ic2==ihx) {
			iero = ihx
			return
		}
		if (ipts > 0 && iov > 0) {
			for(ii=1;ii<=ipts;ii=ii+1) {
				ovrdat(idlt(ii),iov) = -1.23e34   # deleted point
			}
		}
		write (*,*) "DEBUG: deleted ", ipts, "  overlay channels"
	}

	#call crtin   # DEBUG
	return
	end
