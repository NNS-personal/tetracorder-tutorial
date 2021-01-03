	subroutine opencube (qlun, icube, filhdr, reclen, rechdr,
			dx, dy, dz, numtyp, filorg, ier)

	implicit integer*4 (i-n)

#ccc  name:  parsvicarlabel
#ccc  version date: 4/24/90
#ccc  author(s): Roger N. Clark
#ccc  language: ratfor
#ccc
#ccc  short description: open a cube check header and pase it.
#ccc                     at present only recognizes vicar
#ccc
#ccc  algorithm description: search strings label for
#ccc                         various fields to recognize correct
#ccc				format, then call parsing routine.
#ccc  system requirements: none
#ccc  subroutines called:       wjfren
#ccc  argument list description:
#ccc     INPUT:
#ccc        qlun:   3D logical unit number
#ccc        icube:  cube file name
#ccc     OUTPUT:
#ccc        filhdr: file header length in recorde
#ccc        reclen: record length in bytes
#ccc        rechdr: record header length in bytes
#ccc
#ccc        dx: x dimension in pixels = number of samples
#ccc        dy: y dimension in pixels = number of lines
#ccc        dz: z dimension in pixels = number of bands
#ccc
#ccc        numtyp: Data type 1= int*2
#ccc        filorg: file organization (1 = BIL)
#ccc
#ccc  parameter description:
#ccc  common description:
#ccc  message files referenced: none
#ccc  internal variables:
#ccc  file description: none
#ccc  user command lines: none
#ccc  update information:
#ccc  NOTES:
#ccc

	integer*4 qlun, reclen, rechdr, idnoff, filhdr
	integer*4 dx, dy, dz, numtyp, filorg, ier

	character*1536 ch
	character*(*) icube

	character*120 ivtitl,avlab1
	integer*4 lblsiz,form,recsiz
	integer*4 ivdate,ivtime,ivdat2,ivtim2

	character*1 ichil,iquote,lowch,highch,cnull
	character*3 cm

	integer*4 ttyout
	integer*4 lnb, fnb
	logical*4	fexist     # file exists: true, false if doesn't
	logical*4	iopened    # file already opened: true, false if doesn't
#
###### TEMPROARY:
	ttyout = 6
#
# initialize variables
#
	iquote = char(39)  # a single quote character

# open the cube

	itmp1 = fnb(icube)
	itmp2 = lnb(icube)
	if (itmp1 == 0 | itmp2 == 0 | itmp2 < itmp1) {
		write (ttyout,*) icube
		call what(itmp1)
		call what(itmp2)
		return
	}

	write (ttyout,*)'Opening:',icube(itmp1:itmp2)

	inquire (file=icube(itmp1:itmp2),exist=fexist,
		opened=iopened,number=inum,iostat=ier)

	if (ier != 0) {
		write (ttyout,*) 'INQUIRE ERROR ',ier
		write (ttyout,*) 'FILE:',icube(itmp1:itmp2)
		call what (-1)
		return
	}

	if (!fexist) {
		write (ttyout,187) icube(itmp1:itmp2)
187		format (' ERROR: file:',a,/,
			' FILE DOES NOT EXIST')
		call what (-1)
		return
	}

	if (iopened) {
		write (ttyout,*) 'FILE:',icube(itmp1:itmp2)
		write (ttyout,*) 'ALREADY OPENED to unit',inum
		if (inum < 20) {
			write (ttyout,*) 'IT IS PROBABLY ASSIGNED IN SPECPR'
		}
		call what (-1)
		return
	}

	open (unit=qlun, file=icube(itmp1:itmp2),
		access='direct',
                recl=1536,
                form='unformatted', status='old',
		iostat=ier)
	if (ier != 0) {
		write (ttyout,*)'ERROR on initial open in opencube: ',ier
		write (ttyout,*)'unit=',qlun
		call what(-1)
		return
	}
#
# determine if header is recognizable
#
	read (qlun,rec=1,iostat=ier) ch
	if (ier != 0) {
		write (ttyout,*)'ERROR on read in opencube: ',ier
		call what(-1)
		return
	}
	if (ch(1:8) == 'LBLSIZE=') {  # found basic vicar label

		call parsvicarlabel (ch,lblsiz,form,recsiz,
			filorg,dy,dx,dz,ivdate,ivtime,ivdat2,
			ivtim2,ivtitl,avlab1,ier)
		rechdr = 0
		if (form == 16) {    # 16-bit int values
			numtyp = 1
			reclen = dx * 2
		} else {
			numtyp = 0 # temporary
			write (ttyout,*) 'can only do 16-bit files ',
				'at present'
			ier = 0
			call what(-1)
			return
		}

		## was ## filhdr = lblsiz / reclen
                ## now:

                filhdr = int(float(lblsiz)/float(reclen)+0.9999)

		if (filhdr*reclen != lblsiz) {
			write (ttyout,*) 'ERROR: filhdr*reclen=',
				filhdr*reclen,' is not =',lblsiz,
				'filhdr=',filhdr,'reclen=',reclen
			write (ttyout,*) 'header size must be a',
				' multiple of the record size'
			write (ttyout,*) 'because this is fortran I/O'
			ier =1
			call what(-1)
			return
		}

		close (qlun, iostat=ier)
		if (ier != 0) {
			write (ttyout,*)'ERROR on close ',
						'in opencube: ',ier
			call what(-1)
			return
		}
		open (unit=qlun, file=icube,
			access='direct',
			recl=reclen,
			form='unformatted', status='old',
			iostat=ier)
		if (ier != 0) {
			write (ttyout,*)'ERROR on second',
					' open in opencube: ',ier
			call what(-1)
			return
		}
	} else {

		write (ttyout,*) 'unrecognized file header'
		call what(-1)
		ier = 0
		return
	}

	return
	end
