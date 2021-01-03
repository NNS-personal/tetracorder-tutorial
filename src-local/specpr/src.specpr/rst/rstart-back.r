      subroutine rstart (ic)
# *********************************************************
#     SPECPR Version 2 restart ASCII read subroutine
#
#
#     this subroutine sets up the specpr files and protections for
#     restarting the program at the same condition as at the time of
#     the rstart call or it also stores the parameters for a future
#     restart.
#
#      ic= 1: store current parameters for a future restart.
#      ic= 2: restart= recall the parameters and assign and open all
#                      the files and devices.
#      ic= 3: recall the parameters but don't assign and open any files
#      ic= 4: open rlun (unit 18) only. (opens restart file)
#
#      modified to SPECPR Version 2 - Summer 2009  (K.E. Livo)
#
# *********************************************************

      implicit integer*4 (i-n)
      integer*4 ic

      include "../common/hptrm"
      include "../common/lundefs"
      include "../common/filenames"
      include "../common/ioftyp"
      include "../common/cmdarg"

      include "../common/lblvol"
      include "../common/lblprt"
      include "../common/label3"
      include "../common/labelf"
      include "../common/lbl4"
	include "../common/lbl7"
	include "../common/lblg"
	include "../common/info"
	include "../common/cmd"

#  declare functions ifnb & lnb
      integer*4 i, ifstchr, ifnb, lnb, icmdmax
      integer*4 ier, idummy, itrip

      integer*4 il, inn, irecl, itmp
      real*4 x

#   local variables
      character*120 iopcon2
      character*8 lnerr
	character*8 rstartver
	character*1 ctmp

#  set max chars input (cmdmax)
#  change icmdmax and iopcon2 for larger input string
	icmdmax = 80
      itrip = 0



      if (ic==1) {
#        store current parameters for a future restart
#        version 2 - specpr ascii restart

#  set SPECPR Restart File Version No. to "2.00"; this place ONLY
      rstartver="2.00"

      write (ttyout,51)
51    format(' Updating Restart File ')

      rewind(rlun)
      write(rlun,'(A8,"  # SPECPR Restart File Version No.")',err=4321) rstartver
      write(rlun,'("#")')

#     lblvol.h
#     8 - 80 character filenames
      write(rlun,'("# file names")')
      write(rlun,'("#")')

      write(rlun,'("ivfl=",A80)',err=4321) ivfl
      write(rlun,'("iwfl=",A80)',err=4321) iwfl
      write(rlun,'("idfl=",A80)',err=4321) idfl
      write(rlun,'("iufl=",A80)',err=4321) iufl
      write(rlun,'("iyfl=",A80)',err=4321) iyfl
      write(rlun,'("isfl=",A80)',err=4321) isfl
      write(rlun,'("ilfl=",A80)',err=4321) ilfl
      write(rlun,'("irfl=",A80)',err=4321) irfl

#     lblprt.h
#     6 - protection # for open files (u,y,d,v,w,s)
      write(rlun,'("#")')
#
      write(rlun,'("# protection number for open files (v,w,d,u,y,s)")')
      write(rlun,'("#")')
      write(rlun,'("iprtv=",I14,"  # device protection v")',err=4321) iprtv
      write(rlun,'("iprtw=",I14,"  # device protection w")',err=4321) iprtw
      write(rlun,'("iprtd=",I14,"  # device protection d")',err=4321) iprtd
      write(rlun,'("iprtu=",I14,"  # device protection u")',err=4321) iprtu
      write(rlun,'("iprty=",I14,"  # device protection y")',err=4321) iprty
      write(rlun,'("iprts=",I14,"  # device protection s")',err=4321) iprts

#     lbl4.h
#     short 8 character names associated with file device letters
      write(rlun,'("#")')
#
      write(rlun,'("# short 8 character names associated with file device letters")')
      write(rlun,'("#")')
      write(rlun,'("isavt=      ",A8,"  # file device letter v")',err=4321) isavt
      write(rlun,'("iwdgt=      ",A8,"  # file device letter w")',err=4321) iwdgt
      write(rlun,'("iwrkt=      ",A8,"  # file device letter d")',err=4321) iwrkt
      write(rlun,'("inmu=       ",A8,"  # file device letter u")',err=4321) inmu
      write(rlun,'("inmy=       ",A8,"  # file device letter y")',err=4321) inmy
#
#     lbl4.h continued
#     graphics plot control values
      write(rlun,'("#")')
#
      write(rlun,'("# plot control values (real number format: ex. 0.23E+01)")')
      write(rlun,'("#")')
      write(rlun,'("wmina=",E14.6,"  # plot min wavelength")',err=4321) wmina
      write(rlun,'("wmaxa=",E14.6,"  # plot max wavelength")',err=4321) wmaxa
      write(rlun,'("bbnd= ",E14.6,"  # plot min reflectance")',err=4321) bbnd
      write(rlun,'("ubnd= ",E14.6,"  # plot max reflectance")',err=4321) ubnd
      write(rlun,'("#",A1)')
      write(rlun,'("ipc(1)=   ",I10,"  #")',err=4321) ipc(1)
      write(rlun,'("ipc(2)=   ",I10,"  #")',err=4321) ipc(2)
      write(rlun,'("ipc(3)=   ",I10,"  #")',err=4321) ipc(3)
      write(rlun,'("ipc(4)=   ",I10,"  #")',err=4321) ipc(4)
      write(rlun,'("ipc(5)=   ",I10,"  #")',err=4321) ipc(5)
      write(rlun,'("ipc(6)=   ",I10,"  #")',err=4321) ipc(6)
      write(rlun,'("ipc(7)=   ",I10,"  #")',err=4321) ipc(7)
      write(rlun,'("ipc(8)=   ",I10,"  #")',err=4321) ipc(8)
      write(rlun,'("ipc(9)=   ",I10,"  #")',err=4321) ipc(9)
      write(rlun,'("ipc(10)=  ",I10,"  #")',err=4321) ipc(10)
      write(rlun,'("istarp(1)=",I10,"  #")',err=4321) istarp(1)
      write(rlun,'("istarp(2)=",I10,"  #")',err=4321) istarp(2)
# NOTE: LINUX (byteorder)only
      ctmp = char( mod(iauto,256) )
      write(rlun,'("iauto=    ",A10,"  # wavelength file id")',err=4321) ctmp




#     label3.h
#     6 - observatory variables:
#     (latitude,right ascension, declination,hour angle, air mass,
#      wavelength calibration shift in angstroms)
      write(rlun,'("#")')
#
      write(rlun,'("alat=   ",F12.7,"  # observatory lat in rads")',err=4321) alat
      write(rlun,'("ra=     ",F12.7,"  # right ascension in radians")',err=4321) ra
      write(rlun,'("dec=    ",F12.7,"  # declination in radians")',err=4321) dec
      write(rlun,'("ha=     ",F12.7,"  # hour angle in radians")',err=4321) ha
      write(rlun,'("airmas= ",F12.6,"  # air mass of object")',err=4321) airmas
      write(rlun,'("iwch=   ",I12,"  # wavelength calib shift")',err=4321) iwch

#     labelf.h
#     11 - file pointers and status
      write(rlun,'("#")')
      write(rlun,'("# record pointers",A17)')
      write(rlun,'("#")')
#
      write(rlun,'("mag0=     ",I10,"  # mag tape drive 0")',err=4321) mag0
      write(rlun,'("mag1=     ",I10,"  # mag tape drive 1")',err=4321) mag1
      write(rlun,'("isavf=    ",I10,"  # file v")',err=4321) isavf
      write(rlun,'("iwjf=     ",I10,"  # file w")',err=4321) iwjf
      write(rlun,'("iwrkf=    ",I10,"  # file d")',err=4321) iwrkf
      write(rlun,'("isvcu=    ",I10,"  # file u")',err=4321) isvcu
      write(rlun,'("iwjcy=    ",I10,"  # file y")',err=4321) iwjcy
      write(rlun,'("istrf=    ",I10,"  # file s")',err=4321) istrf
      write(rlun,'("ilpt=     ",I10,"  # line printer")',err=4321) ilpt
      write(rlun,'("icdr=     ",I10,"  # card reader")',err=4321) icdr
      write(rlun,'("ipp=      ",I10,"  # printer/plotter")',err=4321) ipp

#     lblg.h
#     spectrum in use specifics
      write(rlun,'("#")')
      write(rlun,'("nchans=   ",I10,"  # num wave chans")',err=4321) nchans
      write(rlun,'("ibnrm1=   ",I10,"  # band anal")',err=4321) ibnrm1
      write(rlun,'("ibnrm2=   ",I10,"  # band anal")',err=4321) ibnrm2
      write(rlun,'("nchsav=   ",I10,"  # num chan save")',err=4321) nchsav
      write(rlun,'("iline=    ",I10,"  # graphics line type")',err=4321) iline

#     info.h
#     info print on crt control flags
      write(rlun,'("#")')
      write(rlun,'("infmth=   ",I10,"  #")',err=4321) infmth
      write(rlun,'("infopr=   ",I10,"  #")',err=4321) infopr
      write(rlun,'("inftrn=   ",I10,"  #")',err=4321) inftrn
      write(rlun,'("iother(1)=",I10,"  #")',err=4321) iother(1)
      write(rlun,'("iother(2)=",I10,"  #")',err=4321) iother(2)
      write(rlun,'("iother(3)=",I10,"  #")',err=4321) iother(3)
      write(rlun,'("iother(4)=",I10,"  #")',err=4321) iother(4)
      write(rlun,'("iother(5)=",I10,"  #")',err=4321) iother(5)
      write(rlun,'("iother(6)=",I10,"  #")',err=4321) iother(6)
      write(rlun,'("iother(7)=",I10,"  #")',err=4321) iother(7)
      write(rlun,'("iother(8)=",I10,"  #")',err=4321) iother(8)
      write(rlun,'("iother(9)=",I10,"  #")',err=4321) iother(9)

#     cmd.h
      write(rlun,'("#")')
      write(rlun,'("cfile=",A40,"  #")',err=4321) cfile

#     hptrm.h
      write(rlun,'("#")')
      write(rlun,'("igrmod=   ",I10,"  # graphics mode")',err=4321) igrmod

#     lbl7.h
#     variables lbl(23) - lbl(25): itrol(1) - itrol(3)
#     wavelength file id, record in use, and
#     chan/wave/energy plot flag
      write(rlun,'("#")')
# NOTE: LINUX (byteorder)only
      ctmp = char( mod(itrol(1),256) )
      write(rlun,'("itrol(1)= ",A10,"  # wavelength file id")',err=4321) ctmp
      write(rlun,'("itrol(2)= ",I10,"  # record in use")',err=4321) itrol(2)
# NOTE: LINUX (byteorder)only
      ctmp = char( mod(itrol(3),256) )
      write(rlun,'("itrol(3)= ",A10,"  # chan/wave/energy plot flag")',err=4321) ctmp

#     ioftyp.h
#     3D file reading routine
      write(rlun,'("#")')
      write(rlun,'("# parameters for 3D file I/O",A28)')
      write(rlun,'("#")')
      write(rlun,'("filtyp(1,1)= ",I10,"  # specpr file flag")',err=4321) filtyp(1,1)
      write(rlun,'("filtyp(2,1)= ",I10,"  # file header lgth")',err=4321) filtyp(2,1)
      write(rlun,'("filtyp(3,1)= ",I10,"  # record length")',err=4321) filtyp(3,1)
      write(rlun,'("filtyp(4,1)= ",I10,"  # record hdr lgth")',err=4321) filtyp(4,1)
      write(rlun,'("filtyp(5,1)= ",I10,"  # DN offset")',err=4321) filtyp(5,1)
      write(rlun,'("filtyp(6,1)= ",I10,"  # x - dimension")',err=4321) filtyp(6,1)
      write(rlun,'("filtyp(7,1)= ",I10,"  # y - dimension")',err=4321) filtyp(7,1)
      write(rlun,'("filtyp(8,1)= ",I10,"  # z - dimension")',err=4321) filtyp(8,1)
      write(rlun,'("filtyp(9,1)= ",I10,"  # data type")',err=4321) filtyp(9,1)
      write(rlun,'("filtyp(10,1)=",I10,"  # file order")',err=4321) filtyp(10,1)
      write(rlun,'("filtyp(11,1)=",I10,"  # point deletion")',err=4321) filtyp(11,1)
      write(rlun,'("filtyp(12,1)=",I10,"  # blank")',err=4321) filtyp(12,1)
#
      write(rlun,'("filtyp(1,2)= ",I10,"  # specpr file flag")',err=4321) filtyp(1,2)
      write(rlun,'("filtyp(2,2)= ",I10,"  # file header lgth")',err=4321) filtyp(2,2)
      write(rlun,'("filtyp(3,2)= ",I10,"  # record length")',err=4321) filtyp(3,2)
      write(rlun,'("filtyp(4,2)= ",I10,"  # record hdr lgth")',err=4321) filtyp(4,2)
      write(rlun,'("filtyp(5,2)= ",I10,"  # DN offset")',err=4321) filtyp(5,2)
      write(rlun,'("filtyp(6,2)= ",I10,"  # x - dimension")',err=4321) filtyp(6,2)
      write(rlun,'("filtyp(7,2)= ",I10,"  # y - dimension")',err=4321) filtyp(7,2)
      write(rlun,'("filtyp(8,2)= ",I10,"  # z - dimension")',err=4321) filtyp(8,2)
      write(rlun,'("filtyp(9,2)= ",I10,"  # data type")',err=4321) filtyp(9,2)
      write(rlun,'("filtyp(10,2)=",I10,"  # file order")',err=4321) filtyp(10,2)
      write(rlun,'("filtyp(11,2)=",I10,"  # point deletion")',err=4321) filtyp(11,2)
      write(rlun,'("filtyp(12,2)=",I10,"  # blank")',err=4321) filtyp(12,2)
#
      write(rlun,'("filtyp(1,3)= ",I10,"  # specpr file flag")',err=4321) filtyp(1,3)
      write(rlun,'("filtyp(2,3)= ",I10,"  # file header lgth")',err=4321) filtyp(2,3)
      write(rlun,'("filtyp(3,3)= ",I10,"  # record length")',err=4321) filtyp(3,3)
      write(rlun,'("filtyp(4,3)= ",I10,"  # record hdr lgth")',err=4321) filtyp(4,3)
      write(rlun,'("filtyp(5,3)= ",I10,"  # DN offset")',err=4321) filtyp(5,3)
      write(rlun,'("filtyp(6,3)= ",I10,"  # x - dimension")',err=4321) filtyp(6,3)
      write(rlun,'("filtyp(7,3)= ",I10,"  # y - dimension")',err=4321) filtyp(7,3)
      write(rlun,'("filtyp(8,3)= ",I10,"  # z - dimension")',err=4321) filtyp(8,3)
      write(rlun,'("filtyp(9,3)= ",I10,"  # data type")',err=4321) filtyp(9,3)
      write(rlun,'("filtyp(10,3)=",I10,"  # file order")',err=4321) filtyp(10,3)
      write(rlun,'("filtyp(11,3)=",I10,"  # point deletion")',err=4321) filtyp(11,3)
      write(rlun,'("filtyp(12,3)=",I10,"  # blank")',err=4321) filtyp(12,3)
#
      write(rlun,'("filtyp(1,4)= ",I10,"  # specpr file flag")',err=4321) filtyp(1,4)
      write(rlun,'("filtyp(2,4)= ",I10,"  # file header lgth")',err=4321) filtyp(2,4)
      write(rlun,'("filtyp(3,4)= ",I10,"  # record length")',err=4321) filtyp(3,4)
      write(rlun,'("filtyp(4,4)= ",I10,"  # record hdr lgth")',err=4321) filtyp(4,4)
      write(rlun,'("filtyp(5,4)= ",I10,"  # DN offset")',err=4321) filtyp(5,4)
      write(rlun,'("filtyp(6,4)= ",I10,"  # x - dimension")',err=4321) filtyp(6,4)
      write(rlun,'("filtyp(7,4)= ",I10,"  # y - dimension")',err=4321) filtyp(7,4)
      write(rlun,'("filtyp(8,4)= ",I10,"  # z - dimension")',err=4321) filtyp(8,4)
      write(rlun,'("filtyp(9,4)= ",I10,"  # data type")',err=4321) filtyp(9,4)
      write(rlun,'("filtyp(10,4)=",I10,"  # file order")',err=4321) filtyp(10,4)
      write(rlun,'("filtyp(11,4)=",I10,"  # point deletion")',err=4321) filtyp(11,4)
      write(rlun,'("filtyp(12,4)=",I10,"  # blank")',err=4321) filtyp(12,4)
#
      write(rlun,'("filtyp(1,5)= ",I10,"  # specpr file flag")',err=4321) filtyp(1,5)
      write(rlun,'("filtyp(2,5)= ",I10,"  # file header lgth")',err=4321) filtyp(2,5)
      write(rlun,'("filtyp(3,5)= ",I10,"  # record length")',err=4321) filtyp(3,5)
      write(rlun,'("filtyp(4,5)= ",I10,"  # record hdr lgth")',err=4321) filtyp(4,5)
      write(rlun,'("filtyp(5,5)= ",I10,"  # DN offset")',err=4321) filtyp(5,5)
      write(rlun,'("filtyp(6,5)= ",I10,"  # x - dimension")',err=4321) filtyp(6,5)
      write(rlun,'("filtyp(7,5)= ",I10,"  # y - dimension")',err=4321) filtyp(7,5)
      write(rlun,'("filtyp(8,5)= ",I10,"  # z - dimension")',err=4321) filtyp(8,5)
      write(rlun,'("filtyp(9,5)= ",I10,"  # data type")',err=4321) filtyp(9,5)
      write(rlun,'("filtyp(10,5)=",I10,"  # file order")',err=4321) filtyp(10,5)
      write(rlun,'("filtyp(11,5)=",I10,"  # point deletion")',err=4321) filtyp(11,5)
      write(rlun,'("filtyp(12,5)=",I10,"  # blank")',err=4321) filtyp(12,5)
#
      return
#
#----------------------------------------------------------------------
#     error handling routines
#
4321  write(ttyout,4322) ier
4322  format ( 'write error on restart file, error=', i5, /, 
               ' Terminating specpr')
      stop

      } else {

#
#----------------------------------------------------------------------
#        ic != 1: recall the parameters (restart ascii version 2)
#                 (Continue when ic = 2, ic = 3, or ic = 4)
#                 Do a Restart
#

#        just open restart file only when ic=4
         if (ic == 4) goto 33

#        initialize rstart variables

         ivfl = ""
         iwfl = ""
         idfl = ""
         iufl = ""
         iyfl = ""
         isfl = ""
         ilfl = ""
         irfl = ""

         iprtv = -1
         iprtw = -1
         iprtd = -1
         iprtu = -1
         iprty = -1
         iprts = -1

         isavt = ""
         iwdgt = ""
         iwrkt = ""
         inmu = ""
         inmy = ""

         wmina = 0.2
         wmaxa = 3.0
         bbnd = 0.0
         ubnd = 3.0

         ipc(1) = -1
         ipc(2) = -1
         ipc(3) = -1
         ipc(4) = -1
         ipc(5) = -1
         ipc(6) = -1
         ipc(7) = -1
         ipc(8) = -1
         ipc(9) = -1
         ipc(10) = -1
         istarp(1) = -1
         istarp(2) = -1
         iauto = c

         alat = -1.0
         ra = -1.0
         dec = -1.0
         ha = -1.0
         airmas = -1.0
         iwch = -1

         mag0 = -1
         mag1 = -1
         isavf = -1
         iwjf = -1
         iwrkf = -1
         isvcu = -1
         iwjcy = -1
         istrf = -1
         ilpt = -1
         icdr = -1
         ipp = -1

         nchans = -1
         ibnrm1 = -1
         ibnrm2 = -1
         nchsav = -1
         iline = -1

         infmth = -1
         infopr = -1
         inftrn = -1
         iother(1) = -1
         iother(2) = -1
         iother(3) = -1
         iother(4) = -1
         iother(5) = -1
         iother(6) = -1
         iother(7) = -1
         iother(8) = -1
         iother(9) = -1

         cfile = ""

         igrmod = -1

         itrol(1) = 538976342
         itrol(2) = 6
         itrol(3) = 538976353

#  ***   to do: zero out array filtyp(12,5) later  ***


#        restart rstart-file
29       continue

	   write (ttyout,50)
50       format(' Restarting ')
         close(rlun,iostat=idummy)

         if (ncmdarg >= 1) {
            irfl = charg1
            write (ttyout, 30) irfl
30          format ('restart file= ', a)
         }

33       continue

#  sole SPECPR Version 2 ascii restart file-open statement here ONLY
         open(rlun,file=irfl,iostat=ier,
            access='sequential',form='formatted')
         if (ier != 0) goto 1234

#  end of file open routine (no restart reads)
         if (ic == 4) return

#  read restart version number
         read(rlun,'(A120)',end=511,iostat=ier) iopcon2
            if (ier != 0) goto 1234
         rstartver = iopcon2(1:8)

#  test for version 1 Speclab1 HPUX version of restart file
         if (rstartver(1:1) != "2") {
            write(*,*) "entering version 1 restart open"
            call rstart0
#              exit restart after ver. 1 read
#              (terminate by using file open only ic=4)
            ic = 4
#              prevent 'open' endless loop
            itrip = itrip + 1
            if (itrip >= 2) {
               write(*,*) "stopping: restart file unreadable !"
               stop
            }
         }
         if (rstartver(1:1) != "2") goto 29

#
#        START version 2 read LOOP (statements 510-511)
#
510      read(rlun,'(A120)',end=511,iostat=ier) iopcon2
            if (ier != 0) goto 1234

         iopcon = iopcon2(1:80)
#
#        lblvol.h
#        8 - 80 character filenames
#          (filename must start at character position 1)
#		max command string = 120 char (iopcon2)
#		(icmdmax = 80)
#
         if(iopcon2(1:4)=='ivfl') {
            istart = 4 + ifnb(iopcon2(5:4 + icmdmax))
            i = istart + lnb(iopcon2(istart:4 + icmdmax))
            ivfl = iopcon2(istart:i) }
         if(iopcon2(1:4)=='iwfl') {
            istart = 4 + ifnb(iopcon2(5:4 + icmdmax))
            i = istart + lnb(iopcon2(istart:4 + icmdmax))
            iwfl = iopcon2(istart:i) }
         if(iopcon2(1:4)=='idfl') {
            istart = 4 + ifnb(iopcon2(5:4 + icmdmax))
            i = istart + lnb(iopcon2(istart:4 + icmdmax))
            idfl = iopcon2(istart:i) }
         if(iopcon2(1:4)=='iufl') {
            istart = 4 + ifnb(iopcon2(5:4 + icmdmax))
            i = istart + lnb(iopcon2(istart:4 + icmdmax))
            iufl = iopcon2(istart:i) }
         if(iopcon2(1:4)=='iyfl') {
            istart = 4 + ifnb(iopcon2(5:4 + icmdmax))
            i = istart + lnb(iopcon2(istart:4 + icmdmax))
            iyfl = iopcon2(istart:i) }
         if(iopcon2(1:4)=='isfl') {
            istart = 4 + ifnb(iopcon2(5:4 + icmdmax))
            i = istart + lnb(iopcon2(istart:4 + icmdmax))
            isfl = iopcon2(istart:i) }
         if(iopcon2(1:4)=='ilfl') {
            istart = 4 + ifnb(iopcon2(5:4 + icmdmax))
            i = istart + lnb(iopcon2(istart:4 + icmdmax))
            ilfl = iopcon2(istart:i) }
         if(iopcon2(1:4)=='irfl') {
            istart = 4 + ifnb(iopcon2(5:4 + icmdmax))
            i = istart + lnb(iopcon2(istart:4 + icmdmax))
            irfl = iopcon2(istart:i) }

#        lblprt.h
#        6 - protection # for open files (u,y,d,v,w,s)
#
         if (iopcon(1:5)=='iprtv') {
            i = 7
            call wjfren(i,x,il)
            if(il!=0) {
               lnerr='iprtv'
               goto 1235
            }
            iprtv = int(x)
         }

         if (iopcon(1:5)=='iprtw') {
            i = 7
            call wjfren(i,x,il)
            if(il!=0) {
               lnerr='iprtw'
               goto 1235
            }
            iprtw = int(x)
         }

         if (iopcon(1:5)=='iprtd') {
            i = 7
            call wjfren(i,x,il)
            if(il!=0) {
               lnerr='iprtd'
               goto 1235
            }
            iprtd = int(x)
         }

         if (iopcon(1:5)=='iprtu') {
            i = 7
            call wjfren(i,x,il)
            if(il!=0) {
               lnerr='iprtu'
               goto 1235
            }
            iprtu = int(x)
         }

         if (iopcon(1:5)=='iprty') {
            i = 7
            call wjfren(i,x,il)
            if(il!=0) {
               lnerr='iprty'
               goto 1235
            }
            iprty = int(x)
         }

         if (iopcon(1:5)=='iprts') {
            i = 7
            call wjfren(i,x,il)
            if(il!=0) {
               lnerr='iprts'
               goto 1235
            }
            iprts = int(x)
         }

#     lbl4.h
#     short 8 character names associated with file device letters
#
         if(iopcon2(1:5)=='isavt') {
            istart = 5 + ifnb(iopcon2(6:5 + icmdmax))
            i = istart + lnb(iopcon2(istart:istart+7))
            isavt = iopcon2(istart:i) }
         if(iopcon2(1:5)=='iwdgt') {
            istart = 5 + ifnb(iopcon2(6:5 + icmdmax))
            i = istart + lnb(iopcon2(istart:istart+7))
            iwdgt = iopcon2(istart:i) }
         if(iopcon2(1:5)=='iwrkt') {
            istart = 5 + ifnb(iopcon2(6:5 + icmdmax))
            i = istart + lnb(iopcon2(istart:istart+7))
            iwrkt = iopcon2(istart:i) }
         if(iopcon2(1:4)=='inmu') {
            istart = 4 + ifnb(iopcon2(5:4 + icmdmax))
            i = istart + lnb(iopcon2(istart:istart+7))
            inmu = iopcon2(istart:i) }
         if(iopcon2(1:4)=='inmy') {
            istart = 4 + ifnb(iopcon2(5:4 + icmdmax))
            i = istart + lnb(iopcon2(istart:istart+7))
            inmy = iopcon2(istart:i) }
#
#     lbl4.h continued
#     graphics plot control values
#
         if (iopcon(1:5)=='wmina') {
            i = 7
            call rlchng(i,x,il)
            if(il!=0) {
               lnerr='wmina'
               goto 1235
            }
            wmina = x
         }

         if (iopcon(1:5)=='wmaxa') {
            i = 7
            call rlchng(i,x,il)
            if(il!=0) {
               lnerr='wmaxa'
               goto 1235
            }
            wmaxa = x
         }

         if (iopcon(1:4)=='bbnd') {
            i = 6
            call rlchng(i,x,il)
            if(il!=0) {
               lnerr='bbnd'
               goto 1235
            }
            bbnd = x
         }

         if (iopcon(1:4)=='ubnd') {
            i = 6
            call rlchng(i,x,il)
            if(il!=0) {
               lnerr='ubnd'
               goto 1235
            }
            ubnd = x
         }
         if (iopcon(1:6)=='ipc(1)') {
            i = 8
            call wjfren(i,x,il)
            if(il!=0) {
               lnerr='ipc(1)'
               goto 1235
            }
            ipc(1) = x
         }
         if (iopcon(1:6)=='ipc(2)') {
            i = 8
            call wjfren(i,x,il)
            if(il!=0) {
               lnerr='ipc(2)'
               goto 1235
            }
            ipc(2) = x
         }
         if (iopcon(1:6)=='ipc(3)') {
            i = 8
            call wjfren(i,x,il)
            if(il!=0) {
               lnerr='ipc(3)'
               goto 1235
            }
            ipc(3) = x
         }
         if (iopcon(1:6)=='ipc(4)') {
            i = 8
            call wjfren(i,x,il)
            if(il!=0) {
               lnerr='ipc(4)'
               goto 1235
            }
            ipc(4) = x
         }
         if (iopcon(1:6)=='ipc(5)') {
            i = 8
            call wjfren(i,x,il)
            if(il!=0) {
               lnerr='ipc(5)'
               goto 1235
            }
            ipc(5) = x
         }
         if (iopcon(1:6)=='ipc(6)') {
            i = 8
            call wjfren(i,x,il)
            if(il!=0) {
               lnerr='ipc(6)'
               goto 1235
            }
            ipc(6) = x
         }
         if (iopcon(1:6)=='ipc(7)') {
            i = 8
            call wjfren(i,x,il)
            if(il!=0) {
               lnerr='ipc(7)'
               goto 1235
            }
            ipc(7) = x
         }
         if (iopcon(1:6)=='ipc(8)') {
            i = 8
            call wjfren(i,x,il)
            if(il!=0) {
               lnerr='ipc(8)'
               goto 1235
            }
            ipc(8) = x
         }
         if (iopcon(1:6)=='ipc(9)') {
            i = 8
            call wjfren(i,x,il)
            if(il!=0) {
               lnerr='ipc(9)'
               goto 1235
            }
            ipc(9) = x
         }
         if (iopcon(1:7)=='ipc(10)') {
            i = 9
            call wjfren(i,x,il)
            if(il!=0) {
               lnerr='ipc(10)'
               goto 1235
            }
            ipc(10) = x
         }
         if (iopcon(1:9)=='istarp(1)') {
            i = 11
            call wjfren(i,x,il)
            if(il!=0) {
               lnerr='istarp(1)'
               goto 1235
            }
            istarp(1) = x
         }
         if (iopcon(1:9)=='istarp(2)') {
            i = 11
            call wjfren(i,x,il)
            if(il!=0) {
               lnerr='istarp(2)'
               goto 1235
            }
            istarp(2) = x
         }
         if (iopcon(1:5)=='iauto') {
            istart = 5 + ifnb(iopcon2(6:5 + icmdmax))
            ctmp = iopcon2(istart:istart)
            itmp = 32
#            if ((ctmp >="a") && (ctmp <= "z")) {
               iauto = ihchar(ctmp)
#            }
         }

#     label3.h
#     6 - observatory variables:
#     (latitude,right ascension, declination,hour angle, air mass,
#      wavelength calibration shift in angstroms)
#
         if (iopcon(1:4)=='alat') {
            i = 6
            call wjfren(i,x,il)
            if(il!=0) {
               lnerr='alat'
               goto 1235
            }
            alat = x
         }

         if (iopcon(1:2)=='ra') {
            i = 4
            call wjfren(i,x,il)
            if(il!=0) {
               lnerr='ra'
               goto 1235
            }
            ra = x
         }

         if (iopcon(1:3)=='dec') {
            i = 5
            call wjfren(i,x,il)
            if(il!=0) {
               lnerr='dec'
               goto 1235
            }
            dec = x
         }

         if (iopcon(1:2)=='ha') {
            i = 4
            call wjfren(i,x,il)
            if(il!=0) {
               lnerr='ha'
               goto 1235
            }
            ha = x
         }

         if (iopcon(1:6)=='airmas') {
            i = 8
            call wjfren(i,x,il)
            if(il!=0) {
               lnerr='airmas'
               goto 1235
            }
            airmas = x
         }

         if (iopcon(1:4)=='iwch') {
            i = 6
            call wjfren(i,x,il)
            if(il!=0) {
               lnerr='iwch'
               goto 1235
            }
            iwch = int(x)
         }

#     labelf.h
#     11 - file pointers and status
#
         if (iopcon(1:4)=='mag0') {
            i = 6
            call wjfren(i,x,il)
            if(il!=0) {
               lnerr='mag0'
               goto 1235
            }
            mag0 = int(x)
         }

         if (iopcon(1:4)=='mag1') {
            i = 6
            call wjfren(i,x,il)
            if(il!=0) {
               lnerr='mag1'
               goto 1235
            }
            mag1 = int(x)
         }

         if (iopcon(1:5)=='isavf') {
            i = 7
            call wjfren(i,x,il)
            if(il!=0) {
               lnerr='isavf'
               goto 1235
            }
            isavf = int(x)
         }

         if (iopcon(1:4)=='iwjf') {
            i = 6
            call wjfren(i,x,il)
            if(il!=0) {
               lnerr='iwjf'
               goto 1235
            }
            iwjf = int(x)
         }

         if (iopcon(1:5)=='iwrkf') {
            i = 7
            call wjfren(i,x,il)
            if(il!=0) {
               lnerr='iwrkf'
               goto 1235
            }
            iwrkf = int(x)
         }

         if (iopcon(1:5)=='isvcu') {
            i = 7
            call wjfren(i,x,il)
            if(il!=0) {
               lnerr='isvcu'
               goto 1235
            }
            isvcu = int(x)
         }

         if (iopcon(1:5)=='iwjcy') {
            i = 7
            call wjfren(i,x,il)
            if(il!=0) {
               lnerr='iwjcy'
               goto 1235
            }
            iwjcy = int(x)
         }

         if (iopcon(1:5)=='istrf') {
            i = 7
            call wjfren(i,x,il)
            if(il!=0) {
               lnerr='istrf'
               goto 1235
            }
            istrf = int(x)
         }

         if (iopcon(1:4)=='ilpt') {
            i = 6
            call wjfren(i,x,il)
            if(il!=0) {
               lnerr='ilpt'
               goto 1235
            }
            ilpt = int(x)
         }

         if (iopcon(1:4)=='icdr') {
            i = 6
            call wjfren(i,x,il)
            if(il!=0) {
               lnerr='icdr'
               goto 1235
            }
            icdr = int(x)
         }

         if (iopcon(1:3)=='ipp') {
            i = 5
            call wjfren(i,x,il)
            if(il!=0) {
               lnerr='ipp'
               goto 1235
            }
            ipp = int(x)
         }

#     lblg.h
#     spectrum in use specifics
#
         if (iopcon(1:6)=='nchans') {
            i = 8
            call wjfren(i,x,il)
            if(il!=0) {
               lnerr='nchans'
               goto 1235
            }
            nchans = int(x)
         }
         if (iopcon(1:6)=='ibnrm1') {
            i = 8
            call wjfren(i,x,il)
            if(il!=0) {
               lnerr='ibnrm1'
               goto 1235
            }
            ibnrm1 = int(x)
         }
         if (iopcon(1:6)=='ibnrm2') {
            i = 8
            call wjfren(i,x,il)
            if(il!=0) {
               lnerr='ibnrm2'
               goto 1235
            }
            ibnrm2 = int(x)
         }
         if (iopcon(1:6)=='nchsav') {
            i = 8
            call wjfren(i,x,il)
            if(il!=0) {
               lnerr='nchsav'
               goto 1235
            }
            nchsav = int(x)
         }
         if (iopcon(1:5)=='iline') {
            i = 7
            call wjfren(i,x,il)
            if(il!=0) {
               lnerr='iline'
               goto 1235
            }
            iline = int(x)
         }

#     info.h
#     info print on crt control flags
#
         if (iopcon(1:6)=='infmth') {
            i = 8
            call wjfren(i,x,il)
            if(il!=0) {
               lnerr='infmth'
               goto 1235
            }
            infmth = int(x)
         }
         if (iopcon(1:6)=='infopr') {
            i = 8
            call wjfren(i,x,il)
            if(il!=0) {
               lnerr='infopr'
               goto 1235
            }
            infopr = int(x)
         }
         if (iopcon(1:6)=='inftrn') {
            i = 8
            call wjfren(i,x,il)
            if(il!=0) {
               lnerr='inftrn'
               goto 1235
            }
            inftrn = int(x)
         }
         if (iopcon(1:9)=='iother(1)') {
            i = 11
            call wjfren(i,x,il)
            if(il!=0) {
               lnerr='iother(1)'
               goto 1235
            }
            iother(1) = int(x)
         }
         if (iopcon(1:9)=='iother(2)') {
            i = 11
            call wjfren(i,x,il)
            if(il!=0) {
               lnerr='iother(2)'
               goto 1235
            }
            iother(2) = int(x)
         }
         if (iopcon(1:9)=='iother(3)') {
            i = 11
            call wjfren(i,x,il)
            if(il!=0) {
               lnerr='iother(3)'
               goto 1235
            }
            iother(3) = int(x)
         }
         if (iopcon(1:9)=='iother(4)') {
            i = 11
            call wjfren(i,x,il)
            if(il!=0) {
               lnerr='iother(4)'
               goto 1235
            }
            iother(4) = int(x)
         }
         if (iopcon(1:9)=='iother(5)') {
            i = 11
            call wjfren(i,x,il)
            if(il!=0) {
               lnerr='iother(5)'
               goto 1235
            }
            iother(5) = int(x)
         }
         if (iopcon(1:9)=='iother(6)') {
            i = 11
            call wjfren(i,x,il)
            if(il!=0) {
               lnerr='iother(6)'
               goto 1235
            }
            iother(6) = int(x)
         }
         if (iopcon(1:9)=='iother(7)') {
            i = 11
            call wjfren(i,x,il)
            if(il!=0) {
               lnerr='iother(7)'
               goto 1235
            }
            iother(7) = int(x)
         }
         if (iopcon(1:9)=='iother(8)') {
            i = 11
            call wjfren(i,x,il)
            if(il!=0) {
               lnerr='iother(8)'
               goto 1235
            }
            iother(8) = int(x)
         }
         if (iopcon(1:9)=='iother(9)') {
            i = 11
            call wjfren(i,x,il)
            if(il!=0) {
               lnerr='iother(9)'
               goto 1235
            }
            iother(9) = int(x)
         }

#     cmd.h
#	NOTE: icmdmax + 40 = 120; iopcon2 = 120; so array not exceeded
         if(iopcon2(1:5)=='cfile') {
            istart = 5 + ifnb(iopcon2(6:5 + icmdmax))
            i = istart + lnb(iopcon2(istart:istart + 39))
            cfile = iopcon2(istart:i) }

#     hptrm.h
         if (iopcon(1:6)=='igrmod') {
            i = 8
            call wjfren(i,x,il)
            if(il!=0) {
               lnerr='igrmod'
               goto 1235
            }
            igrmod = int(x)
         }

#     lbl7.h
#     variables lbl(23) - lbl(25): itrol(1) - itrol(3)
#     wavelength file id, record in use, and
#     chan/wave/energy plot flag
         if (iopcon(1:8)=='itrol(1)') {
            istart = 8 + ifnb(iopcon2(9:8 + icmdmax))
            ctmp = iopcon2(istart:istart)
            itmp = 32
            if ((ctmp >="A") && (ctmp <= "Z")) {
               itrol(1) = ihchar(ctmp)
#   itrol(1)=((256*itmp + itmp)*256 + itmp)*256 + ichar(ctmp)
            }
         }
         if (iopcon(1:8)=='itrol(2)') {
            i = 10
            call wjfren(i,x,il)
            if(il!=0) {
               lnerr='itrol(2)'
               goto 1235
            }
            itrol(2) = int(x)
         }
         if (iopcon(1:8)=='itrol(3)') {
            istart = 8 + ifnb(iopcon2(9:8 + icmdmax))
            ctmp = iopcon2(istart:istart)
            itmp = 32
            if ((ctmp >="a") && (ctmp <= "z")) {
               itrol(3) = ihchar(ctmp)
#   itrol(3)=((256*itmp + itmp)*256 + itmp)*256 + ichar(ctmp)
            }
         }

#     ioftyp.h
#     3D file reading routine
#
         if (iopcon(1:11)=='filtyp(1,1)') {
            i = 13
            call wjfren(i,x,il)
            if(il!=0) goto 1234
            filtyp(1,1) = int(x)
         }
         if (iopcon(1:11)=='filtyp(2,1)') {
            i = 13
            call wjfren(i,x,il)
            if(il!=0) goto 1234
            filtyp(2,1) = int(x)
         }
         if (iopcon(1:11)=='filtyp(3,1)') {
            i = 13
            call wjfren(i,x,il)
            if(il!=0) goto 1234
            filtyp(3,1) = int(x)
         }
         if (iopcon(1:11)=='filtyp(4,1)') {
            i = 13
            call wjfren(i,x,il)
            if(il!=0) goto 1234
            filtyp(4,1) = int(x)
         }
         if (iopcon(1:11)=='filtyp(5,1)') {
            i = 13
            call wjfren(i,x,il)
            if(il!=0) goto 1234
            filtyp(5,1) = int(x)
         }
         if (iopcon(1:11)=='filtyp(6,1)') {
            i = 13
            call wjfren(i,x,il)
            if(il!=0) goto 1234
            filtyp(6,1) = int(x)
         }
         if (iopcon(1:11)=='filtyp(7,1)') {
            i = 13
            call wjfren(i,x,il)
            if(il!=0) goto 1234
            filtyp(7,1) = int(x)
         }
         if (iopcon(1:11)=='filtyp(8,1)') {
            i = 13
            call wjfren(i,x,il)
            if(il!=0) goto 1234
            filtyp(8,1) = int(x)
         }
         if (iopcon(1:11)=='filtyp(9,1)') {
            i = 13
            call wjfren(i,x,il)
            if(il!=0) goto 1234
            filtyp(9,1) = int(x)
         }
         if (iopcon(1:12)=='filtyp(10,1)') {
            i = 14
            call wjfren(i,x,il)
            if(il!=0) goto 1234
            filtyp(10,1) = int(x)
         }
         if (iopcon(1:12)=='filtyp(11,1)') {
            i = 14
            call wjfren(i,x,il)
            if(il!=0) goto 1234
            filtyp(11,1) = int(x)
         }
         if (iopcon(1:12)=='filtyp(12,1)') {
            i = 14
            call wjfren(i,x,il)
            if(il!=0) goto 1234
            filtyp(12,1) = int(x)
         }
#
         if (iopcon(1:11)=='filtyp(1,2)') {
            i = 13
            call wjfren(i,x,il)
            if(il!=0) goto 1234
            filtyp(1,2) = int(x)
         }
         if (iopcon(1:11)=='filtyp(2,2)') {
            i = 13
            call wjfren(i,x,il)
            if(il!=0) goto 1234
            filtyp(2,2) = int(x)
         }
         if (iopcon(1:11)=='filtyp(3,2)') {
            i = 13
            call wjfren(i,x,il)
            if(il!=0) goto 1234
            filtyp(3,2) = int(x)
         }
         if (iopcon(1:11)=='filtyp(4,2)') {
            i = 13
            call wjfren(i,x,il)
            if(il!=0) goto 1234
            filtyp(4,2) = int(x)
         }
         if (iopcon(1:11)=='filtyp(5,2)') {
            i = 13
            call wjfren(i,x,il)
            if(il!=0) goto 1234
            filtyp(5,2) = int(x)
         }
         if (iopcon(1:11)=='filtyp(6,2)') {
            i = 13
            call wjfren(i,x,il)
            if(il!=0) goto 1234
            filtyp(6,2) = int(x)
         }
         if (iopcon(1:11)=='filtyp(7,2)') {
            i = 13
            call wjfren(i,x,il)
            if(il!=0) goto 1234
            filtyp(7,2) = int(x)
         }
         if (iopcon(1:11)=='filtyp(8,2)') {
            i = 13
            call wjfren(i,x,il)
            if(il!=0) goto 1234
            filtyp(8,2) = int(x)
         }
         if (iopcon(1:11)=='filtyp(9,2)') {
            i = 13
            call wjfren(i,x,il)
            if(il!=0) goto 1234
            filtyp(9,2) = int(x)
         }
         if (iopcon(1:12)=='filtyp(10,2)') {
            i = 14
            call wjfren(i,x,il)
            if(il!=0) goto 1234
            filtyp(10,2) = int(x)
         }
         if (iopcon(1:12)=='filtyp(11,2)') {
            i = 14
            call wjfren(i,x,il)
            if(il!=0) goto 1234
            filtyp(11,2) = int(x)
         }
         if (iopcon(1:12)=='filtyp(12,2)') {
            i = 14
            call wjfren(i,x,il)
            if(il!=0) goto 1234
            filtyp(12,2) = int(x)
         }
#
         if (iopcon(1:11)=='filtyp(1,3)') {
            i = 13
            call wjfren(i,x,il)
            if(il!=0) goto 1234
            filtyp(1,3) = int(x)
         }
         if (iopcon(1:11)=='filtyp(2,3)') {
            i = 13
            call wjfren(i,x,il)
            if(il!=0) goto 1234
            filtyp(2,3) = int(x)
         }
         if (iopcon(1:11)=='filtyp(3,3)') {
            i = 13
            call wjfren(i,x,il)
            if(il!=0) goto 1234
            filtyp(3,3) = int(x)
         }
         if (iopcon(1:11)=='filtyp(4,3)') {
            i = 13
            call wjfren(i,x,il)
            if(il!=0) goto 1234
            filtyp(4,3) = int(x)
         }
         if (iopcon(1:11)=='filtyp(5,3)') {
            i = 13
            call wjfren(i,x,il)
            if(il!=0) goto 1234
            filtyp(5,3) = int(x)
         }
         if (iopcon(1:11)=='filtyp(6,3)') {
            i = 13
            call wjfren(i,x,il)
            if(il!=0) goto 1234
            filtyp(6,3) = int(x)
         }
         if (iopcon(1:11)=='filtyp(7,3)') {
            i = 13
            call wjfren(i,x,il)
            if(il!=0) goto 1234
            filtyp(7,3) = int(x)
         }
         if (iopcon(1:11)=='filtyp(8,3)') {
            i = 13
            call wjfren(i,x,il)
            if(il!=0) goto 1234
            filtyp(8,3) = int(x)
         }
         if (iopcon(1:11)=='filtyp(9,3)') {
            i = 13
            call wjfren(i,x,il)
            if(il!=0) goto 1234
            filtyp(9,3) = int(x)
         }
         if (iopcon(1:12)=='filtyp(10,3)') {
            i = 14
            call wjfren(i,x,il)
            if(il!=0) goto 1234
            filtyp(10,3) = int(x)
         }
         if (iopcon(1:12)=='filtyp(11,3)') {
            i = 14
            call wjfren(i,x,il)
            if(il!=0) goto 1234
            filtyp(11,3) = int(x)
         }
         if (iopcon(1:12)=='filtyp(12,3)') {
            i = 14
            call wjfren(i,x,il)
            if(il!=0) goto 1234
            filtyp(12,3) = int(x)
         }
#
         if (iopcon(1:11)=='filtyp(1,4)') {
            i = 13
            call wjfren(i,x,il)
            if(il!=0) goto 1234
            filtyp(1,4) = int(x)
         }
         if (iopcon(1:11)=='filtyp(2,4)') {
            i = 13
            call wjfren(i,x,il)
            if(il!=0) goto 1234
            filtyp(2,4) = int(x)
         }
         if (iopcon(1:11)=='filtyp(3,4)') {
            i = 13
            call wjfren(i,x,il)
            if(il!=0) goto 1234
            filtyp(3,4) = int(x)
         }
         if (iopcon(1:11)=='filtyp(4,4)') {
            i = 13
            call wjfren(i,x,il)
            if(il!=0) goto 1234
            filtyp(4,4) = int(x)
         }
         if (iopcon(1:11)=='filtyp(5,4)') {
            i = 13
            call wjfren(i,x,il)
            if(il!=0) goto 1234
            filtyp(5,4) = int(x)
         }
         if (iopcon(1:11)=='filtyp(6,4)') {
            i = 13
            call wjfren(i,x,il)
            if(il!=0) goto 1234
            filtyp(6,4) = int(x)
         }
         if (iopcon(1:11)=='filtyp(7,4)') {
            i = 13
            call wjfren(i,x,il)
            if(il!=0) goto 1234
            filtyp(7,4) = int(x)
         }
         if (iopcon(1:11)=='filtyp(8,4)') {
            i = 13
            call wjfren(i,x,il)
            if(il!=0) goto 1234
            filtyp(8,4) = int(x)
         }
         if (iopcon(1:11)=='filtyp(9,4)') {
            i = 13
            call wjfren(i,x,il)
            if(il!=0) goto 1234
            filtyp(9,4) = int(x)
         }
         if (iopcon(1:12)=='filtyp(10,4)') {
            i = 14
            call wjfren(i,x,il)
            if(il!=0) goto 1234
            filtyp(10,4) = int(x)
         }
         if (iopcon(1:12)=='filtyp(11,4)') {
            i = 14
            call wjfren(i,x,il)
            if(il!=0) goto 1234
            filtyp(11,4) = int(x)
         }
         if (iopcon(1:12)=='filtyp(12,4)') {
            i = 14
            call wjfren(i,x,il)
            if(il!=0) goto 1234
            filtyp(12,4) = int(x)
         }
#
         if (iopcon(1:11)=='filtyp(1,5)') {
            i = 13
            call wjfren(i,x,il)
            if(il!=0) goto 1234
            filtyp(1,5) = int(x)
         }
         if (iopcon(1:11)=='filtyp(2,5)') {
            i = 13
            call wjfren(i,x,il)
            if(il!=0) goto 1234
            filtyp(2,5) = int(x)
         }
         if (iopcon(1:11)=='filtyp(3,5)') {
            i = 13
            call wjfren(i,x,il)
            if(il!=0) goto 1234
            filtyp(3,5) = int(x)
         }
         if (iopcon(1:11)=='filtyp(4,5)') {
            i = 13
            call wjfren(i,x,il)
            if(il!=0) goto 1234
            filtyp(4,5) = int(x)
         }
         if (iopcon(1:11)=='filtyp(5,5)') {
            i = 13
            call wjfren(i,x,il)
            if(il!=0) goto 1234
            filtyp(5,5) = int(x)
         }
         if (iopcon(1:11)=='filtyp(6,5)') {
            i = 13
            call wjfren(i,x,il)
            if(il!=0) goto 1234
            filtyp(6,5) = int(x)
         }
         if (iopcon(1:11)=='filtyp(7,5)') {
            i = 13
            call wjfren(i,x,il)
            if(il!=0) goto 1234
            filtyp(7,5) = int(x)
         }
         if (iopcon(1:11)=='filtyp(8,5)') {
            i = 13
            call wjfren(i,x,il)
            if(il!=0) goto 1234
            filtyp(8,5) = int(x)
         }
         if (iopcon(1:11)=='filtyp(9,5)') {
            i = 13
            call wjfren(i,x,il)
            if(il!=0) goto 1234
            filtyp(9,5) = int(x)
         }
         if (iopcon(1:12)=='filtyp(10,5)') {
            i = 14
            call wjfren(i,x,il)
            if(il!=0) goto 1234
            filtyp(10,5) = int(x)
         }
         if (iopcon(1:12)=='filtyp(11,5)') {
            i = 14
            call wjfren(i,x,il)
            if(il!=0) goto 1234
            filtyp(11,5) = int(x)
         }
         if (iopcon(1:12)=='filtyp(12,5)') {
            i = 14
            call wjfren(i,x,il)
            if(il!=0) goto 1234
            filtyp(12,5) = int(x)
         }

#
#        loop read back up to label 500 until end-of-file
#
         goto 510
511      continue

      }
      if (ic==3) return

#
#----------------------------------------------------------------------
#     ic= 2: restart= assign and open all the files and devices.
#                 (Continue when ic = 2)
#                 open all files and devices as appropriate.
#

	open(cmdlun,file=COMMAND,iostat=idummy,
		access='direct',recl=80,form='unformatted')
	if (idummy!=0) {
		write(ttyout,100)
		stop
	}
#
#     assign title file
#
	open(ttllun,file=TITLE,iostat=idummy,
		access='direct',recl=128,form='unformatted')
	if (idummy!=0) {
		write(ttyout,400)
		stop
	}
#
#     assign addition scratch file
#
	open(addlun,access='direct',recl=19456,form='unformatted',
		iostat=idummy,status='scratch')
	if (idummy!=0) {
		write(ttyout,500)
		stop
	}
#
#     assign plot scratch file
#
	open(pltlun,access='direct',recl=80,form='unformatted',
		iostat=idummy,status='scratch')
	if (idummy!=0) {
		write(ttyout,600)
		stop
	}
#
#     assign device v
#
	inn = 4
	if (filtyp(1,inn) == 3) {   # special 3D file
		irecl = filtyp(3,inn)
	} else {                    # normal specpr file
		irecl = 1536
	}
	if (ivfl != NULL) open(vlun,file=ivfl,iostat=idummy,
		access='direct',recl=irecl,form='unformatted')
	if (idummy!=0) {
		write(ttyout,700) ivfl
		ivfl = NULL
	}
#
#     assign device w
#
	inn = 5
	if (filtyp(1,inn) == 3) {   # special 3D file
		irecl = filtyp(3,inn)
	} else {                    # normal specpr file
		irecl = 1536
	}
	if (iwfl != NULL) open(wlun,file=iwfl,iostat=idummy,
		access='direct',recl=irecl,form='unformatted')
	if (idummy!=0) {
		write(ttyout,700) iwfl
		iwfl = NULL
	}
#
#     assign device d
#
	inn = 3
	if (filtyp(1,inn) == 3) {   # special 3D file
		irecl = filtyp(3,inn)
	} else {                    # normal specpr file
		irecl = 1536
	}
	if (idfl != NULL) open(dlun,file=idfl,iostat=idummy,
		access='direct',recl=irecl,form='unformatted')
	if (idummy!=0) {
		write(ttyout,700) idfl
		idfl = NULL
	}
#
#     assign starpack file =s
#
	if (isfl != NULL) open(slun,file=isfl,iostat=idummy,
		access='direct',recl=77824,form='unformatted')
	if (idummy!=0) {
		write(ttyout,700) isfl
		isfl = NULL
	}
#
#     assign device u
#
	inn = 1
	if (filtyp(1,inn) == 3) {   # special 3D file
		irecl = filtyp(3,inn)
	} else {                    # normal specpr file
		irecl = 1536
	}
	if (iufl != NULL) open(ulun,file=iufl,iostat=idummy,
		access='direct',recl=irecl,form='unformatted')
	if (idummy!=0) {
		write(ttyout,700) iufl
		iufl = NULL
	}
#
#     assign device y
#
	inn = 2
	if (filtyp(1,inn) == 3) {   # special 3D file
		irecl = filtyp(3,inn)
	} else {                    # normal specpr file
		irecl = 1536
	}
	if (iyfl != NULL) open(ylun,file=iyfl,iostat=idummy,
		access='direct',recl=irecl,form='unformatted')
	if (idummy!=0) {
		write(ttyout,700) iyfl
		iyfl = NULL
	}
#
#     assign listing device: lp or dummy
#
	if (ilfl != NULL && ilfl != SPLFILE) open(lstlun,file=ilfl,
		iostat=idummy,form='formatted')
	if (idummy!=0) {
		write(ttyout,700) ilfl
		ilfl = NULL
	}

#     warning errors
100	format('Can''t open command history file (.cmd). exiting!!')
400	format('Can''t open title storage file (.spttl). exiting!!')
500	format('Can''t open addition scratch file. exiting!!')
600	format('Can''t open plot scratch file. exiting!!')
700	format('Can''t open data file ',a,/,
			'reseting it to /dev/null')

      return
#
#----------------------------------------------------------------------
#     error handling routines
#
1234  write(ttyout,"('read error-restart file ier=',i5)")ier
      stop
1235  write(ttyout,"('parsing error-restart file value error=',a8)")lnerr
      stop
#
      end
