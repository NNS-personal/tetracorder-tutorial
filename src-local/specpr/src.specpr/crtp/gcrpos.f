C Output from Public domain Ratfor, version 1.0
      subroutine gcrpos (ixx,iyy,xpos,ypos,xmax,xmin,lbnd,diff,iopcon,ie
     *r)
      implicit integer*4(i-n)
Cccc  version date: 07/01/86
Cccc  author(s): roger clark
Cccc  language:  ratfor
Cccc
Cccc  short description:
Cccc            this subroutine reads the graphics cursor position
Cccc            and returns the x and y screen and scaled data values.
Cccc		iix,iiy = screen space -- xpos, ypos = data space
Cccc  algorithm description: none
Cccc  system requirements:   none
Cccc  subroutines called:
Cccc  argument list description:
Cccc     argumrnts: none
Cccc  parameter description:
Cccc  common description:
Cccc  message files referenced:
Cccc  internal variables:
Cccc  file description:
Cccc  user command lines:
Cccc  update information:
Cccc  notes:
Cccc
C     /lbl3/ common:
C       ictrl  : control flag for indicating errors are involved or a flag
C         indicating overlaping spectra in the display routine.
C       idad   : flag indicating that erorrs are included in the data (needs
C         to be phased out ).
C       ixit   : flag to indicate that user aborted routine and returned to
C         the main routines.
C
      common /lbl3/ ictrl, idad, ibncon,ibncn2, ixit
      integer*4 ictrl, idad, ibncon,ibncn2, ixit
C######  error  : errors to the data ( 1 standard deviation of the mean ).
      common /lblerr/ error(4864)
      real*4 error
      common /hptrm/ igrmod,ihpout,imode,ixlast,iylast,ipen
      common /hptrm/ iot
      character*80 ihpout
      integer*4 igrmod,imode,ixlast,iylast,ipen,iot
      integer*4 luntxt,ulun,ylun,ttyin,ttyout,dlun,vlun,wlun
      integer*4 addlun,wavlun,lstlun,pltlun,wvhlun
      integer*4 ttllun,cmdlun,slun,rlun,cpylun,wrtlun,redlun
      parameter (luntxt=1)
      parameter (ulun=3)
      parameter (ylun=4)
      parameter (ttyin=5)
      parameter (ttyout=6)
      parameter (dlun=7)
      parameter (vlun=8)
      parameter (wlun=9)
      parameter (addlun=10)
      parameter (redlun=10)
      parameter (wrtlun=11)
      parameter (wavlun=11)
      parameter (lstlun=12)
      parameter (pltlun=13)
      parameter (wvhlun=14)
      parameter (ttllun=15)
      parameter (cmdlun=16)
      parameter (slun=17)
      parameter (rlun=18)
      parameter (cpylun=19)
      common /alpha/ iha,ihb,ihc,ihd,ihe,ihf,ihg,ihh,ihi,ihj, ihk,ihl,ih
     *m,ihn,iho,ihp,ihq,ihr,ihs,iht, ihu,ihv,ihw,ihx,ihy,ihz
      common /alpha/ ihca,ihcb,ihcc,ihcd,ihce,ihcf,ihcg,ihch,ihci,ihcj, 
     *ihck,ihcl,ihcm,ihcn,ihco,ihcp,ihcq,ihcr,ihcs,ihct, ihcu,ihcv,ihcw,
     *ihcx,ihcy,ihcz
      common /digit/ ih0,ih1,ih2,ih3,ih4,ih5,ih6,ih7,ih8,ih9
      common /asciic/ihprd,ihandp
      integer*4 iha,ihb,ihc,ihd,ihe,ihf,ihg,ihh,ihi,ihj, ihk,ihl,ihm,ihn
     *,iho,ihp,ihq,ihr,ihs,iht, ihu,ihv,ihw,ihx,ihy,ihz
      integer*4 ihca,ihcb,ihcc,ihcd,ihce,ihcf,ihcg,ihch,ihci,ihcj, ihck,
     *ihcl,ihcm,ihcn,ihco,ihcp,ihcq,ihcr,ihcs,ihct, ihcu,ihcv,ihcw,ihcx,
     *ihcy,ihcz
      integer*4 ih0,ih1,ih2,ih3,ih4,ih5,ih6,ih7,ih8,ih9
      integer*4 ihprd,ihandp
      character*80 atemp, iopcon
      real*4 lbnd
C
C clear error variable.  its set for gcchan to use if necessary
C
      ier = 0
C	axl = 56.   # original standard size
C	axh = 500.
C	ayl = 46.
C	ayh = 276.
C 2x size
      axl = 112.
      axh = 1000.
      ayl = 92.
      ayh = 552.
      atemp = iopcon
C
C     determine constants to scale data
C
      if(diff .eq. 0.)then
      diff = 0.1e-36
      endif
      dy = (ayh-ayl)/diff
      an = xmax - xmin
      if(an .le. 0)then
      an = 0.1e-36
      endif
      dx = (axh-axl)/an
C
C this read is to 1) check for e or x, and 2) allow the user to position
C the graphics cursor, and then hit return to enter the spot
C
C
C flush graphics
C
      call sb(0)
1     if(igrmod .lt. 50 .or. igrmod .gt. 53)then
C not x-windows
      read (ttyin,2,end=2000,err=2000) iopcon
2     format (a)
      i = 1
      call wjfren (i,x,il)
      if(il .eq. ihe .or. il .eq. ihx)then
      iopcon = atemp
      ier = il
      return
      endif
C
C get cursor position
C
      endif
      call cursrd(ixx,iyy)
      if(ixx .eq. -1 .and. iyy .eq. -1)then
      ier = ihe
      return
      endif
      x = float (ixx)
      y = float(iyy)
      if(y .gt. ayh .or. y .lt. ayl .or. x .gt. axh .or. x .lt. axl)then
Ccall serase(0,318,511,348)
Ccall movabs (0,338)
      call serase(0,636,1022,696)
      call movabs (0,676)
      call sb(0)
      write (ttyout, 30) ixx,iyy
      call movabs (ixx, iyy)
      go to 1
C
C calculate x and y postion in data space
C
      endif
      xpos = (x-axl)/dx + xmin
      ypos = (y-ayl)/dy + lbnd
20	format (a,'*s3^',a,$)
C RED Added commas just before each i6 and after last i6 in 30 format statement 
30    format (20x,'OUT OF BOUNDS (',i6,',',i6,')')
2000  iopcon = atemp
      return
      end
