	subroutine crtplt (nchans,xmax,xmin,lbnd,diff,y,x,iline)
	implicit integer*4 (i-n)

#ccc  version date: 06/01/83
#ccc  author(s): Roger Clark & Jeff Hoover
#ccc  language:  Ratfor
#ccc
#ccc  short description:
#ccc           This subroutine plots the points and error bars
#ccc           for the crt plotting package.
#ccc  algorithm description: none
#ccc  system requirements:   none
#ccc  subroutines called:
#ccc                    movabs,drwabs,hpline
#ccc  argument list description:
#ccc    arguments: nchans,xmax,xmin,lbnd,diff,y,x,iline
#ccc    nchans  input   number of channels to be plotted
#ccc    xmax    input   maximum x-axis value to plot
#ccc    xmin    input   minimum x-axis value to plot
#ccc    lbnd    input   lower bound of y-axis plot
#ccc    diff    input   vertical axis scale factor
#ccc    y       input   array of y values to plot
#ccc    x       input   array of x values to plot
#ccc    iline   input   line type
#ccc       iline - 0 = crosses only (default)
#ccc               1 = line and crosses
#ccc               2 = line only
#ccc               3 = line with error bars if greater than 1 pixel
#ccc		   4-10= line with line type set by subroutine hpline(iline)
#ccc  parameter description:
#ccc  common description:
#ccc  message files referenced:
#ccc  internal variables:
#ccc  file description:
#ccc  user command lines:
#ccc  update information:
#ccc  NOTES:
#ccc


	include "../common/lbl3"

	real*4 x(4864), y(4864), lbnd
#
#     graph within limits (x,y) = (56,46),(56,276),(500,276),(500,46)
#
#     x and y limits of box =

#	axl = 56.   # original standard size
#	axh = 500.
#	ayl = 46.
#	ayh = 276.

	axl = 112.   # 2x size
	axh = 1000.
	ayl = 92.
	ayh = 552.

#
#     determine first point to plot ignore deleted points =
#       (=-1.23e34)
#

	for (i=1; i<=nchans+1 & y(i)==-1.23e34; i=i+1)
		;
	if (i==nchans+1) return
#
#     determine constants to scale data
#
	if (diff == 0.) diff = 0.1e-36
	dy = (ayh-ayl)/diff
	an = xmax - xmin
	if (an <= 0) an = 0.1e-36
	dx = (axh-axl)/an
#
#     determine cross size on each point = +ibar to -ibar
#
	ibar=1
	if (nchans <=200) ibar=2
	if (nchans <= 60) ibar=3
	if (nchans <= 40) ibar=4
	if (nchans <= 30) ibar=6
#
#     add 0.5 to reduce round off error
#
	aylr = ayl + 0.5
	axlr = axl + 0.5
#
#     set line type
#
	if ((iline>=4)&(iline<=10)) call hpline(iline)
#
#     do loop: plot points
#
	j = 0
	itmp = i
	do i= itmp, nchans {
		yy= y(i)
		if (x(i)<xmin || x(i)>xmax || yy==-1.23e34) next
		yii = (yy - lbnd) * dy + aylr
		xii = (x(i) - xmin) * dx + axlr
#
#     check to see if point is out of bounds
#
		if (yii > ayh) yii= ayh
		if (yii < ayl) yii= ayl
		if (xii > axh) xii= axh
		if (xii < axl) xii= axl
		iix= xii
		iiy= yii
		j=j+1
		if (j == 1) call movabs(iix,iiy)
#
#     if iline == 3 draw just points
#     if iline == 2  draw lines
#
		ilt = iline
		if (ilt > 3) ilt = 2

		if (ilt == 3) {
			call drwabs(iix,iiy)
			go to 260

		} else if (ilt == 2) {
			call drwabs(iix,iiy)
			next

		} else if (ilt == 1) {
			call drwabs(iix,iiy)
			call drwabs(iix-ibar,iiy)

		} else if (ilt == 0) {
			call movabs(iix-ibar,iiy)
		}

		call drwabs(iix+ibar,iiy)

#
#     if errors = 0 then skip plotting of error bars
#
260		if (error(i) == 0) {
#
#     if no error bar, complete plotting of cross unless line type 3
#
270			if (iline != 3) {
				call movabs(iix,iiy+ibar)
				call drwabs(iix,iiy-ibar)
			}
		} else {
#
#     compute error bar size
#
			ebarh = (yy + error(i) - lbnd) * dy + aylr
			ebarl = (yy - error(i) - lbnd) * dy + aylr
#
#     check to see if error bar out of bounds
#
			if (ebarh > ayh) ebarh= ayh
			if (ebarl < ayl) ebarl = ayl
			if (ebarl <= ayh & ebarh >= ayl) {
				iebarh = ebarh
				iebarl = ebarl
#
# determine whether or not to plot error bar
#
			if (iline != 3) {
				if ((ebarh-ebarl) < 1.0) go to 270
			} else {
				if ((ebarh-ebarl) < 2.0) next
			}
#
#     draw error bar if in bounds
#
				if (iebarh != iiy) {
					call movabs(iix-ibar,iebarh)
					call drwabs(iix+ibar,iebarh)
					call drwabs(iix,iebarh)
				} else
					call movabs(iix,iebarh)

				call drwabs(iix,iebarl)
				if (iebarl != iiy) {
					call drwabs(iix-ibar,iebarl)
					call drwabs (iix+ibar,iebarl)
				}
			}
#
#     return to center of point if going to draw line
#     connecting points
#
		}
		if (iline == 1 || iline == 3) call movabs(iix,iiy)
	}
	call hpline(1)
	return
	end
