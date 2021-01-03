	subroutine hpline (i)
	implicit integer*4 (i-n)

#ccc  version date: 06/01/83
#ccc  author(s): Roger Clark & Jeff Hoover
#ccc  language:  Ratfor
#ccc
#ccc  short description:
#ccc                   This subroutine selects the line type
#ccc  algorithm description: none
#ccc  system requirements:   none
#ccc  subroutines called:
#ccc                    texmod,convrt
#ccc  argument list description:
#ccc      argument: i
#ccc  parameter description:
#ccc  common description:
#ccc  message files referenced:
#ccc  internal variables:
#ccc  file description:
#ccc  user command lines:
#ccc  update information:
#ccc  NOTES:
#ccc
###############################################################
#     this subroutine selects the line type:
#        i= 0 - just symbols
#         = 1 - solid line + symbols
#         = 2 - solid line
#         = 3 - solid line + errors
#         = 4 through 10 - hpterm line type i
###############################################################

	include "../common/hptrm"
#RED
	integer*4 iwrite    # function iwrite

	if (igrmod >= 99) return
	if (igrmod >= 20 && igrmod <= 22) return

	iline = 1
	if (i>3 & i<=10) iline= i
	if (i > 10 || i < 0) iline= 1
#
#     send esc*m <line type> bz
#
	if (igrmod < 20) {
		call texmod
		ihpout(1:4) = char(27) // '*m '
		m= iline
		call convrt (m,ihpout(5:10),nchars)
		ihpout(8:9) = 'bZ'
		iot = 9
		ii=iwrite(1,iot,ihpout)
		iot=0
	} else if (igrmod >= 50 && igrmod <= 53) {
#XWIN		call xsetline(iline)
	}

	return
	end
