#include "../tape.h"
#include <sys/ioctl.h>

/* 
 * fortran interface to backward space a mag tape (by records)
 *   usage:
 *		call bkrec(lun,count,ier)
 *
 *	arguments:
 *		lun		integer*2 variable which contains the 
 *				fortran logical unit number of the tape drive.
 *		count	integer*2 variable which contains the number
 *				or records to space.
 *		ier		interer*2 variable used to return error indications
 *				for the rewind call. 0 indicates normal completion.
 *				nonzero values are the UNIX error number. 
 *				See intro(2) in the UNIX Manual for a description
 *				of these error numbers.
 */


bkrec_(lun,count,ier)
short	*lun;
short	*count;
short 	*ier;
{
	return;
}
