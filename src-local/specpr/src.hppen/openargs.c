#include	"hpdaemon.h"
/* RED - Added to following include on RH5 64bit to eliminate TLS mismatch error */
#include <errno.h>

/*
 *	@(#)openargs.c	2.1 04/04/85 09:42:54
 */


static char Sccsid[]="@(#)openargs.c	2.1 04/04/85 09:42:54";

extern errno;

openargs()
{
	char line[128];
	int	error;

	copies = 1;

	GERR(error, line);
	if (error < 0) {
		logerr("Openargs: Can't get error status\n");
	} else {
		line[error] = '\0';
		logerr("Openargs: error = %s\n",line);
	}
	while (gets(line) != NULL) {
		switch (line[0]) {
		case 'T':
			errno = 0;
			text = fopen(&line[1],"r");
			logerr("Text = %s\n", &line[1]);
			break;
		case 'V':
			errno = 0;
			vector = fopen(&line[1],"r");
			logerr("Vector = %s\n", &line[1]);
			break;
		case 'C':
			copies = atoi(&line[1]);
			if (copies<1 || copies>9) copies=1;
			if (vector==(FILE *)NULL || text==(FILE *)NULL) {
				logerr("Can't open input files vector=%d, text=%d\n",vector,text);
				exit(BAD_INPUT_FILE);
			}
			return (1);
		default:
			break;
		}
	}
	if (vector!=NULL) fclose(vector);
	if (text!=NULL) fclose(text);
	return (0);
}
