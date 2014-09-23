unsigned char far *CurrentPos( void );
unsigned char far *ScrPtr( int x, int y );
void RestoreCurPos( unsigned char far *s );
void ScrollUp( int x, int y, int xx, int yy, int n );
void Beep( void );

#define VIDMODE   *(unsigned char far *) 0x00449lu
#define MONOSEG  0xb0000000L
#define COLOR 0xb8000000L
#define VIDSEG (unsigned char far *)((7 == VIDMODE) ? MONOSEG : COLOR )

#define BELL 7
#define TAB  9

#include <stdio.h>

void SayFA( char attrib ,char *fmt, ... )
{
	register unsigned char far *scptr = CurrentPos();
	unsigned char far *scend = ScrPtr( 80, 25 );
	char s[240];
	register char *cp = s;
	va_list argptr;

	va_start( argptr, format );
	vsprintf( s, fmt, argptr );
	va_end( argptr );

	while (*cp) {
		switch( *cp ) {
			case '\n':
				scptr = (unsigned char far *) ((long) scptr + (long) 160 );
			case '\r':
				scptr = (unsigned char far *)
					( ( ( (long) scptr - (long) VIDSEG ) /
						  (long) 160 ) * (long) 160 + (long) VIDSEG );
				cp++;
				break;
			case BELL:
				Beep();
				cp++;
				break;
			case TAB:
				scptr = (unsigned char far *) ( (long) scptr / 16 * 16 );
				cp++;
				break;
			default:
				*scptr++ = *cp++;
				*scptr++ = attrib;
				break;
		}
		if ( scptr > scend ) {
			ScrollUp( 1, 1, 80, 25, 1 );
			scptr -= 160;
		}
	}
	RestoreCurPos( scptr );
}
