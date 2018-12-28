#
#   Makefile for making the executable of program DENSITY
#
#
#    Valid Commands of this makefile
#
#	make		Makes the DENSITY file
#	make clean	Clean up disk to minimum config
#
FFLAGS	        = -O  -c 
CFLAGS		= -O  -c -I/usr/openwin/include -lX11
HDRS		= SIZES
SRCS:sh		= ls *.f *.c
TMPS		= $(SRCS:.f=.o)
OBJS		= $(TMPS:.c=.o)
SIZEDEPSRC:sh	= grep -l -i '      INCLUDE ' *.f; true
SIZEDEPEND	= $(SIZEDEPSRC:.f=.o)

draw.exe:     	SIZES $(OBJS) 
		@echo -n "Loading density.exe ... "
		f77  $(OBJS) -L/usr/openwin/lib/X11 -lX11 -o ../density.exe
		@echo "done"

clean:
	 	rm -f $(OBJS)


$(SIZEDEPEND):	$$< $(HDRS) 
		$(FC) $(FFLAGS) $<

###
