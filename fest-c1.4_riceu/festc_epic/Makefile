# SWAT MAKEFILE FOR INTEL COMPILER on LINUX
#
F95=ifort

#FFLAGS= -O3 -132 -zero -save -common-args -traceback
FFLAGS= -O3 -132 -zero -save -common-args

#LDFLAGS= $(FFLAGS) -static -static-intel
LDFLAGS= $(FFLAGS) -static-intel
EXEAPP=EPICappriceu_20220711.exe
EXESU=EPICappriceu_20220711.exe

SRCAPP=EPIC0509app_1.4_20220711.for
SRCSU=EPIC0509app_biochar_20220711.for

#OBJAPP=$(SRCAPP:.f=.o)
OBJSU=$(SRCSU:.f=.o)

all: $(EXEAPP) $(EXESU)
clean:
	rm -f *.o $(EXESU)
	rm -f *.o $(EXEAPP)

$(EXEAPP): $(OBJAPP)
	$(F95) $(LDFLAGS) $(OBJAPP) -o $@

$(EXESU): $(OBJSU)
	$(F95) $(LDFLAGS) $(OBJSU) -o $@

