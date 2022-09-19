SHELL = /bin/sh
#set compilier and flags
FC = ifort
FFLAGS=  -qopenmp -O3 -132 -zero -save -common-args -traceback
#FFLAGS= -qopenmp -traceback
#FFLAGS = -O -Mvect=sse -Mextend -Msecond_underscore -Bstatic_pgi -Mlfs
PROGRAM = NOX_CRF_AVE.exe
#LIBS = -L/work/dsc1/COMMON_LIBRARYS/local/IOAPI_PGI/Linux2_x86_64pg_pgcc_nomp -lioapi \
       -L/work/dsc1/COMMON_LIBRARYS/local/NETCDF_PGI/lib -lnetcdff -lnetcdf
#INCLUDE = -I/work/dsc1/COMMON_LIBRARYS/ioapi-3.1/ioapi/fixed_src \
          -I/work/dsc1/COMMON_LIBRARYS/local/NETCDF_PGI/include
LIBS = -L/home/ll70/libs/ioapi3.1/Linux2_x86_64ifort -lioapi \
       -L/home/ll70/libs/netcdf/libs -lnetcdff -lnetcdf -lhdf5_hl -lhdf5 -lz
INCLUDE = -I/home/ll70/libs/ioapi3.1/ioapi/fixed_src \
 	 -I/home/ll70/libs/netcdf/include

RAW = sunparam.o GET_CANOPY_R.o czangle.o NOX_CRF_AVE.o

.f.o:
	$(FC) $(FFLAGS) $(INCLUDE) -c -o $@ $<

.F.o:
	$(FC) $(FFLAGS) $(INCLUDE) -c -o $@ $<

$(PROGRAM):	$(RAW)
	$(FC) $(FFLAGS) $(INCLUDE) -o $(@) $(RAW) $(LIBS)

clean:
	rm -f $(PROGRAM) *.o
