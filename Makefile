executables = xdlm_gfort.exe
FC     = gfortran
FFLAGS = -O0 -Wall -Werror=unused-parameter -Werror=unused-variable -Werror=unused-function -Wno-maybe-uninitialized -Wno-surprising -fbounds-check -static -g -fmodule-private
obj    = kind.o constants.o random.o dlm.o xdlm.o

all: $(executables)

# Compile .f90 to .o
%.o: %.f90
	$(FC) $(FFLAGS) -c $<

xdlm_gfort.exe: kind.o constants.o random.o dlm.o xdlm.o
	$(FC) -o xdlm_gfort.exe kind.o constants.o random.o dlm.o xdlm.o $(FFLAGS)

run: $(executables)
	./xdlm_gfort.exe

clean:
	rm -f $(executables) $(obj)

