FC=gcc
CFLAGS=-O3
LDFLAGS=-llapack -lm
EXE=nonlinfit.x
SRC=nonlinfit.c
OBJ=nonlinfit.o

all: $(EXE)

$(EXE): $(OBJ)
	$(FC) $(LDFLAGS) $(OBJ) -o $(EXE)

$(OBJ): $(SRC)
	$(FC) $(CFLAGS) -c $< -o $@

clean:
	rm -f $(OBJ) $(EXE)
