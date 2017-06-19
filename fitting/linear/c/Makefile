FC=gcc
CFLAGS=-O3
LDFLAGS=-lm
EXE=linfit.x
SRC=linfit.c
OBJ=linfit.o

all: $(EXE)

$(EXE): $(OBJ)
	$(FC) $(LDFLAGS) $(OBJ) -o $(EXE)

$(OBJ): $(SRC)
	$(FC) $(CFLAGS) -c $< -o $@

clean:
	rm -f $(OBJ) $(EXE)
