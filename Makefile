SRC = ./src/
OUT = ./out/

all:
	ghc --make -i$(SRC) -o idbjava $(SRC)Main.hs -outputdir $(OUT)
