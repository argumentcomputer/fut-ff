SRC=./lib/github.com/porcuquine/fut-ff

test: $(SRC)/*.fut
	futhark test $(SRC)/*.fut

