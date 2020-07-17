SRC=./lib/github.com/filecoin-project/fut-ff

test: $(SRC)/*.fut
	futhark test $(SRC)/*.fut

