stackdir := $(shell stack path | grep local-install-root | cut -c 21-)/bin/eden.jsexe

bin/HaskellReverse.js: src/Main.hs src/Exports.js
	stack build
	echo "(function(global) {" > bin/HaskellReverse.js
	cat $(stackdir)/rts.js >> bin/HaskellReverse.js
	cat $(stackdir)/lib.js >> bin/HaskellReverse.js
	cat $(stackdir)/out.js >> bin/HaskellReverse.js
	cat src/Exports.js >> bin/HaskellReverse.js
	echo "})(exports);" >> bin/HaskellReverse.js

.PHONY: clean
clean:

