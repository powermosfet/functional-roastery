all: elm haskell

elm: app.js 

app.js: frontend/*.elm
	elm-make frontend/Main.elm --yes --output static/app.js

haskell: backend/*.hs haskell-clean
	stack build

haskell-clean:
	stack clean

elm-clean:
	-rm -r static/app.js elm-stuff/ 

run: all
	stack exec functional-roastery

clean: elm-clean haskell-clean
