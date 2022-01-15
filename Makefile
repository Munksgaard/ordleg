.PHONY: elm
elm:
	npx elm make --optimize --output=public/main.js src/Main.elm

.PHONY: elm
elm-debug:
	npx elm make --debug --output=public/main.js src/Main.elm

.PHONY: elm-review
elm-review:
	npx elm-review \
	  --template jfmengels/elm-review-unused/example \
	  --ignore-dirs generated,externs \
	  --no-details

.PHONY: server
server: elm
	python -m http.server --directory public

.PHONY: server-debug
server-debug: elm-debug
	python -m http.server --directory public
