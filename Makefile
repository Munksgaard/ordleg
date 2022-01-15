.PHONY: elm
elm:
	npx elm make --optimize --output=public/main.js src/Main.elm

.PHONY: elm-review
elm-review:
	npx elm-review \
	  --template jfmengels/elm-review-unused/example \
	  --ignore-dirs generated,externs \
	  --no-details
