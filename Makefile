export PATH := $(shell npm bin):$(PATH)

HTML_MINIFY=--remove-comments --collapse-whitespace

NODE_SASS=--include-path=node_modules/normalize.css

.PHONEY: all serve

all: build

clean:
	rm -rf build assets/*.css

build/%.html: %.html
	mkdir -p build
	html-minifier $(HTML_MINIFY) -o $@ $<

build/%.svg: %.svg
	mkdir -p build
	html-minifier $(HTML_MINIFY) -o $@ $<

build/assets/%.css: assets/%.scss
	mkdir -p build/assets
	node-sass $(NODE_SASS) --output-style compressed $< > $@

build: build/index.html build/assets/main.css build/assets/sprites.svg
	touch build

serve:
	node-sass --watch --recursive --output assets $(NODE_SASS) assets & \
	http-server . & \
	wait
