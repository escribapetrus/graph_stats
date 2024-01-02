build:
	mkdir -p _build
	erlc -o _build concurrent/*.erl

run: build
	cd _build && erl

clean:
	rm -rf _build
