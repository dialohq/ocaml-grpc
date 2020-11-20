.PHONY: docs
docs:
	dune build @doc
	rm -rf docs
	mv _build/default/_doc/_html docs
