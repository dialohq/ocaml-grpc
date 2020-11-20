.PHONY: docs
docs:
	dune build @doc
	mv _build/default/_doc/_html docs
