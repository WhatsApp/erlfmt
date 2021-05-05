gen: clean
	python3.8 columns.py > columns.html
	python3.8 types.py > types.md
	./module_comments.sh > module_comments.md

clean:
	rm columns.html || true
	rm types.md || true
	rm module_comments.md || true