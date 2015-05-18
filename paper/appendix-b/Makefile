paper:
	xelatex appendix-b

all: bib

bib:
	xelatex appendix-b
	bibtex  appendix-b
	xelatex appendix-b
	xelatex appendix-b

clean:
	rm -f appendix-b.aux
	rm -f appendix-b.bbl
	rm -f appendix-b.blg
	rm -f appendix-b.log
	rm -f appendix-b.pdf
	rm -f appendix-b.out
