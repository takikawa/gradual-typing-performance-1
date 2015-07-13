paper:
	xelatex paper

all: bib

bib:
	xelatex paper
	bibtex  paper
	xelatex paper
	xelatex paper

appendix:
	xelatex appendix-b
	bibtex  appendix-b
	xelatex appendix-b
	xelatex appendix-b

clean:
	rm -f *.aux
	rm -f *.bbl
	rm -f *.blg
	rm -f *.log
	rm -f *.pdf
	rm -f *.out
