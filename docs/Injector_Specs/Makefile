PDF=pdflatex
TEX=latexmkrc
BIB=bibtex

all: spec 

spec: main.tex 1-section.tex 2-section.tex 2-subsection_1.tex 2-subsection_2.tex  7-appendix.tex
	#$(TEX) main.tex; \#
	$(PDF) main.tex;  \
	$(BIB) main.tex;  \
	$(PDF) main.tex;

clean:
	rm *.aux *.log *.blg *.bbl *.out *.fls *.fdb_latexmk *.synctex.gz *.dvi *.out.ps *.toc

clear:
	rm *.aux *.log *.blg *.bbl *.out *.fls *.fdb_latexmk *.synctex.gz *.dvi *.out.ps *.toc *.pdf 

