PDF=pdflatex
TEX=latexmk

all: spec 

spec: main.tex 1-module_description.tex 2_1-drivers.tex 2_2-baremetal.tex 2-software.tex 7-appendix.tex 
	$(TEX) main.tex; \
	$(PDF) main.tex

clean:
	rm *.aux *.log *.blg *.bbl *.out *.fls *.fdb_latexmk *.synctex.gz *.dvi *.out.ps *.toc

clear:
	rm *.aux *.log *.blg *.bbl *.out *.fls *.fdb_latexmk *.synctex.gz *.dvi *.out.ps *.toc *.pdf 

