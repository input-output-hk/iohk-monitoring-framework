.PHONY: all clean

all: logging benchmarking monitoring store_pdfs remove_artifacts

logging: tex/logging.tex
	latexmk -pdfxe $<

benchmarking: tex/benchmarking.tex
	latexmk -pdfxe $<

monitoring: tex/monitoring.tex
	latexmk -pdfxe $<

store_pdfs:
	mv *.pdf pdf/

remove_artifacts:
	rm -f *.aux *.fls *.xdv *.out *.log *.fdb_latexmk

clean:
	latexmk -CA
