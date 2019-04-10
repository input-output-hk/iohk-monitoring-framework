
SOURCES = milestones-logging.dot

OUTPUTS = milestones-logging.png milestones-logging.pdf

%.png: %.dot
	dot -Tpng $< -o$@
%.pdf: %.dot
	dot -Tpdf $< -o$@

all: $(OUTPUTS) $(SOURCES)

