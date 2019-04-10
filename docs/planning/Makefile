
SOURCES = milestones-logging.dot milestones-shell.dot

OUTPUTS = milestones-logging.png milestones-logging.pdf milestones-shell.png milestones-shell.pdf

%.png: %.dot
	dot -Tpng $< -o$@
%.pdf: %.dot
	dot -Tpdf $< -o$@

all: $(OUTPUTS) $(SOURCES)

