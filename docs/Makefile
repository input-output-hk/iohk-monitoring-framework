DOC = IOHK-Monitoring

SRCDIR1 = ../iohk-monitoring/src
SRCDIR2 = ../contra-tracer/src
SOURCES = Code.lhs \
					$(SRCDIR1)/Cardano/BM/Configuration.lhs \
					$(SRCDIR1)/Cardano/BM/Configuration/Model.lhs \
					$(SRCDIR1)/Cardano/BM/Configuration/Static.lhs \
					$(SRCDIR1)/Cardano/BM/Counters.lhs \
					$(SRCDIR1)/Cardano/BM/Counters/Common.lhs \
					$(SRCDIR1)/Cardano/BM/Counters/Dummy.lhs \
					$(SRCDIR1)/Cardano/BM/Counters/Linux.lhs \
					$(SRCDIR1)/Cardano/BM/Data/Aggregated.lhs \
					$(SRCDIR1)/Cardano/BM/Data/AggregatedKind.lhs \
					$(SRCDIR1)/Cardano/BM/Data/Backend.lhs \
					$(SRCDIR1)/Cardano/BM/Data/BackendKind.lhs \
					$(SRCDIR1)/Cardano/BM/Data/Configuration.lhs \
					$(SRCDIR1)/Cardano/BM/Data/Counter.lhs \
					$(SRCDIR1)/Cardano/BM/Data/LogItem.lhs \
					$(SRCDIR1)/Cardano/BM/Data/MonitoringEval.lhs \
					$(SRCDIR1)/Cardano/BM/Data/Observable.lhs \
					$(SRCDIR1)/Cardano/BM/Data/Output.lhs \
					$(SRCDIR1)/Cardano/BM/Data/Rotation.lhs \
					$(SRCDIR1)/Cardano/BM/Data/Severity.lhs \
					$(SRCDIR1)/Cardano/BM/Data/SubTrace.lhs \
					$(SRCDIR1)/Cardano/BM/Data/Trace.lhs \
					$(SRCDIR1)/Cardano/BM/Data/Tracer.lhs \
					$(SRCDIR1)/Cardano/BM/Observer/Monadic.lhs \
					$(SRCDIR1)/Cardano/BM/Observer/STM.lhs \
					$(SRCDIR1)/Cardano/BM/Backend/Aggregation.lhs \
					$(SRCDIR1)/Cardano/BM/Backend/Editor.lhs \
					$(SRCDIR1)/Cardano/BM/Backend/EKGView.lhs \
					$(SRCDIR1)/Cardano/BM/Backend/Graylog.lhs \
					$(SRCDIR1)/Cardano/BM/Backend/Log.lhs \
					$(SRCDIR1)/Cardano/BM/Backend/LogBuffer.lhs \
					$(SRCDIR1)/Cardano/BM/Backend/Monitoring.lhs \
					$(SRCDIR1)/Cardano/BM/Backend/Prometheus.lhs \
					$(SRCDIR1)/Cardano/BM/Backend/Switchboard.lhs \
					$(SRCDIR1)/Cardano/BM/Backend/TraceAcceptor.lhs \
					$(SRCDIR1)/Cardano/BM/Backend/TraceForwarder.lhs \
					$(SRCDIR1)/Cardano/BM/Rotator.lhs \
					$(SRCDIR1)/Cardano/BM/Setup.lhs \
					$(SRCDIR1)/Cardano/BM/Trace.lhs \
					$(SRCDIR2)/Control/Tracer.lhs \

TSTDIR1 = ../iohk-monitoring/test
TSTDIR2 = ../contra-tracer/test
TESTS = $(TSTDIR1)/Test.lhs \
				$(TSTDIR1)/Cardano/BM/Test/Aggregated.lhs \
				$(TSTDIR1)/Cardano/BM/Test/Configuration.lhs \
				$(TSTDIR1)/Cardano/BM/Test/Routing.lhs \
				$(TSTDIR1)/Cardano/BM/Test/STM.lhs \
				$(TSTDIR1)/Cardano/BM/Test/Structured.lhs \
				$(TSTDIR1)/Cardano/BM/Test/Trace.lhs \
				$(TSTDIR1)/Cardano/BM/Test/Tracer.lhs \
				$(TSTDIR1)/Cardano/BM/Arbitrary/Aggregated.lhs \

EXAMPLES = ../iohk-monitoring/examples/complex/Main.lhs ../iohk-monitoring/examples/simple/Main.lhs

TeXs = aggregation.tex configuration.tex monitoring.tex mu-benchmarks.tex output-selection.tex requirements.tex traces.tex

ImagePdf = OverviewModules.pdf SetupProcedure.pdf ConfigurationModel.pdf ConfigEditor.pdf

.PHONY: $(DOC).pdf all clean

all: $(DOC).pdf

$(DOC).tex: $(SOURCES) $(TESTS) $(EXAMPLES) $(TeXs) references.fmt
	lhs2TeX -v -o $(DOC).tex Code.lhs

$(DOC).pdf: $(DOC).tex $(ImagePdf)
	#latexmk -pdf -pdflatex="pdflatex -interaction=nonstopmode" -use-make $<
	latexmk -pdf -use-make $<

OverviewModules.pdf: OverviewModules.png
	convert $< $@

SetupProcedure.pdf: SetupProcedure.png
	convert $< $@

ConfigurationModel.pdf: ConfigurationModel.png
	convert $< $@

ConfigEditor.pdf: ConfigEditor.png
	convert $< $@

clean:
	latexmk -CA

