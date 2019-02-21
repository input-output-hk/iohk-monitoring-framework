DOC = IOHK-Monitoring

SOURCES = ../src/Code.lhs \
					../src/Cardano/BM/BaseTrace.lhs \
					../src/Cardano/BM/Configuration.lhs \
					../src/Cardano/BM/Configuration/Model.lhs \
					../src/Cardano/BM/Configuration/Static.lhs \
					../src/Cardano/BM/Configuration/Editor.lhs \
					../src/Cardano/BM/Counters.lhs \
					../src/Cardano/BM/Counters/Common.lhs \
					../src/Cardano/BM/Counters/Dummy.lhs \
					../src/Cardano/BM/Counters/Linux.lhs \
					../src/Cardano/BM/Data/Aggregated.lhs \
					../src/Cardano/BM/Data/AggregatedKind.lhs \
					../src/Cardano/BM/Data/Backend.lhs \
					../src/Cardano/BM/Data/BackendKind.lhs \
					../src/Cardano/BM/Data/Configuration.lhs \
					../src/Cardano/BM/Data/Counter.lhs \
					../src/Cardano/BM/Data/LogItem.lhs \
					../src/Cardano/BM/Data/MonitoringEval.lhs \
					../src/Cardano/BM/Data/Observable.lhs \
					../src/Cardano/BM/Data/Output.lhs \
					../src/Cardano/BM/Data/Rotation.lhs \
					../src/Cardano/BM/Data/Severity.lhs \
					../src/Cardano/BM/Data/SubTrace.lhs \
					../src/Cardano/BM/Data/Trace.lhs \
					../src/Cardano/BM/Observer/Monadic.lhs \
					../src/Cardano/BM/Observer/STM.lhs \
					../src/Cardano/BM/Output/Aggregation.lhs \
					../src/Cardano/BM/Output/EKGView.lhs \
					../src/Cardano/BM/Output/Log.lhs \
					../src/Cardano/BM/Output/Monitoring.lhs \
					../src/Cardano/BM/Output/Switchboard.lhs \
					../src/Cardano/BM/Rotator.lhs \
					../src/Cardano/BM/Setup.lhs \
					../src/Cardano/BM/Trace.lhs \

TESTS = ../test/Test.lhs \
				../test/Cardano/BM/Test/Aggregated.lhs \
				../test/Cardano/BM/Test/Configuration.lhs \
				../test/Cardano/BM/Test/Routing.lhs \
				../test/Cardano/BM/Test/STM.lhs \
				../test/Cardano/BM/Test/Trace.lhs \
				../test/Cardano/BM/Arbitrary/Aggregated.lhs \

TeXs = aggregation.tex monitoring.tex mu-benchmarks.tex output-selection.tex requirements.tex traces.tex

ImagePdf = OverviewModules.pdf SetupProcedure.pdf ConfigurationModel.pdf

.PHONY: $(DOC).pdf all clean

all: $(DOC).pdf

$(DOC).tex: $(SOURCES) $(TESTS) $(TeXs) test-coverage.tex ../src/references.fmt
	cd ../src; lhs2TeX -v -o ../docs/$(DOC).tex Code.lhs; cd ../docs

$(DOC).pdf: $(DOC).tex $(ImagePdf)
	#latexmk -pdf -pdflatex="pdflatex -interaction=nonstopmode" -use-make $<
	latexmk -pdf -use-make $<

OverviewModules.pdf: OverviewModules.png
	convert $< $@

SetupProcedure.pdf: SetupProcedure.png
	convert $< $@

ConfigurationModel.pdf: ConfigurationModel.png
	convert $< $@

clean:
	latexmk -CA

