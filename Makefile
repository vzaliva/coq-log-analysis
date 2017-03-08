
# I've tried several viewers, and most of them could not handle
# large PDFs. 'zathura' has minimalistic interface and
# pretty fast but fails to show some files.
# Keys: 'q' to exit. Arrows to move. +-= to zoom.
#PDFVIEWER = zathura --mode=fullscreen
# As a fallback you can always try good old 'xpdf'.

EXE = _build/default/parselog.exe

.PHONY: all clean distclean run run1 runt runs run1s runp run1p

all: $(EXE)

$(EXE):
	dune build

PDFVIEWER = xpdf

SVGVIEWER = geeqie -t

PNGVIEWER = geeqie -t

clean:
	dune clean
	rm -f *.run *.dot *.pdf

samples/test.dot: $(EXE)
	$(EXE) -d -v -f samples/test.txt.out -o samples/test.dot

samples/test.pdf: samples/test.dot 
	dot -Tpdf samples/test.dot -osamples/test.pdf

samples/refine.dot: $(EXE)
	$(EXE) -d -v -f samples/refine.txt.out -o samples/refine.dot

samples/refine.pdf: samples/refine.dot $(EXE)
	dot -Tpdf samples/refine.dot -osamples/refine.pdf

samples/refine.svg: samples/refine.dot
	dot -Tsvg samples/refine.dot -osamples/refine.svg

samples/refine_fail.dot: $(EXE) 
	$(EXE) -d -f samples/refine_fail.txt.out -o samples/refine_fail.dot

samples/refine_fail.pdf: samples/refine_fail.dot 
	dot -Tpdf samples/refine_fail.dot -osamples/refine_fail.pdf

samples/refine_fail.svg: samples/refine_fail.dot 
	dot -Tsvg samples/refine_fail.dot -osamples/refine_fail.svg

samples/refine.png: samples/refine.svg
	 inkscape -z -e refine.png refine.svg

samples/refine_fail.png: samples/refine_fail.svg
	 inkscape -z -e refine_fail.png refine_fail.svg

# helpers
run: samples/refine.pdf
	$(PDFVIEWER) samples/refine.pdf

runs: samples/refine.svg
	$(SVGVIEWER) samples/refine.svg

run1: samples/refine_fail.pdf
	$(PDFVIEWER) samples/refine_fail.pdf

run1s: samples/refine_fail.svg
	$(SVGVIEWER) samples/refine_fail.svg

runp:
	$(PNGVIEWER) samples/refine.png

run1p:
	$(PNGVIEWER) samples/refine_fail.png

runt: samples/test.pdf
	$(PDFVIEWER) samples/test.pdf




