dot_files  = $(wildcard *.dot)
svg_files  = $(dot_files:%.dot=%.svg)
png_files  = $(dot_files:%.dot=%.png)
pdf_files  = $(dot_files:%.dot=%.pdf)

layout = dot

.PHONY: all clean
.DEFAULT: all

all: $(patsubst %.dot,%.svg,$(wildcard *.dot))

%.svg: %.dot
	$(layout) -Tsvg $< -o $@

%.png: %.dot
	$(layout) -Tpng $< -o $@

%.pdf: %.dot
	$(layout) -Tpdf $< -o $@

clean:
	-rm -f $(svg_files) $(png_files) $(pdf_files)
