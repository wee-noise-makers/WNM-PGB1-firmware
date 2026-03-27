INPUT_FILES=$(wildcard *.png)
OUTPUT_DIR=framed
FRAMED_FILES=$(patsubst %.png,$(OUTPUT_DIR)/%-framed.png,$(INPUT_FILES)) 

ZOOM_INPUT_FILES=$(wildcard zoom/*.tex)
ZOOM_OUTPUT_FILES=$(ZOOM_INPUT_FILES:.tex=.png)

$(OUTPUT_DIR)/%-framed.png: %.png
	convert -size 138x74 xc:transparent \
		-fill black -stroke grey -draw "roundRectangle 0,0 137,73 6,6"\
	         \( \( -gravity center  $< \) \( -fill "#0099ff" -opaque	black \) \( -fill black -opaque white \) \) \
		-composite $@

%.pdf: %.tex
	pdflatex -output-directory $(shell dirname $@)  $< 

%.png: %.pdf
	pdfcrop $< $<
	convert -density 300 $< -quality 90 $@

all: $(FRAMED_FILES) $(ZOOM_OUTPUT_FILES)
