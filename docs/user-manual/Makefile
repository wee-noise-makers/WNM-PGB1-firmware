OUT_DIR = build
SPHINX_CONF    := .
SPHINX_SRC     := source
SPHINX_HTML    := $(OUT_DIR)/html
JEKYLL_HTML    := $(OUT_DIR)/jekyll
SPHINX_PDF_DIR := $(OUT_DIR)/pdf
PYTHON         := python3

VERSION  := $(shell python3 -c "exec(open('conf.py').read()); print(release)")
PDF_FILE := pgb1-user-manual$(if $(VERSION),-$(VERSION),).pdf

.PHONY: all sphinx-html sphinx-pdf clean help setup

all: sphinx-pdf sphinx-html jekyll-html

sphinx-html: sphinx-pdf $(SPHINX_SRC)/index.md
	sphinx-build -b html -c $(SPHINX_CONF) $(SPHINX_SRC) $(SPHINX_HTML)
	cp $(SPHINX_PDF_DIR)/$(PDF_FILE) $(SPHINX_HTML)/$(PDF_FILE)
	@echo "Sphinx HTML built: $(SPHINX_HTML)/index.html"

jekyll-html: sphinx-html
	# Rename underscore dirs so Jekyll doesn't ignore them
	rm -rf $(JEKYLL_HTML)
	cp -r $(SPHINX_HTML) $(JEKYLL_HTML)
	mv $(JEKYLL_HTML)/_static  $(JEKYLL_HTML)/static
	mv $(JEKYLL_HTML)/_images  $(JEKYLL_HTML)/images
	find $(JEKYLL_HTML) -name "*.html" -o -name "*.js" | \
	    xargs sed -i 's|_static/|static/|g; s|_images/|images/|g'

sphinx-pdf: $(SPHINX_SRC)/index.md
	sphinx-build -b latex -c $(SPHINX_CONF) $(SPHINX_SRC) $(SPHINX_PDF_DIR)
	cd $(SPHINX_PDF_DIR) && latexmk -pdf -interaction=nonstopmode pgb1-user-manual.tex
	mv $(SPHINX_PDF_DIR)/pgb1-user-manual.pdf $(SPHINX_PDF_DIR)/$(PDF_FILE)
	@echo "Sphinx PDF built: $(SPHINX_PDF_DIR)/$(PDF_FILE)"

setup:
	$(PYTHON) -m venv .venv
	.venv/bin/pip install -r requirements.txt

clean:
	rm -rf $(OUT_DIR)
