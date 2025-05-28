all:
	pdflatex --shell-escape pgb1-safety-instructions.tex
	pdfjam --outfile pgb1-safety-instructions-multi.pdf --nup '1x2'	--duplicatepages=2 --landscape=false--angle=0 -- pgb1-safety-instructions.pdf
