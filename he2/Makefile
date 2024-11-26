
.PHONY: compile clean

output = handout.pdf

temporary =            \
	*.cut          \
	*.aux          \
	*.out          \
	*.synctex.gz   \
	*.latexmk      \
	*.bbl          \
	*.blg          \
	*.log _minted* \
	*.lot          \
	*.lof          \
	*.toc          \
	*.idx          \
	*.fls          \
	*latexmk


TeXMake = latexmk -synctex=1 -shell-escape -pdf

compile: clean $(output)

%.pdf:
	$(TeXMake) $(basename $@).tex

clean:
	rm -rf  $(output) $(temporary)


