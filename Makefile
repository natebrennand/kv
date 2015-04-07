NAME = report

default:
	pdflatex $(NAME).tex
	bibtex $(NAME).aux
	pdflatex $(NAME).tex

clean:
	latexmk -CA
