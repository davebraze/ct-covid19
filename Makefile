R_OPTS = --vanilla
fname = ct-covid
# rmd = .Rmd
source = $(fname).R

date:
	date.exe "+%Y%m%d"
	echo "base::source('"$(source)"')"

### build figures only

figs:
	R $(R_OPTS) -e "base::source('"$(source)"')"

### building reports

# html:
# 	R $(R_OPTS) -e "rmarkdown::render('"$(source)"', output_format='bookdown::html_document2')"

# pdf:
# 	R $(R_OPTS) -e "rmarkdown::render('"$(source)"', output_format='bookdown::pdf_document2')"

# docx:
# 	# make tidy nocache
# 	R $(R_OPTS) -e "rmarkdown::render('"$(source)"', output_format='bookdown::word_document2')"

# all:
# 	make html
# 	make docx
# 	make pdf

### cleaning up

nocache:
	# delete cache files
	rm -rf $(fname)_cache $(fname)_files

tidy:
	# delete intermediate files
	rm -f $(fname).aux $(fname).log $(fname).out
	rm -f $(fname).tex $(fname).toc
	rm -f $(fname).md $(fname).knit.md $(fname).utf8.md

noreports:
	# delete formatted reports
	rm -f $(fname).html $(fname).pdf $(fname).docx

clean:
	make nocache
	make tidy
	make noreports
