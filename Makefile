R_OPTS = --vanilla
fname = ct-covid
rmd = $(fname).Rmd
source = $(fname).R

date:
	date.exe "+%Y%m%d"

### building reports

html: 
	R $(R_OPTS) -e "rmarkdown::render('"$(rmd)"', output_format='bookdown::html_document2')"

pdf: 
	R $(R_OPTS) -e "rmarkdown::render('"$(rmd)"', output_format='bookdown::pdf_document2')"


figs: # generate figures only
	R $(R_OPTS) -e "base::source('"$(source)"')"


html2web: # make html page suitable for web
	sed '/Covid-19 in Connecticut/ r gtag.js' < $(fname).html > tmp0.html ## insert google analytics tag
	sed '/Covid-19 in Connecticut/ r html-meta.txt' < tmp0.html > index.html ## insert meta tags
	mv --backup index.html ./docs/.
	rm -f tmp0.html

publish: # push webpage updates
	git commit -a -m "update webpage"
	git push --all

### cleaning up

nocache: # delete cache files
	rm -rf $(fname)_cache $(fname)_files

tidy: # delete intermediate files
	rm -f $(fname).aux $(fname).log $(fname).out
	rm -f $(fname).tex $(fname).toc
	rm -f $(fname).md $(fname).knit.md $(fname).utf8.md

noreports: # delete formatted reports
	rm -f $(fname).html $(fname).pdf

clean:
	make nocache
	make tidy
	make noreports
