render:
	Rscript --quiet -e "rmarkdown::render('index.Rmd')"

all:
	make render && make serve

moon:
	Rscript --quiet -e "xaringan::inf_mr('.')"

serve:
	Rscript --quiet -e "servr::httw(dir = '.', pattern = 'html|css|js', port = 9090, browser = FALSE, daemon = FALSE)"
	
clean:
	rm -rf index.html index_files
