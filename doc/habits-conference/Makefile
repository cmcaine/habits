end.pdf:
	pandoc -f markdown-implicit_figures end.md -t beamer -o end.tex -s \
		&& lualatex end.tex >/dev/null && rm end.{aux,nav,snm,toc,out,log}
