R --vanilla < download.R ## fetches .html and .svg files
python svg2ps.py data/*.svg ## converts .svg to .ps
R --vanilla < trace.R ## converts .ps to .xml
R --vanilla < plotflags.R ## reads .xml files and plots them to pdf
xpdf Rflags.pdf