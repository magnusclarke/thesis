# Combine all bibs into lit.bib
rm lit.bib 
cat *.bib > lit.bib

# Tidy up chars which mess up latex
sed 's_%_\\%_g' lit.bib -i
sed 's_&_\\&_g' lit.bib -i

# Generate pdf
pdflatex -interaction batchmode *.tex
bibtex *.aux
pdflatex -interaction batchmode *.tex
pdflatex -interaction batchmode *.tex
