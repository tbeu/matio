#! /bin/bash

pdflatex refman.tex
makeindex refman.idx
pdflatex refman.tex

latex_count=5;
while egrep -s 'Rerun (LaTeX|to get cross-references right)' refman.log && [ $$latex_count -gt 0 ]; do
    echo "Rerunning latex...." ;
    pdflatex refman.tex ;
    latex_count=`expr $$latex_count - 1` ;
done
