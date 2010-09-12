#! /bin/bash

pdflatex refman.tex
if [ $? -ne 0 ]; then
    echo "pdflatex failed"
    exit 1
fi

makeindex refman.idx
if [ $? -ne 0 ]; then
    echo "makeindex failed"
    exit 1
fi

pdflatex refman.tex
if [ $? -ne 0 ]; then
    echo "pdflatex failed"
    exit 1
fi

latex_count=5;
while egrep -s 'Rerun (LaTeX|to get cross-references right)' refman.log && [ $$latex_count -gt 0 ]; do
    echo "Rerunning latex...." ;
    pdflatex refman.tex ;
    latex_count=`expr $$latex_count - 1` ;
done
