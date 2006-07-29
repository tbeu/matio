#! /bin/bash
mkdir sed
for f in `ls *.tex`;
do
# cat $f | sed "/generated from the following file/,/hyperlink/d" | sed "s/by Doxygen/ /g" | sed "s/Modules/Library/" | sed "s/Module/Library/" | sed "/subsection*Files/,/subsection/{ /subsection*Files/b /subsection/b s/^/>>/ }" > sed/$f
cat $f | sed "/generated from the following file/,/hyperlink/d" | sed "s/by Doxygen/ /g" | sed "s/Modules/Library/" | sed "s/Module/Library/" | sed -e "/subsection.*Files/,/subsection/{ /subsection.*Files/d; /subsection/p; /.*/d }" > sed/$f
done
mv sed/* .
rm -fr sed
cp refman.tex refman.tex.save
cp doxygen.sty doxygen.sty.save
cat refman.tex | sed "/File Index/,/input{files}/d" | sed "/File Documentation/,/include/d" | sed "/8c/d" | sed "/8h/d" > refman.sed
cat doxygen.sty | sed "s/by Doxygen//g" > doxygen.sed
mv refman.sed refman.tex
mv doxygen.sed doxygen.sty
