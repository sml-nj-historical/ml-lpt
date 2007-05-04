@ECHO off
%COMSPEC% /C "..\..\bin\ml-build -D NO_ML_ANTLR -D NO_ML_LEX -D NO_ML_YACC sources.cm Main.main ml-ulex"
%COMSPEC% /C "..\..\bin\sml -m $smlnj/library-install.cm tool/ml-ulex-tool.cm ..\..\lib"
