@ECHO off
%COMSPEC% /C "..\..\bin\ml-build -DNO_ML_ANTLR -DNO_ML_LEX -DNO_ML_YACC sources.cm Main.main ml-ulex"
%COMSPEC% /C "..\..\bin\sml -m $smlnj/library-install.cm tool/ml-ulex-tool.cm ..\..\lib"
