@ECHO off
%COMSPEC% /C "..\..\bin\ml-build -DNO_ML_ULEX -DNO_ML_ANTLR sources.cm Main.main ml-antlr"
%COMSPEC% /C "..\..\bin\sml -m $smlnj/library-install.cm tool/ml-antlr-tool.cm ..\..\lib"
