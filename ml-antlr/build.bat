@ECHO off
%COMSPEC% /C "..\..\bin\ml-build sources.cm Main.main ml-antlr"
%COMSPEC% /C "..\..\bin\sml -m $smlnj/library-install.cm tool/ml-antlr-tool.cm ..\..\lib"
