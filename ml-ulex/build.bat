@ECHO off
%COMSPEC% /C "..\..\bin\ml-build sources.cm Main.main ml-ulex"
%COMSPEC% /C "..\..\bin\sml -m $smlnj/library-install.cm tool/ml-ulex-tool.cm ..\..\lib"
