@ECHO OFF


ECHO used command: %1

IF %1%==update (
LoadData.bat
)

IF %1%==startServer (
"C:\Program Files\R\R-3.6.3\bin\Rscript.exe"  RunShinyServer.R
)



