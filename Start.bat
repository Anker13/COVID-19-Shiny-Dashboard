@echo off
echo Was wollen sie tun?
echo update        =  Starten des Servers MIT Datenupdate  
echo startServer   =  Starten des Servers OHNE Datenupdate 
set /p input=""
cls

StartShinyServer.bat %input%
