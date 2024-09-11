@echo off

echo To run D-Particle Tracking:
echo - change to the directory containing the input file
echo - type the command: run_delpar name.inp

cd /d %USERPROFILE%

set path=%~dp0\;%PATH%

start /B
exit
