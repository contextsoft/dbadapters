@echo off

cd %1

mkdir ..\-releases\%2

pkzip25 -add ..\-releases\%2\%1-bin-%2.zip adapter\*.*

pkzip25 -add -rec -path ..\-releases\%2\%1-src-%2.zip readme.txt adapter\source\*.* vcl\*.*

cd ..