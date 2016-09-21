@echo off
REM ****************** create test2_clean
START /WAIT python processToCSV.py
REM ****************** create outputs 

REM post outputs to github
cd outputs\AITweets
START /WAIT git add test2_clean.csv
START /WAIT R CMD BATCH AI.R

START /WAIT git add .

set "str1=Processed on "
set "str2=%date% %time%"
set "str3=%str1%%str2%"
echo.%str3%  
START /WAIT git commit -m "%str3%"
START /WAIT git push XXXXXX master
 
cd ..\..
