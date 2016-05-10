SET WORKDIR=%~dp0
cd %WORKDIR%

where plink.exe > tmpFile
set /p GIT_SSH= < tmpFile
del tmpFile

:again
git pull origin master
if errorlevel 1 goto :err
exit 0

:err
echo Error, trying again in 5 sec...
sleep 5
goto :again
exit
