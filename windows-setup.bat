REM run from command prompt
setx HOME %USERPROFILE%
setx ChocolateyInstall "C:\ProgramData\Chocolatey

@powershell -NoProfile -ExecutionPolicy unrestricted -Command "iex ((new-object net.webclient).DownloadString('https://chocolatey.org/install.ps1'))" && SET PATH=%PATH%;%ALLUSERSPROFILE%\chocolatey\bin

cinst git.commandline 7zip.commandline emacs
cinst git-credential-winstore -ignoreDependencies