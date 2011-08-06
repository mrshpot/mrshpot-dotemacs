IF NOT DEFINED EMACS (
    IF EXIST c:\emacs-23.3\bin\runemacs.exe (
        SET EMACS=c:\emacs-23.3\bin\runemacs.exe
    ) ELSE (
        ECHO The EMACS environment variable is not set, could not find Emacs.
        pause
        exit /B 1
    )
)

set HOME=%~dp0
start %EMACS% || pause
