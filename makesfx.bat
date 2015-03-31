REM The following comamnd works if you remove the last 13 bytes of the
REM assembled untgz.exe file.  It can be useful for older versions of DOS
REM which don't allow a program to find out its own execution path.
REM copy/b untgz.exe+noname.txt+%1.tgz %1.exe
copy/b untgz.exe+%1.tgz %1.exe
