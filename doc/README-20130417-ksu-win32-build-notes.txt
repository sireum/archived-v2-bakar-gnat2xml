PREPARATION
======================

Install MinGW and add to path: http://sourceforge.net/projects/mingw/files/Installer/mingw-get-inst/
Install MinGW shell trough cmd (mingw-get.exe is in c:/MinGW/bin - if c:/MinGW/bin is in path execute below command in cmd): 
	mingw-get install msys-dvlpr

Install GNATPRO and add to path.


INSTALATION
======================

--in cmd:

SET GNAT_HOME=C:/GNATPRO/7.2.0w-20130331

SET PATH=C:\GNATPRO\7.2.0w-20130331\bin;C:\MinGW\bin;C:\MinGW\msys\1.0\bin;C:\Windows\system32;C:\Windows;C:\Windows\System32\Wbem;C:\Windows\System32\WindowsPowerShell\v1.0\;C:\GtkAda\bin

cd into asis-7.2.0w-src

gnatmake -j0 -Pasis

gprinstall -p --prefix=%GNAT_HOME% asis.gpr

--in MinGW shell (run by c:\MinGW\msys\1.0\msys.bat):

export GNAT_HOME=/c/GNATPRO/7.2.0w-20130331

export PATH=/c/GNATPRO/7.2.0w-20130331/bin:/c/MinGW/bin:/c/MinGW/msys/1.0/bin:/c/Windows/system32:/c/Windows:/c/Windows/System32/Wbem:/c/Windows/System32/WindowsPowerShell/v1.0/:/c/GtkAda/bin:/c/MinGW/bin:/c/MinGW/msys/1.0/bin:/c/GNATPRO/7.2.0w-20130331/bin

cd /c/asis-7.2.0w-src/tools/gnat2xml

make all test

************************
This section seems to be not required for last version...

cd /c/MinGW/msys/1.0/home/sireum/libxml2-2.8.0

./configure --without-threads --prefix=/c/opt

make

make install

export PATH=/c/opt/bin:$PATH

cd /c/asis-7.2.0w-src/tools/gnat2xml/

make all test

************************

cp gnat2xml.exe $GNAT_HOME/bin

cp gnat2xsd.exe $GNAT_HOME/bin

cp xml2gnat.exe $GNAT_HOME/bin
