Build Gnat2XML on Windows 32
============================

1. installed mingw-get-inst-20120426.exe via http://sourceforge.net/projects/mingw/files/Installer/mingw-get-inst/

2. installed gnatpro at C:\GNATPRO\7.2.0w-20121129.  Under command shell, cd'ed into asis-7.2.0w-src directory

   * ``gnatmake -j0 -Pasis``
   * ``gprinstall -p --prefix=%GNAT_HOME% asis.gpr``

   Windows warned that gprinstall didn't install correctly but ignored that. cd'ed into gnat2xml-7.2.0w-src 
     
     * ``make all test``

   Couldn't find xmllint.exe so downloaded libxml2-2.8.0 via ftp://xmlsoft.org/libxml2/libxml2-2.8.0.tar.gz.  Win32 install instruction for libxml2 didn't work.  Instead, opened mingw shell

   * ``mingw-get install libtool``

   copied libxml2 source to C:\MinGW\msys\1.0\home\sireum\libxml2-2.8.0 and cd'ed into it.

   * ``configure --without-threads --prefix=c:/opt``

   ignored warning /bin/rm: cannot lstat 'libtoolT': No such file or directory

   * ``make`` 
   * ``make install``

3. Went back to command shell.  Added C:\opt\bin to PATH. cd'ed into gnat2xml-7.2.0w-src

   * ``make all test``

   test failed but this is due to the line endings being different.

4. Package the resutls

   * ``cp gnat2xml.exe %GNAT_HOME%\bin``
   * ``cp gnat2xsd.exe %GNAT_HOME%\bin``
   * ``cp xml2gnat.exe %GNAT_HOME%\bin``
   * zipped up c:\GNATPRO


5. Contents of final config.bat used when building gnat2xml

   * set GNAT_HOME=C:\GNATPRO\7.2.0w-20121129
   * set MINGW_HOME=C:\MinGW
   * set ASIS_HOME=%GNAT_HOME%
   * set ADA_INCLUDE_PATH=%GNAT_HOME%\include;%GNAT_HOME%\include\gnat_util;%ASIS_HOME%\include\asis
   * set XML_LINT=C:\opt\bin
   * set PATH=%GNAT_HOME%\bin;%MINGW_HOME%\bin;%MINGW_HOME%\msys\1.0\bin;%XML_LINT%;%PATH%
