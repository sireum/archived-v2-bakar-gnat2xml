Gnat2xml Installation
#####################

Preparation
***********

1. Download the latest wavefront version of ASIS and GNAT Pro from AdaCore.
  
  The versions we used for this documentation are:
  
  * ASIS: asis-7.2.0w-20130331-src
  
  * GNAT PRO
    
    - Windows: gnatpro-7.2.0w-20130331-47-i686-pc-mingw32-bin

2. Download the customized Gnat2XML from KSU SAnToS's 
   `open source project https://www.assembla.com/code/sireum-bakar/git-2/nodes>

Build Gnat2XML On Windows 64
****************************
  
1. run ``gnatpro-7.2.0w-20130331-47-i686-pc-mingw32-bin`` and
   install GNAT PRO at some location, for example: C:\GNATPRO\7.2.0w-20130331. 
  
2. download and install ``mingw-get-inst-20120426.exe`` (for example, in C:\MinGW) 
   via http://sourceforge.net/projects/mingw/files/Installer/mingw-get-inst/
   
   Install MinGW shell trough cmd (mingw-get.exe is in C:\MinGW\bin): 
   
   * > cd C:\MinGW\bin
   * > mingw-get install msys-dvlpr 
   
3. install ASIS (if ASIS_DIR is asis-7.2.0w-src directory)

   * > cd ASIS_DIR
   * > use KSU customized Gnat2XML to replace the one in ASIS_DIR\tools\gnat2xml
   * > set GNAT_HOME=C:/GNATPRO/7.2.0w-20130331
   * > gnatmake -j0 -Pasis 
   * > gprinstall -p --prefix=%GNAT_HOME% asis.gpr  (Note: it's double dash before option ``prefix``)

4. install gnat2xml

  * download libxml and its dependencies from ftp://ftp.zlatkovic.com/libxml/, 
    and unzip them under a directory, let's call it ``LIBXML_DIR``
    
    - iconv-1.9.2.win32.zip
    - libxml2-2.7.8.win32.zip
    - zlib-1.2.5.win32.zip
    
    
  * run MinGW shell (by C:\MinGW\msys\1.0\msys.bat), and then type the following commands:
  * $ cd ASIS_DIR/tools/gnat2xml
  * $ export PATH=/c/GNATPRO/7.2.0w-20130331/bin:/c/MinGW/bin:/c/MinGW/msys/1.0/bin:
    LIBXML_DIR/libxml2-2.7.8/bin:LIBXML_DIR/iconv-1.9.2.win32/bin:LIBXML_DIR/zlib-1.2.5/bin:$PATH
    
    (It seems that ``/c/MinGW/bin`` and ``/c/MinGW/msys/1.0/bin`` should be put 
    after ``/c/GNATPRO/7.2.0w-20130331/bin``. Otherwise, there will be errors)
  
  * build Gnat2xml by
 
    $ make all test
  
     - ``all`` will build two executables, gnat2xsd and gnat2xml
     - ``test`` will run gnat2xml on some test cases, validate the output 
       using ``xmllint``, and compare against expected results
     - ``xmllint`` program is included in ``libxml2``, that's why we have to set up
       the PATH environment variable with libxml
  
  
  * KSU has made the following changes to the original Gnat2XML source code to run our developed tools:   
    
    - for xsd type of ``Source_Location``, all its fields type are changed from original
      ``xsd:positiveInteger`` to current ``xsd:int``
    - for xsd type of ``pragmas_group``, all its ``element name`` are changed to ``element ref`` 
  
    So after compilation for Gnat2xml, only the following error should show up: 
    
      ``diff -q ada-schema.xsd ada-schema.xsd.new``
    
      ``Files ada-schema.xsd and ada-schema.xsd.new differ``
    
      ``make: *** [test-gnat2xml] Error 1``
  
    The difference between these two files, ``ada-schema.xsd`` and ``ada-schema.xsd.new``, can
    be checked through the following commands:
      
      - > dos2unix.exe -n result-file-name result-file-unix-format-name 
        
        (for example: ``dos2unix.exe -n ada-schema.xsd.new ada-schema.xsd.unix``)
      
      - > diff expected-file-name result-file-unix-format-name 
        
        (for example: ``diff ada-schema.xsd ada-schema.xsd.unix``)
    
  * put the compiled results under directory GNAT_HOME
  
    - $ export GNAT_HOME=/c/GNATPRO/7.2.0w-20130331
    - $ cp gnat2xml.exe $GNAT_HOME/bin
    - $ cp gnat2xsd.exe $GNAT_HOME/bin
    - $ cp xml2gnat.exe $GNAT_HOME/bin

  * to run KSU developed tools, add gnat2xml to environment variable PATH 
  
    - $ export PATH=$GNAT_HOME/bin:$PATH