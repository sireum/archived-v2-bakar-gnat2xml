Build Gnat2XML on MAC 64
========================

1.
  ``export GNAT_HOME=/home/super/packages/gnat/gnat-7.2.0w-bin``

2. cd'ed into asis-src dir and
 
   * ~/packages/gnat/asis-7.2-0w-src$ ``export PATH=$GNAT_HOME/bin/:$PATH:.``
   * ~/packages/gnat/asis-7.2-0w-src$ ``make all install prefix=$GNAT_HOME``

3. build succeeded so cd'ed into gnat2xml-src dir,

   * ~/packages/gnat/gnat2xml-7.2.0w-src$ ``export ADA_INCLUDE_PATH=$GNAT_HOME/include:$GNAT_HOME/include/asis:$GNAT_HOME/include/gnat_util``
   * ~/packages/gnat/gnat2xml-7.2.0w-src$ ``make all test``

4. build succeeded.

   * ~/packages/gnat/gnat2xml-7.2.0w-src$ ``cp gnat2xml xml2gnat gnat2xsd ~/packages/gnat/gnat-7.2.0w-bin/bin/``
   * ~/packages/gnat/gnat2xml-7.2.0w-src$ ``cd ..``
   * ~/packages/gnat$ ``tar cfvz ksu-gnat-bundle-20130411-macos.tar.gz gnat-7.2.0w-bin``
