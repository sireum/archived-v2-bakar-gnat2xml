for 64 bit placed gnat-pro in /home/super/packages/gnat/gnat-7.2.0w-x86_64-pc-linux-gnu-bin

  export GNAT_HOME=/home/super/packages/gnat/gnat-7.2.0w-x86_64-pc-linux-gnu-bin

for 32 bit placed gnat-pro in /home/super/packages/gnat/gnat-7.2.0w-i686-linux-libc2.3-bin

  export GNAT_HOME=/home/super/packages/gnat/gnat-7.2.0w-i686-linux-libc2.3-bin

cd'ed into asis-src dir and
 
  ~/packages/gnat/asis-7.2-0w-src$ export PATH=$GNAT_HOME/bin/:$PATH:.
  ~/packages/gnat/asis-7.2-0w-src$ make all install prefix=$GNAT_HOME

couldn't find crt1.o so had to ...

  sudo apt-get update
  sudo apt-get install --reinstall binutils
  sudo apt-get install --reinstall build-essential
  sudo apt-get install --reinstall libc6-dev
  sudo apt-get install --reinstall gcc-4.6
  sudo apt-get install --reinstall libboost1.46-all-dev

for 64bit

  export LIBRARY_PATH=/usr/lib/x86_64-linux-gnu

for 32 bit

  export LIBRARY_PATH=/usr/lib/i386-linux-gnu

build succeeded so cd'ed into gnat2xml-src dir,

  ~/packages/gnat/gnat2xml-7.2.0w-src$ export ADA_INCLUDE_PATH=$GNAT_HOME/include:$GNAT_HOME/include/asis:$GNAT_HOME/include/gnat_util
  ~/packages/gnat/gnat2xml-7.2.0w-src$ make all test

complained that xmllint couldn't be found so

  sudo apt-get install libxml2-utils

then couldn't find dom.ads (and others) so had to build xmlada from source (retrieved from wavefront)

  ~/packages/gnat/xmlada-4.4w-src$ ./configure --prefix=$GNAT_HOME
  ~/packages/gnat/xmlada-4.4w-src$ make all install

cd'ed into into gnat2xml dir

  ~/packages/gnat/gnat2xml-7.2.0w-src$ export ADA_INCLUDE_PATH=$GNAT_HOME/include:$GNAT_HOME/include/asis:$GNAT_HOME/include/gnat_util:$GNAT_HOME/include/xmlada
  ~/packages/gnat/gnat2xml-7.2.0w-src$ make all test 

build succeeded. For linux32 (similar for 64)

  ~/packages/gnat/gnat2xml-7.2.0w-src$ cp gnat2xml xml2gnat gnat2xsd ~/packages/gnat/gnat-7.2.0w-i686-gnu-linux-libc2.3-bin/bin/
  ~/packages/gnat/gnat2xml-7.2.0w-src$ cd ..
  ~/packages/gnat$ tar cfvz ksu-gnat-bundle-20121129-i686-32-linux.tar.gz gnat-7.2.0w-i686-gnu-linux-libc2.3-bin

