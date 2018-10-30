
fxp - The Functional XML Parser
===============================

#### A validating XML parser and manipulation library written in Standard ML

fxp is a validating XML parser, written completely in the functional
programming language SML. It has a programming interface allowing for
production of XML applications based on fxp.

fxp was written by Andreas Neumann, University of Trier (neumann (AT)
psi.uni-trier.de) and Alexandru Berlea, TU Munich (berlea (AT)
in.tum.de). This repository also contains patches collected by Timothy
Bourke (see http://www.tbrk.org/software/sml-fxp.html) and a few other
fixes to improve compatibility with more recent compiler systems.


Example applications
--------------------

The following example applications are included:
  
 * `fxp` - The pure parser. It parses a document and finds
   well-formedness errors, validity errors and other problems;

 * `fxcanon` - Produces an equivalent canonical XML
   document. Canonical XML was invented by James Clark for testing XML
   parsers. It contains only the information a processor is required
   to pass to the application;

 * `fxcopy` - Reproduces the document parsed by fxp. The copy can be
   generated in a different encoding than the input, and can be
   normalized in different ways concerning, e.g., expansion of entity
   references;

 * `fxesis` - Produces an output similar to nsgmls's ESIS (Element
   Structure Information Set) output;

 * `fxviz` - An XML tree visualizer. It produces a graph description
   suitable as input to Georg Sander's vcg.


Programming Interface
---------------------

fxp's API is described in `doc/api.ps`.


Compiling fxp
-------------

#### With MLton

The MLB file `src/fxlib.mlb` lists the library sources, and each of
the command-line utilities has its own `.mlb` file. So to build, for
example, the `canon` utility:

```
$ cd src/Apps/Canon
$ mlton canon.mlb
```


#### With SML/NJ

Compilation Manager (CM) files are provided, as well as a `Makefile`
that invokes them. The `Makefile` is retained from the original fxp
sources and may not be current. See the section "Historical build
instructions" below for more details.


#### With Poly/ML

For each of the MLB build files, a corresponding file `poly-*.ML` is
provided that pulls in the same sources via `use` declarations. So to
build, for example, the `canon` utility:

```
$ cd src/Apps/Canon
$ polyc -o canon poly-canon.ML
```


Copyright
---------

Copyright 1999-2004 Andreas Neumann and Alexandru Berlea. See the file
`COPYRIGHT` for more information about licensing.


Historical build instructions
-----------------------------

These are the steps described in the original fxp distribution for
installing fxp under Unix:

```
    1. Download the latest version of fxp; 
    2. Unpack the sources, and change to the fxp directory, e.g.: 

         gunzip -c fxp-2.0.tar.gz | tar xf -
         cd fxp-2.0

    3. Read the COPYRIGHT; 
    4. Edit the Makefile according to your needs. Probably you will only have 
       to change the following: 

         INSTALL_PROGS is the list of programs to be installed. fxlib is only
                       required if you want to develop applications with fxp. 

         FXP_BINDIR    is where the executables are installed; 

         FXP_LIBDIR    is where other files needed by fxp - the heap images 
                       and the library - are installed; 

         SML_BINDIR    is the directory where the SML executables are found. 
                       It must contain the .arch-n-opsys script from the 
                       SML-NJ distribution, so make sure that this is where 
                       SML-NJ is physically installed; 

         SML_EXEC      is the name of the SML executable. This is the program 
                       that is called for generating the heap image and at 
                       execution of fxp. If sml is in your PATH at 
                       installation time, you don't need the full path here. 

         SML_MAKEDEF   is for defining the make command in SML. After version 
                       110.0.3, SML-NJ changed the type of CM.make'. 
                       For earlier or working versions of SML-NJ, use the 
                       second or third variant of this definition. 

    5. Edit the file src/config.sml according to your needs. Currently only a 
       single value can be configured here: 
            
         val retrieveCommand : string
             
             is the command to be used by fxp for retrieving a remote URI from
             the internet and storing it in a temporary file on the local file 
             system. It is a string value and should contain the strings %1 
             and %2, where:
 
               - %1 is replaced by the URI; 
               - %2 is replaced by the local filename. 

             It is recommended that the command exits with failure in case the 
             URI cannot be retrieved. If the command generates an HTML error
             message instead (like, e.g., "lynx -source %1 > %2"), this HTML
             file is considered to be XML and will probably cause a mess of 
             parsing errors. If you don't need URI retrieval, use "exit 1"
             which always fails on Unix. Sensible values are, e.g: 

               - "wget -qO %2 %1"   
                 (ftp://gnjilux.cc.fer.hr/pub/unix/util/wget/)
               - "got_it -o %2 %1" 
                 (ftp://sunsite.unc.edu/pub/Linux/apps/www/
                                 mirroring/got_it-0.34.tar.gz)
               - "urlget -s -o %2 %1" 
                  (ftp://sunsite.unc.edu/pub/Linux/apps/www/
                                 mirroring/urlget-3.12.tar.gz)

    6. Compile fxp by typing make; 
    7. Install fxp by typing make install. 
    8. If you want to use fxviz, you should also install vcg
       (ftp://ftp.cs.uni-sb.de/pub/graphics/vcg/).
```
