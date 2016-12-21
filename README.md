# AutoParallel-Fortran

A domain specific, automatically parallelising source-to-source compiler for Fortran-95 that takes scientific Fortran as input and produces parallel Fortran/OpenCL.

* Developed by Gavin Davidson for his Masters Computing Science project at the University of Glasgow.
* Maintained by Wim Vanderbauwhede

The Fortran parser used for this compiler is _Language-Fortran_, a Haskell based Fortran parser. The original parser is available at [https://github.com/dagit/language-fortran](https://github.com/dagit/language-fortran); the current project contains a modified version.

## Installation

This project requires a Haskell compiler that is at least GHC version 7.8. Installation is very simple:

    cd compiler
    make

If you change the `language-fortran` parser, do the following:

    cd language-fortran
    runhaskell Setup.hs configure
    runhaskell Setup.hs build  

Then copy the file `Lexer.hs` from  `dist/build/Language/Fortran` to `src/Language/Fortran`.  

    cp dist/build/Language/Fortran/Lexer.hs src/Language/Fortran

## Use of the compiler

The compiler is a command line tool. There are a number of command line arguments, but most are optional. For example, to run the compiler on a codebase whose main program is in 'main.f95' with subroutines located in 'subroutines.f95', located in the same directory as the compiler itself:

    cd compiler
    ./AutoParallel-Fortran -main main.f95 -modules subroutines.f95 -D NO_IO -v

NOTE: currently, the -D option must be specified with at least one macro (even if it is not used in the source code).    

The `target.f95` could be replaced with an absolute or relative path if the file was located elsewhere. Supplying more than one filename to the compiler as arguments will cause it to consider both as part of the same program and produce a super kernel style program, combining the kernels that would be produced by each source file into one super kernel.

The optional command line flags are are follows:
- *-out* defines the directory where the output program will be saved. The name of the kernel file is derived from the original filenames. Not including this argument results in a the output being saved in the current directory.
- *-lfb* defines a value for the loop fusion bound. That is, the difference in iterator end value that is allowed for two loops to be fused. Not including this argument results in there not being a bound for loop fusion, and therefore all pairs of loops that meet the other conditions are fused.
- *-D* defines a list of c preprocessor (cpp) macros that are to be defined.
- *-v* enables verbose mode in which obstacles to parallelisation are reported to the user.
- *-ffixed-form* enforces that input lines must be no more than 72 characters long. Output is also formatted as fixed for (6 leading spaces on each line and no more than 72 characters per line).

## OpenCL/C code generation

The compiler generates Fortran code in two parts: host code using the [_OclWrapper_ Fortran OpenCL API](https://github.com/wimvanderbauwhede/OpenCLIntegration) and kernel code in Fortran. OpenCL does not support Fortran so this code needs to be translated to OpenCL C code. This is done using [a separate compiler](https://github.com/wimvanderbauwhede/RefactorF4Acc) as follows:

### Create a config file

The AutoParallel-Fortran compiler will create a module containing the kernel files, with the name based on the original source code filename. Create a file `rf4a.cfg` in the folder containing the generated module:

    MODULE = module_<orig src name>_superkernel
    MODULE_SRC = module_<orig src name>_superkernel.f95
    TOP = <orig subroutine name>_superkernel
    KERNEL = <orig subroutine name>_superkernel
    PREFIX = .
    SRCDIRS = .  
    # A regex specifying which source files should be skipped
    EXCL_SRCS = (module_sub_superkernel_init|main_host|sub_host|\.[^f])
    EXCL_DIRS = < any folder that should not be search for source files >

### Now run the OpenCL translation

    $PATH_TO_SCRIPT/refactorF4acc.pl -P translate_to_OpenCL -c rf4a.cfg

## Modification of Parser and Lexer

The parser used by this project was generated from a grammar file using the _Happy_ parser generator and _Alex_ lexer. Therefore, to generate the Haskell sources after making changes to the parser or lexer (`Parser.y` or `Lexer.x`) requires the installation of [happy](https://www.haskell.org/happy/#download) and [alex](https://www.haskell.org/alex/).
