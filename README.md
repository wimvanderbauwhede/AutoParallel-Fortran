# AutoParallel-Fortran
Gavin Davidson's Masters computing science project. Domain specific, automatically parallelising fortran compiler that takes scientific Fortran as input and produces paralell Fortran/OpenCL.

Language-Fortran, a Haskell based Fortran parser, is used by this project. The original parser is available at https://github.com/dagit/language-fortran.

## Installation

This project requires a Haskell compiler that is at least GHC version 6.0. Otherwise, installation is very simple:

    cd compiler
    make

If you change the `language-fortran` parser, do the following:
    
    cd language-fortran
    runhaskell Setup.hs configure
    runhaskell Setup.hs build

Then copy the file `Lexer.hs` from  `dist/build/Language/Fortran` to `src/Language/Fortran`.  

## Use

The compiler is a purely command line tool and offers no graphical interface. There are a number of command line arguments, but most are optional. For example, to run the compiler on a codebase whose main program is in 'main.f95' with subroutines located in 'subroutines.f95', located in the same directory as the compiler itself:

    cd compiler
    ./AutoParallel-Fortran -main main.f95 -modules subroutines.f95 -D NO_IO -v

NOTE: currently, the -D option must be specified with at least one macro.    

The `target.f95` could be replaced with an absolute or relative path if the file was located elsewhere. Supplying more than one filename to the compiler as arguments will cause it to consider both as part of the same program and produce a super kernel style program, combining the kernels that would be produced by each source file into one super kernel.

The optional command line flags are are follows:
- *-out* defines the directory where the output program will be saved. The name of the kernel file is derived from the original filenames. Not including this argument results in a the output being saved in the current directory.
- *-lfb* defines a value for the loop fusion bound. That is, the difference in iterator end value that is allowed for two loops to be fused. Not including this argument results in there not being a bound for loop fusion, and therefore all pairs of loops that meet the other conditions are fused.
- *-D* defines a list of c preprocessor (cpp) macros that are to be defined.
- *-v* enables verbose mode in which obstacles to parallelisation are reported to the user.
- *-ffixed-form* enforces that input lines must be no more than 72 characters long. Output is also formatted as fixed for (6 leading spaces on each line and no more than 72 characters per line).

## Modification

The parser used by this project was generated from a grammar file using the Happy parser generator. Therefore, making changes to the parser (Parser.y or Lexer.x) may require the installation of happy. Please refer to the readme in the original Language-Fortran repo for details on the installation of happy.
