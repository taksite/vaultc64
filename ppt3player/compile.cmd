echo off

set assfile=ppt3.asm
set outfile=program.prg
set prghex=%outfile%.hex

del %outfile%
del %prghex%

rem echo on
.\bin\64Tass.exe  %assfile% -o %outfile% -L program_listing.txt

