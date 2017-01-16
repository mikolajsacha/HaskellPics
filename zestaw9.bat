@echo Zestaw 9
@echo Zadanie 1 - niezrobione
@echo Zadanie 2 - otoczka wypukła
start /WAIT /B hspics.exe convex_hull pics/convex.png 5 +RTS -N2 -H
pause
start /WAIT /B hspics.exe convex_hull pics/convex.png 20 +RTS -N2 -H
pause
start /WAIT /B hspics.exe convex_hull pics/convex.png 1000 +RTS -N2 -H
pause
@echo Zadanie 3 - szkielet
start /WAIT /B hspics.exe skeleton pics/template1.png 5 +RTS -N2 -H
pause
start /WAIT /B hspics.exe skeleton pics/template1.png 20 +RTS -N2 -H
pause
start /WAIT /B hspics.exe skeleton pics/template1.png 1000 +RTS -N2 -H
pause
start /WAIT /B hspics.exe skeleton pics/template2.png 1000 +RTS -N2 -H
pause
start /WAIT /B hspics.exe skeleton pics/template3.png 1000 +RTS -N2 -H
pause
start /WAIT /B hspics.exe skeleton pics/template4.png 1000 +RTS -N2 -H
pause
start /WAIT /B hspics.exe skeleton pics/template5.png 1000 +RTS -N2 -H
pause
start /WAIT /B hspics.exe skeleton pics/template6.png 1000 +RTS -N2 -H
pause
@echo Zadanie 4 - pruning
start /WAIT /B hspics.exe pruning pics/template1.png 1000 +RTS -N2 -H
pause
start /WAIT /B hspics.exe pruning pics/template2.png 1000 +RTS -N2 -H
pause
start /WAIT /B hspics.exe pruning pics/template3.png 1000 +RTS -N2 -H
pause
start /WAIT /B hspics.exe pruning pics/template4.png 1000 +RTS -N2 -H
pause
start /WAIT /B hspics.exe pruning pics/template5.png 1000 +RTS -N2 -H
pause
start /WAIT /B hspics.exe pruning pics/template6.png 1000 +RTS -N2 -H
pause
@echo zadanie 5 - erozja i dylacja z LUT
start /WAIT /B hspics.exe lut_erosion pics/LENA_512.jpg Circle 1 +RTS -N2 -H
pause
start /WAIT /B hspics.exe lut_dilation pics/LENA_512.jpg Circle 1 +RTS -N2 -H
pause
