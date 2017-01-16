@echo Zestaw 8
@echo Zadanie 1 - erozja (figury)
start /WAIT /B hspics.exe erosion pics/figury.png Circle 1 +RTS -N2 -H
pause
start /WAIT /B hspics.exe erosion pics/figury.png Circle 2 +RTS -N2 -H
@echo Zadanie 2 - erozja (Lena)
start /WAIT /B hspics.exe erosion pics/LENA_512.jpg Circle 1 +RTS -N2 -H
pause
start /WAIT /B hspics.exe erosion pics/LENA_512.jpg Square 1 +RTS -N2 -H
pause
@echo zadanie 3 - niezrobione
@echo zadanie 4 - dylacja (figury)
start /WAIT /B hspics.exe dilation pics/figury.png Circle 1 +RTS -N2 -H
pause
start /WAIT /B hspics.exe dilation pics/figury.png Circle 2 +RTS -N2 -H
pause
@echo Zadanie 5 - dylacja (Lena)
start /WAIT /B hspics.exe dilation pics/LENA_512.jpg Circle 1 +RTS -N2 -H
pause
start /WAIT /B hspics.exe dilation pics/LENA_512.jpg Square 1 +RTS -N2 -H
pause
@echo Zadanie 6 - otwarcie (dla r = 2,5,7,20)
start /WAIT /B hspics.exe opening pics/figury.png Circle 1 +RTS -N2 -H
pause
start /WAIT /B hspics.exe opening pics/figury.png Circle 2 +RTS -N2 -H
pause
start /WAIT /B hspics.exe opening pics/figury.png Circle 4 +RTS -N2 -H
pause
start /WAIT /B hspics.exe opening pics/figury.png Circle 7 +RTS -N2 -H
pause
@echo Zadanie 7 - zamkniecie (dla r = 2,5,7,20)
start /WAIT /B hspics.exe closing pics/figury.png Circle 1 +RTS -N2 -H
pause
start /WAIT /B hspics.exe closing pics/figury.png Circle 2 +RTS -N2 -H
pause
start /WAIT /B hspics.exe closing pics/figury.png Circle 4 +RTS -N2 -H
pause
start /WAIT /B hspics.exe closing pics/figury.png Circle 7 +RTS -N2 -H
pause
@echo Zadanie 8 - erozja i dylacja RGB
start /WAIT /B hspics.exe rgb_erosion pics/LENA_512.jpg Square 1 +RTS -N2 -H
pause
start /WAIT /B hspics.exe rgb_erosion pics/LENA_512.jpg Circle 1 +RTS -N2 -H
pause
start /WAIT /B hspics.exe rgb_dilation pics/LENA_512.jpg Square 1 +RTS -N2 -H
pause
start /WAIT /B hspics.exe rgb_dilation pics/LENA_512.jpg Circle 1 +RTS -N2 -H
pause
