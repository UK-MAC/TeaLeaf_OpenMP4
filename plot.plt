set xlabel 'X'
set ylabel 'Y'
set term png
set palette rgbformulae 33,13,10

set output 'plot2d.png'
plot 'plot2d.dat' using 1:2:3 with points pointtype 5 pointsize 0.5 palette
