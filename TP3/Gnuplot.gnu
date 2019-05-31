 # (c) Hacène Ouzia, UPMC 2013
 # Script Gnuplot ... 

 set title "Graphe  derivee  de x-->x2cosx "
 set xlabel "x "
 set ylabel "y "
 set grid

 plot "data.txt" title 'f(x)' with lines

 pause -1
 quit
