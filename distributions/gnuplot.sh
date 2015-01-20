#!/bin/bash
clear
echo "generating png's for datafiles"

gnuplot -e "datafile='Fork.dat'" gnuplot.txt > Fork.png &&
gnuplot -e "datafile='Spoon.dat'" gnuplot.txt > Spoon.png &&
gnuplot -e "datafile='Pyramide.dat'" gnuplot.txt > Pyramide.png &&
gnuplot -e "datafile='Steps.dat'" gnuplot.txt > Steps.png &&
gnuplot -e "datafile='ForkAddSpoon.dat'" gnuplot.txt > ForkAddSpoon.png &&
gnuplot -e "datafile='ForkAddPyramide.dat'" gnuplot.txt > ForkAddPyramide.png &&
gnuplot -e "datafile='SpoonAddSteps.dat'" gnuplot.txt > SpoonAddSteps.png &&
gnuplot -e "datafile='PyramideAddSteps.dat'" gnuplot.txt > PyramideAddSteps.png &&
gnuplot -e "datafile='StepsAddPyramide.dat'" gnuplot.txt > StepsAddPyramide.png &&
gnuplot -e "datafile='ForkSubSpoon.dat'" gnuplot.txt > ForkSubSpoon.png &&
gnuplot -e "datafile='SpoonSubFork.dat'" gnuplot.txt > SpoonSubFork.png &&
gnuplot -e "datafile='ScaledFork.dat'" gnuplot.txt > ScaledFork.png &&
gnuplot -e "datafile='ScaledsubFork.dat'" gnuplot.txt > ScaledsubFork.png &&

echo "finished"
