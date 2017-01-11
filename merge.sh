#!/bin/bash

topdir=/home/srearl/Desktop/superfolder
dir1=ar 
dir2=cr
# dir3=br
# dir4=cr


for f in $topdir/$dir1/*.csv

do

    outf=$topdir/`basename $f .csv`.csv
    cp $f $outf
    sed -e '1 d' $topdir/$dir2/`basename $f` >> $outf
#    sed -e '1 d' $topdir/$dir3/`basename $f` >> $outf
#    sed -e '1 d' $topdir/$dir4/`basename $f` >> $outf

done

# make a superfolder in the working directory that will hold the existing data and the newly acquired data
# ex reflect existing data, so, for example, the 'all' folder from the last data processing date (e.g. 'all_0501')
# ar is essentially alpha
# br is essentially bravo
# cr is essentially charlie
# be sure to delete the media and briefcase folders from the alpha, bravo, and charlie out folders before merging - this processing is for data only.
# put the folder with the existing data (e.g., aggregation of all previous data sets), and the alpha, bravo, and charlie, folders - all of those - in the superfolder. Because you have hardcoded the paths, you could really run merge.sh from any location
