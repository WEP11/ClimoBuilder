#!/bin/bash

# Run with: ' ./RunAnalysis <StormReports.csv> <SOUNDING_SITE_NUMBER> <ASOS_STID(KCAR)>

#################################################
#           RUN-ANALYSIS SCRIPT                 #
#################################################
#	Author: Warren Pettee			#
#	Date  : 21-April-2016			#
#						#
#						#
# This script begins the climatology analysis   #
# of any storm reports file. This extracts	#
# the dates, obtains data, and then executes	#
# a FORTRAN, Python, and NCL program to		#
# process the data.				#
#################################################

# First we want to read in the storm reports file
# but we want to omit repeat entries
#
# In BASH, the command argument is represented by $1,$2,etc.
# where $1 is the first argument provided, $2 is the seconc, etc.

# There are 35 columns in a storm reports file. 
# We only read the first 5 columns and the 6th is where the unneeded 
# columns are bundled together. We need time and date (columns 4 and 5)
# so those are saved as unique variables

zero=0
tempDate='NULL'
line=1
i=1 #Counter for filling an array

while IFS=, read col1 col2 col3 col4 col5 col6
do	if [ $line != 1 ]; then
		# echo "$col1|$col2|$col3|$col4|$col5" # -- FOR DEBUG
	
		if [ $col4 != $tempDate ]; then
			# Temporary Arrays because we still need to find duplicates..	
			MONTH[$i]=${col4:0:2}
			DAY[$i]=${col4:3:2}
			YEAR[$i]=${col4:6:9}
			TIME[$i]=$col5
			HOUR[$i]=${col5:0:2}
			if [ ${HOUR[$i]} -gt 23 ]; then
				HOUR[i]=${HOUR[$i]:0:1}
				#HOUR[i]=0${HOUR[$i]}
			fi
			if [ ${HOUR[$i]} -lt 1 ]; then
				HOUR[i]=${HOUR[$i]:0:1}
				#HOUR[i]=0${HOUR[$i]}
			fi
			i=$(( $i+1 ))
		fi
		tempDate=$col4
	fi
	line=$(( $line+1 ))
done < $1

#echo ${HOUR[*]} # -- FOR DEBUG
echo "Found $i unique dates..."
echo ""
echo "Obtaining datasets for analysis..."
echo ""

# Grab and prep the data for each day...
#	(Ideally we'd feed one file at a time into the FORTRAN program to save disk space
#	 but to meet the guidelines we will process all of the files in FORTRAN)

j=1 # loop counter
echo $i >> TIMES.DAT

while [ $j -lt $i ]
do
	# We get the html file and then extract the sounding data between the <PRE> tags...
	wget -q "http://weather.uwyo.edu/cgi-bin/sounding?region=naconf&TYPE=TEXT%3ALIST&YEAR=${YEAR[$j]}&MONTH=${MONTH[$j]}&FROM=${DAY[$j]}12&TO=${DAY[$j]}12&STNM=$2" -O tempSounding.html
	sed -n "/<PRE>/,/<\/PRE/p" tempSounding.html > ${YEAR[$j]}${MONTH[$j]}${DAY[$j]}.SND
	sed -i '$ d' ${YEAR[$j]}${MONTH[$j]}${DAY[$j]}.SND
	sed -i '1d' ${YEAR[$j]}${MONTH[$j]}${DAY[$j]}.SND
	rm tempSounding.html

	# Now we grab ASOS Data from IA State...
	wget -q "https://mesonet.agron.iastate.edu/cgi-bin/request/asos.py?station=$3&data=all&year1=${YEAR[$j]}&month1=${MONTH[$j]}&day1=${DAY[$j]}&year2=${YEAR[$j]}&month2=${MONTH[$j]}&day2=${DAY[$j]}&tz=Etc%2FUTC&format=comma&latlon=no&direct=no" -O temp.SFC	
	
	# Fortran has been reacting wildly to the raw data from IA State, but it's easy to rebuild a FORTRAN-Friendly file
	csvFile='temp.SFC'
	
	badDataCount=0
	line=1
	while IFS=, read col1 col2 col3 col4 col5 col6 col7 col8 col9 col10 col11
	do	
		# QUALITY CONTROL
		if [ $line -gt 5 ]; then
			#echo "$col1|$col2|$col3|$col4|$col5|$col6|$col7|$col8|$col9|$col10" # -- FOR DEBUG
			if [ $col3 = 'M' ]; then
				col3=999
				badDataCount=$(( $badDataCount+1 ))
			fi
			if [ $col4 = 'M' ]; then
				col4=999
				badDataCount=$(( $badDataCount+1 ))
			fi
			if [ $col5 = 'M' ]; then
				col5=999
				badDataCount=$(( $badDataCount+1 ))
			fi
			if [ $col6 = 'M' ]; then
				col6=999
				badDataCount=$(( $badDataCount+1 ))
			fi
			if [ $col7 = 'M' ]; then
				col7=999
				badDataCount=$(( $badDataCount+1 ))
			fi
			if [ $col8 = 'M' ]; then
				col8=999
				badDataCount=$(( $badDataCount+1 ))
			fi
			if [ $col10 = 'M' ]; then
				col10=999
				badDataCount=$(( $badDataCount+1 ))
			fi
			
			H=${col2:11:2}
			M=${col2:14:2}
			echo "$H$M,$col3,$col4,$col5,$col6,$col7,$col8,$col10" >> ${YEAR[$j]}${MONTH[$j]}${DAY[$j]}.SFC
		fi
		line=$(( $line+1 ))
	done < $csvFile

	if [ $badDataCount -gt 10 ]; then # More than 10 missing data values will alert the user
		echo "${YEAR[$j]}${MONTH[$j]}${DAY[$j]} : $badDataCount missing values!"	
	fi
	rm temp.SFC

	# Find the forecast hour...
	fadd=$(( ${HOUR[$j]}%3 )) #BASH only does integer division, so this works well for us
	if [ $fadd -eq 0 ]; then
		fhour=${HOUR[$j]}
	fi	
	if [ $fadd -gt 2 ]; then
		fhour=$(( ${HOUR[$j]}+$fadd ))
	fi
	if [ $fadd -le 2 ]; then
		fhour=$(( ${HOUR[$j]}-$fadd ))
	fi
	
	if [ $fhour -lt 10 ]; then
		fhour=0$fhour
	fi

	#echo $fhour # -- FOR DEBUG

	# We also create a time table for FORTRAN to use
	echo "${YEAR[$j]},${MONTH[$j]},${DAY[$j]},${TIME[$j]}" >> TIMES.DAT
	
	j=$(( $j+1 )) # Advance loop counter
done

# Now we run our FORTRAN program, which will return information for Python and NCL to plot with...
./climoBuilder

# Now we run NCL to create the 'average' sounding...
ncl sounding.ncl

# FINALLY, we cleanup the data we downloaded, leaving our results!
rm *.SND
rm *.SFC
rm *.DAT
