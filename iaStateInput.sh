#!/bin/bash

# Run with: ' ./RunAnalysis <StormReports.csv> <ASOS_STID(KCAR)>

#################################################
#         IA-STATE ASOS CSV INPUT SCRIPT        #
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

# ************ THIS SCRIPT RUNS WITH IA STATE ASOS OBS ******************
#
# TIP: "How do I run an analysis of every January between 1974 and 2014?"
# -----------------------------------------------------------------------
# First download the IA State data as a csv between the two years you want.
# now you can run sed to remove any unwanted months. Here's an example:
#     sed '/-[0-1][2-9]-/d;/-10-/d;/-11-/d' GSO.txt > GSO_JAN.csv 
# Here we are running 3 delete commands: One to remove all dates starting with 0-1
# and ending in 2-9, another removing October, and the third removing November.
# Now just run this script to get your statistics. Ultimately there will be one
# command to find these kinds of solutions.
#


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

# Take care of some administrative business with the command arguments...
usage="$(basename "$0") <CSV Obs File> <Station ID> [-i]

<CSV Obs File> : ASOS Obs File formatted in IA State CSV output
<Station ID> : 4 or 3 letter station ID, depending on location (see help text)
-i : International location flag

This program will calculate long term statistics given any CSV observations from the IA State
archive. The station ID should be the three letter
IATA code for locations in the lower 48 US States. For any locations beyond the lower 48,
use the ICAO code and the [-i] flag when executing. All data generated will appear in your
working directory.

Example:
Data for Anchorage, AK from Jan 1 1990 to Dec 31 1990
Command: climoBuilder PANC 1990 1990 -i

Example:
Data for Greensboro, NC from Jan 1 1990 to Dec 31 2000
Command: climoBuilder GSO 1990 2000

Here are common prefixes for ICAO codes:
U.S. - K (Don't use ICAO for lower 48 in this program)
AK - P
PR - T
Mexico - M
Canada - C"

international=0
while getopts "ih" option; do
	case "$option" in
		i) 	international=1
			exit 1
			;;
		h)	echo "$usage"
			exit
			;;
		\?)	printf "illegal option: -%s\n" "$OPTARG" >&2
       			echo "$usage" >&2
       			exit 1
       			;;
	esac
done

# First we determine the upper air site ID for the station provided, for this we lookup from a reference file...
while IFS=, read col1 col2 col3 col4 col5 col6 col7
do
	#echo "$col1|$col2" # -- DEBUG
	if [ $line -gt 49 ]; then	
		if [ $international -eq 0 ]; then	
			if [[ "$col4" = "$2" ]]; then
				wmoID=$col5
				stnID=$col4
				stationName=$col2
			fi
		fi
		if [ $international -eq 1 ]; then
			
			if [[ "$col3" = "$2" ]]; then
				wmoID=$col5
				stnID=$col3
				stationName=$col2
			fi
		fi
	fi
	line=$(( $line+1 ))
done < stationTable.csv

echo "$stationName|$wmoID|$stnID"

#-----------------------------------------------------------------------------------------
# Filter Given ASOS File to meet case conditions
#-----------------------------------------------------------------------------------------
line=1
echo "Filtering Data..."
while IFS=, read col1 col2 col3 col4 col5 col6 col7 col8 col9 col10 col11 col12 col13 col14 col15 col16 col17 col18 col19 col20 col21 col22
do	
	if [ "$line" -gt "6" ]; then
		if (( $(bc <<< "$col11 < 0.62") )); then # If Visibility meets AMS Fog Definition... And reports a type of fog...
			if [[ "$col21" == *"FG"* ]]; then
				#echo "$col1|$col2|$col3|$col4|$col11|$col21" # -- FOR DEBUG
				timeStamp=${col2:5:2}${col2:8:2}${col2:0:4}
			
				# Find onset time...
				if [ $timeStamp != $tempDate ]; then
					i=$(( $i+1 ))
					# Temporary Arrays because we still need to find duplicates..	
					MONTH[$i]=${col2:5:2}
					DAY[$i]=${col2:8:2}
					YEAR[$i]=${col2:0:4}
					OTIME[$i]="${col2:11:2}${col2:14:2}"
					ETIME[$i]="${col2:11:2}${col2:14:2}"
					PTIME[$i]="${col2:11:2}${col2:14:2}"
					HOUR[$i]=${col2:11:2}
					#echo "${YEAR[$i]}|${MONTH[$i]}|${DAY[$i]}|${TIME[$i]}|${HOUR[$i]}" # -- DEBUG
					if [ ${HOUR[$i]} -gt 23 ]; then
						HOUR[i]=${HOUR[$i]:0:1}
						#HOUR[i]=0${HOUR[$i]}
					fi
					if [ ${HOUR[$i]} -lt 1 ]; then
						HOUR[i]=${HOUR[$i]:0:1}
						#HOUR[i]=0${HOUR[$i]}
					fi
					minVis=$col11
					tempDate=${col2:5:2}${col2:8:2}${col2:0:4}
		
				# Find event peak time
				elif (( $(echo $col11 "<" $minVis | bc -l) )); then
					minVis=$col11
					PTIME[$i]="${col2:11:2}${col2:14:2}"
				fi
		
				# Find Ending Time
				if [[ $timeStamp == $tempDate ]]; then
					ETIME[$i]="${col2:11:2}${col2:14:2}"
				fi
			fi
		fi
	fi
	line=$(( $line+1 ))
done < $1

#echo ${HOUR[*]} # -- FOR DEBUG
echo "Found $i unique dates..."
echo ""
echo "Obtaining datasets for analysis..."
echo ""

#------------------------------------------------------------------------------------------------------
# Grab and prep the data for each day...
#------------------------------------------------------------------------------------------------------
j=1 # loop counter
echo $i >> TIMES.DAT

while [ $j -lt $i ]
do
	wget -q "http://esrl.noaa.gov/raobs/intl/GetRaobs.cgi?shour=All+Times&ltype=Mandatory&wunits=Knots&bdate=${YEAR[$j]}${MONTH[$j]}${DAY[$j]}12&edate=${YEAR[$j]}${MONTH[$j]}${DAY[$j]}12&access=WMO+Station+Identifier&view=NO&StationIDs=$wmoID&osort=Station+Series+Sort&oformat=FSL+format+%28ASCII+text%29" -O ${YEAR[$j]}${MONTH[$j]}${DAY[$j]}.SND
	
	# Now we grab ASOS Data from IA State...
	wget -q "https://mesonet.agron.iastate.edu/cgi-bin/request/asos.py?station=$stnID&data=all&year1=${YEAR[$j]}&month1=${MONTH[$j]}&day1=${DAY[$j]}&year2=${YEAR[$j]}&month2=${MONTH[$j]}&day2=${DAY[$j]}&tz=Etc%2FUTC&format=comma&latlon=no&direct=no" -O temp.SFC	

	# Fortran has been reacting wildly to the raw data from IA State, but it's easy to rebuild a FORTRAN-Friendly file
	csvFile='temp.SFC'
	
	badDataCount=0
	line=1
	while IFS=, read col1 col2 col3 col4 col5 col6 col7 col8 col9 col10 col11 col12
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
			if [ $col11 = 'M' ]; then
				col11=999
				badDataCount=$(( $badDataCount+1 ))
			fi
			
			H=${col2:11:2}
			M=${col2:14:2}
			echo "$H$M,$col3,$col4,$col5,$col6,$col7,$col8,$col10,$col11" >> ${YEAR[$j]}${MONTH[$j]}${DAY[$j]}.SFC
		fi
		line=$(( $line+1 ))
	done < $csvFile

	if [ $badDataCount -gt 10 ]; then # More than 10 missing data values will alert the user
		echo "${YEAR[$j]}${MONTH[$j]}${DAY[$j]} : $badDataCount missing values!"	
	fi
	rm temp.SFC

	# We also create a time table for FORTRAN to use
	echo "${YEAR[$j]},${MONTH[$j]},${DAY[$j]},${OTIME[$j]},${PTIME[$j]},${ETIME[$j]}" >> TIMES.DAT
	
	j=$(( $j+1 )) # Advance loop counter
done

#-------------------------------------------------------------------------------------------------------------------------
# RUN ANALYSIS PROGRAMS
#-------------------------------------------------------------------------------------------------------------------------
# Now we run our FORTRAN program, which will return information for Python and NCL to plot with...
./climoBuilder

# Now we run NCL to create the 'average' sounding...
ncl sounding.ncl

#-------------------------------------------------------------------------------------------------------------------------
# CLEAN UP
#-------------------------------------------------------------------------------------------------------------------------
# FINALLY, we cleanup the data we downloaded, leaving our results!
rm *.SND
rm *.SFC
rm *.DAT
