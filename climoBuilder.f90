!---------------------------------------------------------
!
! AUTOMATED STORM EVENT CLIMATOLOGY BUILDER
! Author    : Warren Pettee
! Date      : 21-Apr-2016
!
!---------------------------------------------------------
! This program reads in IA State ASOS data and Wyoming Sounding data
! for storm event cases given by the RunAnalysis.sh script. It
! will find the statistics from all cases.
! in addition an 'average' sounding is built from which the
! standard deviation is obtained. The number of soundings within
! one standard deviation is also given.
! 
! Compile with gfortran climoBuilder.f90 -o climoBuilder
! Run with climoBuilder.f90 <asosOnly> <soundingOnly>
!
!---------------------------------------------------------

PROGRAM climoBuilder

	IMPLICIT NONE	
	REAL :: versionControl = 1.2

	! INITIALIZATION -------------------------------------------------------------------------------------------------

	! External Functions:
	CHARACTER(len=8),EXTERNAL :: dateFileName
	REAL, EXTERNAL :: averageArray,stdArray,hiArray,loArray,eventOnsetAvg

	! Variables:
	INTEGER :: ioStatus, numberOfDates	! File IO Status, Number Of Case Dates, NULL Variable for bogus data reading
	INTEGER :: i,j,k,l,cases		! Loop Counters i,j,k,and l , and running case total for loops

	INTEGER,DIMENSION(:,:),ALLOCATABLE :: date 	! Array containing the Year,month,day,time as read from the dates input file

	!                          ORDER OF VARIABLES IN ASOS/SOUNDING ARRAY DIMENSIONS

	! ASOS DATA VARIABLE ORDER: time, temperature(F), dewpoint(F), relative humidity(%), wind direction(deg), 
	!	wind speed(kts), 1hr precip(in), sea level pressure(mb)

	! SOUNDING DATA VARIABLE ORDER: Pressure(mb), height(m),Temperature(C),Dewpoint(C), Wind Direction(deg), Wind Speed(kts)	


	REAL,DIMENSION(:,:,:),ALLOCATABLE :: soundingData, asosData	! Sounding Data/ASOS Data Arrays(CASE#,LEVEL/TIME,VARIABLE)
	REAL,DIMENSION(15,8) :: avgSounding,stdSounding !Average sounding and standard deviation sounding (LEVEL,VARIABLE)
	INTEGER,DIMENSION(15,8) :: goodSoundings !Sounding quantifying number of soundings within one standard deviation of a given
						 !level/variable (LEVEL,VARIABLE)
	REAL,DIMENSION(14) :: modeWindDir

	! Mandatory Levels: 925,850,700,500,400,300,250,200,150,100,70,50,30,20 ... No guarantee higher levels are reported
	REAL,DIMENSION(14),PARAMETER :: mandatoryLevels=(/925.,850.,700.,500.,400.,300.,250.,200.,150.,100.,70.,50.,30.,20./)

	LOGICAL,DIMENSION(:,:),ALLOCATABLE :: badSoundings ! A logical array to I.D. bad/non-existant soundings from QC
	
	CHARACTER(len=12),DIMENSION(:),ALLOCATABLE :: filenames	! Filenames to be used in getting the data
	CHARACTER(len=9) :: datesFile='TIMES.DAT'		! Name of the file containing the case dates

	! Surface,850,500 heights ; 850/500mb thicknesses, averages, highs, lows
	REAL :: sfc=0,lev850=0,lev500=0,thk850=0,thk850HI=0,thk850LOW=0,thk850AVG=0,thk500=0,thk500HI=0,thk500LOW=0,thk500AVG=0

	! Precipitable water, average, low, high; Average Mixing Ratio; average ASOS Temperature, running total for finding
	!    standard deviation
	REAL :: PW=0,avgPW=0,lowPW=0,hiPW=0,averageMixingRatio,stdTotal
	
	! Array for finding the average and standard deviation of 8 given variables
	REAL,DIMENSION(9) :: asosAvg=0, asosStd=0, asosHi=0, asosLo=0, asosOnsetAvg=0,asosOnsetStd=0,asosOnsetHi=0,asosOnsetLo=0,&
		asosPeakAvg=0,asosPeakStd=0,asosPeakHi=0,asosPeakLo=0, asosEndAvg=0,asosEndStd=0,asosEndHi=0,asosEndLo=0
        
        REAL,DIMENSION(9) :: asosMorningAvg=0, asosMorningStd=0, asosMorningLo=0, asosMorningHi=0, asosDayAvg=0, asosDayStd=0,&
                asosDayLo=0,asosDayHi=0,asosDuskAvg=0,asosDuskStd,asosDuskHi=0,asosDuskLo=0,asosNightAvg=0,asosNightStd=0,&
                asosNightHi=0,asosNightLo=0        


        ! Time-of-Day Case Counters:
        INTEGER :: morningCases=0, dayCases=0, duskCases=0, nightCases=0

	REAL,DIMENSION(14) :: upperBound,lowerBound
	INTEGER,DIMENSION(14) :: windCases

	INTEGER,DIMENSION(8,2,100) :: histogramData=0


	! BEGIN EXECUTION ------------------------------------------------------------------------------------------------

	WRITE(*,204)
	WRITE(*,205) versionControl
204	FORMAT(///)
205	FORMAT("*************************************************************",/,&
"*                                                           *",/,&
"*        AUTOMATED STORM EVENT CLIMATOLOGY BUILDER          *",/,&
"*                        - V",F5.2," -                           *",/,&
"*                  Warren Pettee (2016)                     *",/,&
"*************************************************************",//)

	! DATA PREPARATION -----------------------------------------------------------------------------------------------
	! First we need to discover how many events we have, so we count lines...
	numberOfDates=0
	
	OPEN ( UNIT=1, FILE=datesFile, STATUS='OLD', ACTION='READ', IOSTAT=ioStatus ) ! Open case dates listing

	IF (ioStatus == 0) THEN
		READ(1,*) numberOfDates ! First line of file contains the number of dates
		!numberOfDates=numberOfDates-1

		WRITE(*,203) numberOfDates
203		FORMAT('***', I3,' DATES FOUND! ***',//)

		! Knowing the number of dates, we can now allocate arrays dependant on that value
		ALLOCATE (date(numberOfDates,6))
		ALLOCATE (filenames(numberOfDates))
		ALLOCATE (soundingData(numberOfDates,20,8))
		ALLOCATE (asosData(numberOfDates,100,9))
		ALLOCATE (badSoundings(numberOfDates,20))

		! Initialize these arrays to improve Quality Control
		soundingData=-999
		asosData=-999

		! Now read each date component into the date array
		DO i=1,numberOfDates
			READ(1,201) date(i,1),date(i,2),date(i,3),date(i,4),date(i,5),date(i,6)
201			FORMAT (I4,X,I2,X,I2,X,I4,X,I4,X,I4)
		END DO
	ELSE ! If file cannot be opened...
		WRITE(*,202)datesFile,ioStatus
202		FORMAT ('Error opening file:',A10,' IOSTAT is',I6)
		WRITE(*,*)'ENTER NOT A VALID FILE!'
		STOP
	END IF
	
	CLOSE( UNIT=1 ) ! CLOSE DATE FILE

	! Now we turn the dates into respective filenames for the subroutines to pull data, I wrote a function to
	! turn the date numbers into a filename.
	DO i=1,numberOfDates
		filenames(i)=dateFileName(date(i,1),date(i,2),date(i,3))
	END DO

	! Then we need to read in the ASOS and Sounding data, but only one case at a time
	CALL readNoaaSounding(soundingData,filenames,badSoundings,numberOfDates)
	CALL readIaASOS(asosData,filenames,numberOfDates)

	! Since there are many events in this analysis, we have a quality control subroutine to take care of missing/bad
	! data.
	CALL asosQC(asosData,numberOfDates)

	! SOUNDING STATISTICS --------------------------------------------------------------------------------------------
	avgSounding=0 !Initialize the averageSounding to zero

	! First we find an 'average' sounding...
	! 	Notice that we cycle through each variable and level before the actual arrays.
	!	This is needed since we are averaging each part of a sounding.
	
	DO i=2,8 ! Loop through each variable
			
		DO j=1,14 ! Loop through each mandatory level
		cases=0 ! Reset case total as 0
			DO k=1,numberOfDates ! Loop through each date
					DO l=1,20 ! Loop through each level						
							IF(soundingData(k,l,1) == mandatoryLevels(j) .and. soundingData(k,l,i) < 999.98) THEN ! Find the current mandatory level
								avgSounding(j,i)=avgSounding(j,i)+soundingData(k,l,i) ! Add the value to the total
								cases=cases+1 ! Add one case to the running case total
							END IF	
					END DO	
			END DO
			IF(i == 7) THEN
				avgSounding(j,i)=avgSounding(j,i)/cases
				avgSounding(j,i)=MOD(avgSounding(j,i),360.)
			ELSE
				avgSounding(j,i)=avgSounding(j,i)/cases ! Find the average
			END IF
			avgSounding(j,1)=mandatoryLevels(j) ! Set the pressure level
		END DO
	END DO
	! Find the mode of windspeed, this one is a little complicated...
	CALL windMode(soundingData,numberOfDates,modeWindDir)
	! Find number of days within 68% of the mode...
	windCases=0
	DO i=1,14 !Set the boundaries within 68% of mode...
		upperBound(i) = (0.68 * modeWindDir(i)) + modeWindDir(i)
		lowerBound(i) = modeWindDir(i) - (0.68 * modeWindDir(i))
		IF( upperBound(i) < 0 ) THEN
			upperBound(i)=upperBound(i)+360.
		ELSE IF(upperBound(i) > 360) THEN
			upperBound(i)=upperBound(i)-360.
		END IF
		IF( lowerBound(i) < 0 ) THEN
			lowerBound(i)=lowerBound(i)+360.
		ELSE IF(lowerBound(i) > 360) THEN
			lowerBound(i)=lowerBound(i)-360.
		END IF
	END DO
	DO i=1,numberOfDates
		DO j=1,14
			DO k=1,20
				IF(soundingData(i,k,1) == mandatoryLevels(j)) THEN !Which cases are close to the mode?
					IF( soundingData(i,k,7) < upperBound(j) .and. soundingData(i,k,7) > lowerBound(j) ) THEN						
						windCases(j)=windCases(j)+1
					END IF
				END IF
			END DO
		END DO
	END DO
	! Now we find the Standard Deviation for every level/variable in a similar manner to the average array above
	DO i=2,8 ! Loop through each variable
		DO j=1,14 ! Loop through each mandatory level
		cases=0	! Reset running totals
		stdTotal=0
			DO k=1,numberOfDates ! Loop through each date
					DO l=1,20
							IF(soundingData(k,l,1) == mandatoryLevels(j) .and. soundingData(k,l,i) < 999.98) THEN ! Only get mandatory levels
								stdTotal=((soundingData(k,l,i)-avgSounding(j,i))**2)+stdTotal
								cases=cases+1 ! Add one case to the running case total
							END IF	
					END DO	
			END DO
			stdSounding(j,1)=mandatoryLevels(j) ! Set the pressure level

			stdTotal=stdTotal/cases ! Calculate the standard deviation
			stdSounding(j,i)=stdTotal**0.5
		END DO
	END DO

	! And lastly we find how many soundings are within one standard deviation of the mean
	goodSoundings=0
	DO i=2,8 ! Loop through each variable
		DO j=1,14 ! Reset running totals
			DO k=1,numberOfDates ! Loop through each date
					DO l=1,20
							IF(soundingData(k,l,1) == mandatoryLevels(j) .and. soundingData(k,l,i) < 999.98) THEN ! Only get mandatory levels
		
								! Here we find out if a case variable is within one standard deviation...
								IF((soundingData(k,l,i) - avgSounding(j,i)) < stdSounding(j,i) ) THEN
									goodSoundings(j,i)=goodSoundings(j,i)+1 ! ... If it is then lets
														!     include it in our total
								END IF	
							END IF
					END DO	
			END DO
		END DO
	END DO

	! Now we are going to get the hi,low,and average Sfc-850mb,Sfc-500mb thicknesses, and Precipital Water Values...
	DO i=1,numberOfDates ! Loop through each date
		PW=0. ! Reset the precipitable water value at 0
		DO j=2,20 ! Loop through each level
			IF(soundingData(i,j,1) /= 0) THEN ! Only proceed if there is actually data
				IF(j == 1) THEN ! Set the surface height
					sfc=soundingData(i,j,2)
				ELSE IF(soundingData(i,j,1) == mandatoryLevels(2)) THEN ! Set the 850mb height
					lev850=soundingData(i,j,2)
				ELSE IF(soundingData(i,j,1) == mandatoryLevels(4)) THEN ! Set the 500mb height
					lev500=soundingData(i,j,2)
				END IF
				IF(soundingData(i,j,6) < 99998) THEN
					! Find the average mixing ratio for the layer
					IF(i < numberOfDates) THEN
						averageMixingRatio=((soundingData(i,j,6)/1000.) + (soundingData((i+1),j,6)/1000.))/2.
					END IF
					! Find the precipitable water for the layer and add it to the total precipitable water
					IF(j < 20) THEN
						PW=(averageMixingRatio * ((soundingData(i,j,1)-soundingData(i,(j+1),1))*100)/9.81) + PW
					END IF
				END IF
				
			END IF
		END DO
		IF(PW > 99999) THEN
			PW=0
		END IF
		! Find the 850/500mb thicknesses
		thk850=lev850-sfc
		thk500=lev500-sfc

		! Add them to the total thicknesses
		thk850AVG=thk850AVG+thk850
		thk500AVG=thk500AVG+thk500
		
		! Add the precipitable water to the running total of the average
		avgPW=avgPW+PW

		! Prepare the high/lows if this is the first iteration
		IF(i == 1) THEN
			thk850HI=thk850
			thk850LOW=thk850
			thk500HI=thk500
			thk500LOW=thk500
			hiPW=PW
			lowPW=PW
		ELSE ! Otherwise find the high and low 850/500mb thicknesses and precipitable water
			IF(thk850 > thk850HI) THEN
				thk850HI=thk850
			ELSE IF (thk850 < thk850LOW) THEN
				thk850LOW=thk850
			END IF

			IF(thk500 > thk500HI) THEN
				thk500HI=thk500
			ELSE IF (thk500 < thk500LOW) THEN
				thk500LOW=thk500
			END IF

			IF(PW < lowPW) THEN
				lowPW=PW
			ELSE IF (PW > hiPW) THEN
				hiPW=PW
			END IF
			
		END IF
	END DO
	! Find the average 850/500mb thicknesses and precipitable water
	thk850AVG=thk850AVG/numberOfDates
	thk500AVG=thk500AVG/numberOfDates
	avgPW=avgPW/numberOfDates

	! Now we find the standard deviation of the thicknesses, precipitable water, and average sounding
	
	
	! Lastly, we EVALUATE the statistical parameters and generate output to inform the user about the cases

	! Now we do some daily statistics on the ASOS data...
	DO i=2,9
		asosAvg(i)=eventOnsetAvg(asosData(:,:,i),numberOfDates,date(:,4),asosData(:,:,1),0) ! Find ASOS data averages
		asosStd(i)=stdArray(asosData(:,:,i),asosAvg(i),numberOfDates,date(:,4),asosData(:,:,1),0) ! Find ASOS data standard deviations
		asosHi(i)=hiArray(asosData(:,:,i),numberOfDates,date(:,4),asosData(:,:,1),0)
		asosLo(i)=loArray(asosData(:,:,i),numberOfDates,date(:,4),asosData(:,:,1),0)

		asosOnsetAvg(i)=eventOnsetAvg(asosData(:,:,i),numberOfDates,date(:,4),asosData(:,:,1),1) ! Finds data averages at event start
		asosOnsetStd(i)=stdArray(asosData(:,:,i),asosOnsetAvg(i),numberOfDates,date(:,4),asosData(:,:,1),1) ! Find ASOS data standard deviations
		asosOnsetHi(i)=hiArray(asosData(:,:,i),numberOfDates,date(:,4),asosData(:,:,1),1)
		asosOnsetLo(i)=loArray(asosData(:,:,i),numberOfDates,date(:,4),asosData(:,:,1),1)

		asosPeakAvg(i)=eventOnsetAvg(asosData(:,:,i),numberOfDates,date(:,5),asosData(:,:,1),1) ! Finds data averages at event peak
		asosPeakStd(i)=stdArray(asosData(:,:,i),asosPeakAvg(i),numberOfDates,date(:,4),asosData(:,:,1),1) ! Find ASOS data standard deviations
		asosPeakHi(i)=hiArray(asosData(:,:,i),numberOfDates,date(:,5),asosData(:,:,1),1)
		asosPeakLo(i)=loArray(asosData(:,:,i),numberOfDates,date(:,5),asosData(:,:,1),1)

		asosEndAvg(i)=eventOnsetAvg(asosData(:,:,i),numberOfDates,date(:,6),asosData(:,:,1),1) ! Finds data averages at event end
		asosEndStd(i)=stdArray(asosData(:,:,i),asosEndAvg(i),numberOfDates,date(:,4),asosData(:,:,1),1) ! Find ASOS data standard deviations
		asosEndHi(i)=hiArray(asosData(:,:,i),numberOfDates,date(:,6),asosData(:,:,1),1)
		asosEndLo(i)=loArray(asosData(:,:,i),numberOfDates,date(:,6),asosData(:,:,1),1)
	END DO

        ! Next we find the time-of-day statistics. We will be using the following definitions...
        ! Morning : 0600-1000
        ! Day     : 1000-1600
        ! Dusk    : 1600-2000
        ! Night   : 2000-0600
        asosNightHi = (/ 0.,-90.,-90.,0.,-10.,0.,0.,0.,0. /)
        asosNightLo = (/ 0.,200.,200.,200.,400.,400.,1000.,2000.,80. /)
        ! Loop through all times...
        DO i=1,numberOfDates
                IF (date(i,5) < 600 .or. date(i,5) >= 2000) THEN
                        DO j=2,9
                                DO k=1,100
                                        IF (asosData(i,k,1) == date(i,5) .and. asosData(i,k,j) > -900 ) THEN
                                                nightCases=nightCases + 1
                                                asosNightAvg(j) = asosNightAvg(j) + asosData(i,k,j)
                                                IF (asosData(i,k,j) > asosNightHi(j)) THEN
                                                        asosNightHi(j) = asosData(i,k,j)
                                                END IF

                                                IF (asosData(i,k,j) < asosNightLo(j)) THEN
                                                        asosNightLo(j) = asosData(i,k,j)
                                                END IF
                                        END IF
                                END DO
                        END DO
                END IF
        END DO
        
        DO i=1,9
                asosNightAvg(i) = asosNightAvg(i) / nightCases
        END DO



	CALL asosHistogram(histogramData,asosData,asosOnsetAvg,asosOnsetStd,numberOfDates,date(:,4)) ! Grab histogram data based on the onset time
	CALL asosDTHistogram(histogramData,asosData,numberOfDates,date(:,4)) ! Grab histogram data based on the onset time

	! LAST, export the data so the user can use it ...

	OPEN ( UNIT=8, FILE='Results.txt', STATUS='NEW', ACTION='WRITE', IOSTAT=ioStatus )
402	FORMAT("Statistics generated from", I5," cases.")
                                                                                                                                           
801 FORMAT("------------------------------------------------------------------------------------------------")
802 FORMAT("                                           CLIMO-BUILDER")
803 FORMAT("                                               V-",F5.2)
804 FORMAT("                                         Warren Pettee(2016)")
805 FORMAT("                                         Program Output File")

	WRITE(8,801)
	WRITE(8,802)
	WRITE(8,803) versionControl
	WRITE(8,804)
	WRITE(8,801)
	WRITE(8,309)
	WRITE(8,805)
	WRITE(8,309)
	
	WRITE(8,402) numberOfDates
	WRITE(8,309)
	WRITE(8,309)
	WRITE(8,301)
	WRITE(8,302)
	WRITE(8,303)
	WRITE(8,304)
403     FORMAT("                                      EVENING EVENT STATISTICS")
397	FORMAT("                                      EVENT END ASOS STATISTICS")
398	FORMAT("                                     EVENT PEAK ASOS STATISTICS")
399	FORMAT("                                     EVENT ONSET ASOS STATISTICS")	
301	FORMAT("                                       DAILY ASOS STATISTICS")	
302	FORMAT("------------------------------------------------------------------------------------------------")
303	FORMAT("         T(°F)     Td(°F)    RH(%)     W Dir(°)     Spd(kts)  Prcp(in/hr)     SLP(mb)    Vis(mi)")
304	FORMAT("------------------------------------------------------------------------------------------------")	
305	FORMAT("AVERAGE: ",F5.2, 5X, F5.2, 5X, F7.2, 5X, F6.2, 5X, F6.2, 5X, F5.2, 12X, F7.2, 5X, F6.2)
306	FORMAT("STD    : ",F5.2, 5X, F5.2, 5X, F7.2, 5X, F6.2, 5X, F6.2, 5X, F5.2, 12X, F7.2, 5X, F6.2)
307	FORMAT("HIGH   : ",F5.2, 5X, F5.2, 5X, F7.2, 5X, F6.2, 5X, F6.2, 5X, F5.2, 12X, F7.2, 5X, F6.2)
308	FORMAT("LOW    : ",F5.2, 5X, F5.2, 5X, F7.2, 5X, F6.2, 5X, F6.2, 5X, F5.2, 12X, F7.2, 5X, F6.2)
	WRITE(8,305) asosAvg(2),asosAvg(3),asosAvg(4),asosAvg(5),asosAvg(6),asosAvg(7),asosAvg(8),asosAvg(9)
	WRITE(8,306) asosStd(2),asosStd(3),asosStd(4),asosStd(5),asosStd(6),asosStd(7),asosStd(8),asosAvg(9)
	WRITE(8,307) asosHi(2),asosHi(3),asosHi(4),asosHi(5),asosHi(6),asosHi(7),asosHi(8),asosHi(9)
	WRITE(8,308) asosLo(2),asosLo(3),asosLo(4),asosLo(5),asosLo(6),asosLo(7),asosLo(8),asosLo(9)

	WRITE(8,309)
	WRITE(8,399)
	WRITE(8,302)
	WRITE(8,303)
	WRITE(8,304)
	WRITE(8,305) asosOnsetAvg(2),asosOnsetAvg(3),asosOnsetAvg(4),asosOnsetAvg(5),asosOnsetAvg(6),asosOnsetAvg(7),&
			asosOnsetAvg(8),asosOnsetAvg(9)
    WRITE(8,306) asosOnsetStd(2),asosOnsetStd(3),asosOnsetStd(4),asosOnsetStd(5),asosOnsetStd(6),asosOnsetStd(7),&
			asosOnsetStd(8),asosOnsetStd(9)
    WRITE(8,307) asosOnsetHi(2),asosOnsetHi(3),asosOnsetHi(4),asosOnsetHi(5),asosOnsetHi(6),asosOnsetHi(7),&
			asosOnsetHi(8),asosOnsetHi(9)
    WRITE(8,308) asosOnsetLo(2),asosOnsetLo(3),asosOnsetLo(4),asosOnsetLo(5),asosOnsetLo(6),asosOnsetLo(7),&
			asosOnsetLo(8),asosOnsetLo(9)

	WRITE(8,309)
	WRITE(8,398)
	WRITE(8,302)
	WRITE(8,303)
	WRITE(8,304)
	WRITE(8,305) asosPeakAvg(2),asosPeakAvg(3),asosPeakAvg(4),asosPeakAvg(5),asosPeakAvg(6),asosPeakAvg(7),&
			asosPeakAvg(8),asosPeakAvg(9)
    WRITE(8,306) asosPeakStd(2),asosPeakStd(3),asosPeakStd(4),asosPeakStd(5),asosPeakStd(6),asosPeakStd(7),&
			asosPeakStd(8),asosPeakStd(9)
    WRITE(8,307) asosPeakHi(2),asosPeakHi(3),asosPeakHi(4),asosPeakHi(5),asosPeakHi(6),asosPeakHi(7),&
			asosPeakHi(8),asosPeakHi(9)
    WRITE(8,308) asosPeakLo(2),asosPeakLo(3),asosPeakLo(4),asosPeakLo(5),asosPeakLo(6),asosPeakLo(7),&
			asosPeakLo(8),asosPeakLo(9)

	WRITE(8,309)
	WRITE(8,397)
	WRITE(8,302)
	WRITE(8,303)
	WRITE(8,304)
	WRITE(8,305) asosEndAvg(2),asosEndAvg(3),asosEndAvg(4),asosEndAvg(5),asosEndAvg(6),asosEndAvg(7),&
			asosEndAvg(8),asosEndAvg(9)
    WRITE(8,306) asosEndStd(2),asosEndStd(3),asosEndStd(4),asosEndStd(5),asosEndStd(6),asosEndStd(7),&
			asosEndStd(8),asosEndStd(9)
    WRITE(8,307) asosEndHi(2),asosEndHi(3),asosEndHi(4),asosEndHi(5),asosEndHi(6),asosEndHi(7),&
			asosEndAvg(8),asosEndAvg(9)
    WRITE(8,308) asosEndLo(2),asosEndLo(3),asosEndLo(4),asosEndLo(5),asosEndLo(6),asosEndLo(7),&
			asosEndLo(8),asosEndLo(9)
        WRITE(8,309)
        WRITE(8,403)
        WRITE(8,302)
        WRITE(8,303)
        WRITE(8,304)
	WRITE(8,305) asosNightAvg(2),asosNightAvg(3),asosNightAvg(4),asosNightAvg(5),asosNightAvg(6),asosNightAvg(7),&
			asosNightAvg(8),asosNightAvg(9)
    !WRITE(8,306) asosEndStd(2),asosEndStd(3),asosEndStd(4),asosEndStd(5),asosEndStd(6),asosEndStd(7),&
!			asosEndStd(8),asosEndStd(9)
    WRITE(8,307) asosNightHi(2),asosNightHi(3),asosNightHi(4),asosNightHi(5),asosNightHi(6),asosNightHi(7),&
			asosNightAvg(8),asosNightAvg(9)
    WRITE(8,308) asosNightLo(2),asosNightLo(3),asosNightLo(4),asosNightLo(5),asosNightLo(6),asosNightLo(7),&
			asosNightLo(8),asosNightLo(9)
309	FORMAT(/)
310	FORMAT("                                        UPPER AIR STATISTICS")
311	FORMAT("                          AVG       HI       LO")
312	FORMAT("850mb Thickness (m)    : ",F8.2,2X,F8.2,2X,F9.2)
313	FORMAT("500mb Thickness (m)    : ",F8.2,2X,F8.2,2X,F9.2)
314	FORMAT("Precipitable Water (mm): ",F8.2,2X,F8.2,2X,F9.2)
	
	WRITE(8,309)
	WRITE(8,309)
	WRITE(8,310)
	WRITE(8,311)
	WRITE(8,312)thk850AVG,thk850HI,thk850LOW
	WRITE(8,313)thk500AVG,thk500HI,thk500LOW
	WRITE(8,314)avgPW,hiPW,lowPW

	WRITE(8,309)
	WRITE(8,315)
	WRITE(8,302)
	WRITE(8,316)
	WRITE(8,302)
315	FORMAT("                                         MEAN SOUNDING")
316     FORMAT(" P(mb)     Hgt(m)     T(°C)     Td(°C)      RH(%)     Mix(g/kg)     W Dir(°)     Spd(kts)")
317	FORMAT(1X, I4, 6X, I6, 5X, F6.2, 5X, F6.2, 6X, F6.2, 9X, F7.3, 8X, F4.0, 7X, F6.2)	
	DO i=1,10
		WRITE(8,317) INT(avgSounding(i,1)),INT(avgSounding(i,2)),avgSounding(i,3),avgSounding(i,4),avgSounding(i,5),&
		avgSounding(i,6),modeWindDir(i),avgSounding(i,8)
	END DO
318	FORMAT("                                      SOUNDING STATISTICS")
319	FORMAT("                             Standard Deviation | # of cases within 1 STD")
320	FORMAT("     P(mb)        Hgt(m)           T(°C)            Td(°C)           RH(%)            Mix(g/kg)           W Dir(°)",&
"         Spd(kts)")
321	FORMAT(1X, F4.0, 9X, F5.0,' | ',I4, 5X, F6.2,' | ',I4, 5X, F6.2,' | ',I4, 6X, F5.2,' | ',I4, 9X, F5.3,' | ',I4, 8X, F4.0,' | ',&
I4, 7X, F6.2'|',I4)	
322	FORMAT("-------------------------------------------------------------------------------------------------------------------&
&----------------------")
	WRITE(8,309)
	WRITE(8,318)
	WRITE(8,319)
	WRITE(8,322)
	WRITE(8,320)
	WRITE(8,322)
	DO i=1,10
		WRITE(8,321) stdSounding(i,1),stdSounding(i,2),goodSoundings(i,2),stdSounding(i,3),&
		goodSoundings(i,3),stdSounding(i,4),goodSoundings(i,4),stdSounding(i,5),goodSoundings(i,5),stdSounding(i,6),&
		goodSoundings(i,6),modeWindDir(i),windCases(i),stdSounding(i,8),goodSoundings(i,8)
	END DO
	CLOSE(UNIT=8)

    ! Export histogram data as csv...
324 FORMAT( 100(I4,",") )
    OPEN ( UNIT=9, FILE='ASOShistogram.csv', STATUS='NEW', ACTION='WRITE', IOSTAT=ioStatus )
	DO i=2,8
	WRITE(9,*)
		DO j=1,2
			WRITE(9,*)
			DO k=1,100
				WRITE(9,324,advance="no")histogramData(i,j,k)
			END DO
		END DO
	END DO
    CLOSE(UNIT=9)

	! Last we export the average sounding in a way so we can pass it to NCL or Python for plotting...
325	FORMAT(I4, 5X, I5, 5X, F6.2, 5X, F6.2, 5X, F6.2, 5X, F7.3, 5X, I3, 5X, F6.2, 5X, F6.2, 5X, F6.2)
	OPEN ( UNIT=9, FILE='meanSound.csv', STATUS='NEW', ACTION='WRITE', IOSTAT=ioStatus )
	DO i=1,14
		WRITE(9,325) INT(avgSounding(i,1)),INT(avgSounding(i,2)),avgSounding(i,3),avgSounding(i,4),avgSounding(i,5),&
		avgSounding(i,6),INT(modeWindDir(i)),avgSounding(i,8),stdSounding(i,3),stdSounding(i,4)
	END DO
	CLOSE(UNIT=9)

END PROGRAM

!=======================================================
! READ WYOMING SOUNDING DATA
!
! This reads in an unknown number of soundings from the
! Wyoming Sounding database into a 3-dimensional array
!
!=======================================================
SUBROUTINE readWySounding(soundingData,filenames,badSoundings,numberOfDates)

	INTEGER,INTENT(IN) :: numberOfDates
	CHARACTER(len=12),DIMENSION(numberOfDates),INTENT(IN) :: filenames
	REAL,DIMENSION(numberOfDates,100,8),INTENT(OUT) :: soundingData
	LOGICAL,DIMENSION(numberOfDates),INTENT(OUT) :: badSoundings

	REAL,DIMENSION(8) :: upperThreshold, lowerThreshold

	CHARACTER(len=12),DIMENSION(numberOfDates) :: sndFiles
	CHARACTER(len=4) :: sndExt='.SND'

	INTEGER :: i,j,EOF,ioStatus,totalCases !Loop Counters i and j, and IOSTATUS numbers EOF and ioStatus

	upperThreshold = (/0.,0.,40.,40.,104.,20.,360.,150./)
	lowerThreshold = (/0.,0.,-100.,-100.,0.,0.,0.,0./)
	totalCases = 0
	! First create the file names...
	DO i=1,numberOfDates
		sndFiles(i)= TRIM(filenames(i)) // sndExt ! Trim removes trailing blanks in concatenation
	END DO

	! Feed the data into the array...
	DO i=1,numberOfDates
		OPEN ( UNIT=2, FILE=sndFiles(i), STATUS='OLD', ACTION='READ', IOSTAT=ioStatus ) ! Open the file
		IF (ioStatus == 0) THEN

			READ(2,2000,IOSTAT=EOF) ! Skip the first 5 lines, it's all header data
2000			FORMAT(////)
			IF (EOF<0) THEN ! This detects empty files, which hopefully are detected before they get to
					! the fortran program
				WRITE(*,2001)i,sndFiles(i)
2001				FORMAT('*** FORTRAN HAS DETECTED AN ERROR WITH CASE #',I4,' | ',A12)
				badSoundings(i)=.TRUE.
				CYCLE
			END IF
			! Now we cycle through each level...
			badSoundings(i)=.FALSE.
levelLoop:		DO j=1,100

				READ(2,2003,IOSTAT=EOF)soundingData(i,j,1),soundingData(i,j,2),soundingData(i,j,3),&
				soundingData(i,j,4),soundingData(i,j,5),soundingData(i,j,6),soundingData(i,j,7),&
				soundingData(i,j,8)

2003				FORMAT(1X,F6.1,2X,F5.0,2X,F5.1,2X,F5.1,4X,F3.0,2X,F5.2,4X,F3.0,4X,F3.0)
				IF(soundingData(i,j,1) == 0) THEN
					EXIT levelLoop
				END IF
				! Value QC
soundingQC:			DO k=3,5
					IF (soundingData(i,j,k) > upperThreshold(k) .or. soundingData(i,j,k) < lowerThreshold(k)) THEN
						WRITE(*,2001)i,sndFiles(i)
						WRITE(*,*)k,soundingData(i,j,k)
						badSoundings(i)=.TRUE.
						EXIT levelLoop
					END IF
				END DO soundingQC	

				IF (EOF<0) THEN
					EXIT ! We've reached the end of this data file, cycle to the next
				END IF
			END DO levelLoop
		ELSE ! If file cannot be opened...
			WRITE(*,2202)datesFile,ioStatus
2202			FORMAT ('Error opening file:',A10,' IOSTAT is',I6)
			WRITE(*,*)'ENTER NOT A VALID FILE!'
			STOP
		END IF

		CLOSE( UNIT=2 ) ! Close this file and loop to the next
		IF(badSoundings(i) .neqv. .TRUE.) THEN
			totalCases=totalCases+1
		END IF
	END DO
	WRITE(*,2002)totalCases
2002	FORMAT('--- 'I4,' CASES PASSED QUALITY CONTROL ---')		

END SUBROUTINE

!=======================================================
! READ NOAA ESRL SOUNDING DATA
!
! This reads in an unknown number of soundings from the
! ESRL Sounding database into a 3-dimensional array
!
!=======================================================
SUBROUTINE readNoaaSounding(soundingData,filenames,badSoundings,numberOfDates)

	INTEGER,INTENT(IN) :: numberOfDates
	CHARACTER(len=12),DIMENSION(numberOfDates),INTENT(IN) :: filenames
	!SOUNDING DATA VARIABLE ORDER: Pressure(mb), height(m),Temperature(C),Dewpoint(C), Wind Direction(deg), Wind Speed(kts)	
	REAL,DIMENSION(numberOfDates,20,8),INTENT(OUT) :: soundingData
	LOGICAL,DIMENSION(numberOfDates,20),INTENT(OUT) :: badSoundings

	REAL,DIMENSION(6) :: upperThreshold, lowerThreshold
	INTEGER,DIMENSION(7) :: dataRow

	CHARACTER(len=12),DIMENSION(numberOfDates) :: sndFiles
	CHARACTER(len=4) :: sndExt='.SND'

	INTEGER :: i,j,EOF,ioStatus,totalCases !Loop Counters i and j, and IOSTATUS numbers EOF and ioStatus
	dataRow=0
	upperThreshold = (/1500.,50000.,40.,40.,360.,200./)
	lowerThreshold = (/0.,-10.,-100.,-100.,0.,0./)
	totalCases = 0
	! First create the file names...
	DO i=1,numberOfDates
		sndFiles(i)= TRIM(filenames(i)) // sndExt ! Trim removes trailing blanks in concatenation
	END DO

	! Feed the data into the array...
	DO i=1,numberOfDates
		OPEN ( UNIT=2, FILE=sndFiles(i), STATUS='OLD', ACTION='READ', IOSTAT=ioStatus ) ! Open the file
		IF (ioStatus == 0) THEN

			READ(2,2000,IOSTAT=EOF) ! Skip the first 4 lines, it's all header data
2000			FORMAT(///)

			IF (EOF<0) THEN ! This detects empty files, which hopefully are detected before they get to
					! the fortran program
				WRITE(*,2001)i,sndFiles(i)
2001				FORMAT('*** FORTRAN HAS DETECTED AN ERROR WITH CASE #',I4,' | ',A12)
				badSoundings(i,1)=.TRUE.
				CYCLE
			END IF

			! Now we cycle through each level...
			badSoundings(i,1)=.FALSE.
levelLoop:		DO j=1,20
					! Temporary row to read the raw data
					READ(2,2003,IOSTAT=EOF)dataRow(1),dataRow(2),dataRow(3),dataRow(4),dataRow(5),dataRow(6),&
					dataRow(7)
2003				FORMAT(7i7)

					! Let's make sure the first level is assigned to the sfc reading...
					IF(dataRow(1) == 9) THEN ! Only if this is a mandatory level reading
						soundingData(i,1,1)=REAL(dataRow(2))/10. ! Pressure
						soundingData(i,1,2)=REAL(dataRow(3)) ! Height
						IF(dataRow(4) == 99999) THEN
							soundingData(i,1,3)=99999.0 ! Bad Temp
						ELSE
							soundingData(i,1,3)=REAL(dataRow(4))/10. ! Temp
						END IF
						IF(dataRow(5) == 99999) THEN
							soundingData(i,1,4)=99999.0 ! Bad Dewp
						ELSE
							soundingData(i,1,4)=REAL(dataRow(5))/10. ! Dewp
						END IF
						IF(dataRow(6) == 99999) THEN
							soundingData(i,1,7)=99999.0 ! Bad Wind Dir
						ELSE
							soundingData(i,1,7)=REAL(dataRow(6)) ! Wind Dir
						END IF					
						IF(dataRow(7) == 99999) THEN
							soundingData(i,1,8)=99999.0 ! Bad Wind Spd
						ELSE	
							soundingData(i,1,8)=REAL(dataRow(7)) ! Wind Spd
						END IF

						! Find RH and Mixing Ratio, Dependant on Good input values...
						IF(dataRow(4) /= 99999 .and. dataRow(5) /= 99999) THEN
							! Find RH
							soundingData(i,1,5)=100*(EXP((17.625*soundingData(i,1,4))/(243.04+soundingData(i,1,4)))/&
												EXP((17.625*soundingData(i,1,3))/(243.04+soundingData(i,1,3))))
							e=6.11*10**((7.5*soundingData(i,1,4))/(237.7+soundingData(i,1,4)))
							! Find Mixing Ratio							
							soundingData(i,j,6)=621.97*(e/(soundingData(i,1,1)-e))
						ELSE ! Failed QC, create bad variables
							soundingData(i,1,5)=99999.0
							soundingData(i,1,6)=99999.0
						END IF

					END IF

					! Now we assign the data to the main array
					IF(dataRow(1) == 4) THEN ! Only if this is a mandatory level reading
						soundingData(i,j,1)=REAL(dataRow(2))/10. ! Pressure
						soundingData(i,j,2)=REAL(dataRow(3)) ! Height
						IF(dataRow(4) == 99999) THEN
							soundingData(i,j,3)=99999.0 ! Bad Temp
						ELSE
							soundingData(i,j,3)=REAL(dataRow(4))/10. ! Temp
						END IF
						IF(dataRow(5) == 99999) THEN
							soundingData(i,j,4)=99999.0 ! Bad Dewp
						ELSE
							soundingData(i,j,4)=REAL(dataRow(5))/10. ! Dewp
						END IF
						IF(dataRow(6) == 99999) THEN
							soundingData(i,j,7)=99999.0 ! Bad Wind Dir
						ELSE
							soundingData(i,j,7)=REAL(dataRow(6)) ! Wind Dir
						END IF					
						IF(dataRow(7) == 99999) THEN
							soundingData(i,j,8)=99999.0 ! Bad Wind Spd
						ELSE	
							soundingData(i,j,8)=REAL(dataRow(7)) ! Wind Spd
						END IF

						! Find RH and Mixing Ratio, Dependant on Good input values...
						IF(dataRow(4) /= 99999 .and. dataRow(5) /= 99999) THEN
							! Find RH
							soundingData(i,j,5)=100*(EXP((17.625*soundingData(i,j,4))/(243.04+soundingData(i,j,4)))/&
												EXP((17.625*soundingData(i,j,3))/(243.04+soundingData(i,j,3))))
							e=6.11*10**((7.5*soundingData(i,j,4))/(237.7+soundingData(i,j,4)))
							! Find Mixing Ratio							
							soundingData(i,j,6)=621.97*(e/(soundingData(i,j,1)-e))
						ELSE ! Failed QC, create bad variables
							soundingData(i,j,5)=99999.0
							soundingData(i,j,6)=99999.0
						END IF
					END IF
					
					IF (EOF<0) THEN
						EXIT ! We've reached the end of this data file, cycle to the next
					END IF
			END DO levelLoop
		ELSE ! If file cannot be opened...
			WRITE(*,2202)datesFile,ioStatus
2202			FORMAT ('Error opening file:',A10,' IOSTAT is',I6)
			WRITE(*,*)'ENTER NOT A VALID FILE!'
			STOP
		END IF

		CLOSE( UNIT=2 ) ! Close this file and loop to the next
		IF(badSoundings(i,1) .neqv. .TRUE.) THEN
			totalCases=totalCases+1
		END IF
	END DO
	WRITE(*,2002)totalCases,numberOfDates
2002	FORMAT('--- 'I4,' CASES PASSED QUALITY CONTROL OUT OF'I4,' ---')		

END SUBROUTINE

!=======================================================
! READ IA STATE ASOS DATA
!
! This reads in an unknown number of IA State ASOS archive
! files and assigns the data to a 3 dimensional array.
! It is very similar to the above sounding reader
!
!=======================================================
SUBROUTINE readIaASOS(asosData,filenames,numberOfDates)

	INTEGER,INTENT(IN) :: numberOfDates
	CHARACTER(len=12),DIMENSION(numberOfDates),INTENT(IN) :: filenames
	REAL,DIMENSION(numberOfDates,100,9),INTENT(OUT) :: asosData
	
	CHARACTER(len=12),DIMENSION(numberOfDates) :: sfcFiles
	CHARACTER(len=4) :: sfcExt='.SFC'

	INTEGER :: i,j,EOF,ioStatus !Loop Counters i and j, and IOSTATUS numbers EOF and ioStatus

	! First create the file names...
	DO i=1,numberOfDates
		sfcFiles(i)= TRIM(filenames(i)) // sfcExt ! Trim removes trailing blanks in concatenation
	END DO

	! Feed the data into the array...
	DO i=1,numberOfDates
		OPEN ( UNIT=3, FILE=sfcFiles(i), STATUS='OLD', ACTION='READ', IOSTAT=ioStatus ) ! Open the file
		IF (ioStatus == 0) THEN

			READ(3,*,IOSTAT=EOF) ! Skip the first line, it's header data
			IF (EOF<0) THEN ! This detects empty files, which hopefully are detected before they get to
					! the fortran program
				WRITE(*,3001)i,sfcFiles(i)
3001				FORMAT('*** FORTRAN HAS DETECTED AN ERROR WITH CASE #',I4,' | ',A12)
				CYCLE
			END IF
			! Now we cycle through each timestamp...
			DO j=1,100
				READ(3,*,IOSTAT=EOF)asosData(i,j,1),asosData(i,j,2),asosData(i,j,3),&
				asosData(i,j,4),asosData(i,j,5),asosData(i,j,6),asosData(i,j,7),&
				asosData(i,j,8),asosData(i,j,9)
				!WRITE(*,*) asosData(i,j,1)
				IF (EOF<0) THEN
					EXIT ! We've reached the end of this data file, cycle to the next
				END IF
			END DO
		ELSE ! If file cannot be opened...
			WRITE(*,3202)datesFile,ioStatus
3202			FORMAT ('Error opening file:',A10,' IOSTAT is',I6)
			WRITE(*,*)'ENTER NOT A VALID FILE!'
			STOP
		END IF
		CLOSE( UNIT=3 ) ! Close this file and loop to the next
	END DO

END SUBROUTINE

!=======================================================
! ASOS Data Quality Control Subroutine
!
! This identifies missing data and replaces them
!
!=======================================================
SUBROUTINE asosQC(dataArray,arraySize)
	INTEGER,INTENT(IN) :: arraySize
	REAL,DIMENSION(arraySize,100,9),INTENT(INOUT) :: dataArray
	
	INTEGER :: i,j,k,errorCount
	LOGICAL,DIMENSION(arraySize,100,9) :: errorTrack

	errorCount=0

	! Identify all missing data...
	DO i=1,arraySize
		DO j=1,100
			DO k=1,9
				IF(dataArray(i,j,k) == 999) THEN
					errorTrack(i,j,k)=.TRUE.
					errorCount=errorCount+1
				ELSE
					errorTrack(i,j,k)=.FALSE.
				END IF
			END DO
		END DO
	END DO

! Identify bogus data ...
    DO i=1,arraySize
        DO j=1,100
            ! T + Td...    
			IF(dataArray(i,j,2) < dataArray(i,j,3)) THEN
				errorTrack(i,j,3)=.TRUE.
				errorCount=errorCount+1
			END IF

            ! RH...
            IF(dataArray(i,j,4) > 100 .or. dataArray(i,j,4) < 0 .and.&
dataArray(i,j,4) /= -999) THEN
				errorTrack(i,j,4)=.TRUE.
				errorCount=errorCount+1
			END IF

            ! Wind Direction
            IF(dataArray(i,j,5) > 360 .or. dataArray(i,j,5) < 0 .and. dataArray(i,j,4) /= -999) THEN
			    errorTrack(i,j,5)=.TRUE.
			    errorCount=errorCount+1
			END IF
    
            ! Wind Speed (We're optimistic)
            IF(dataArray(i,j,6) > 150 .or. dataArray(i,j,6) < 0 .and. dataArray(i,j,4) /= -999) THEN
			    errorTrack(i,j,6)=.TRUE.
			    errorCount=errorCount+1
			END IF

            ! 1hr Precip (Considering world record of 12?)
            IF(dataArray(i,j,7) > 13 .or. dataArray(i,j,7) < 0 .and. errorTrack(i,j,7) .neqv. .TRUE. .and.&
dataArray(i,j,4) /= -999) THEN
			    errorTrack(i,j,7)=.TRUE.
			    errorCount=errorCount+1
			END IF

            ! Pressure
            IF(dataArray(i,j,8) > 1100 .or. dataArray(i,j,5) < 800 .and. errorTrack(i,j,8) .neqv. .TRUE. .and.&
dataArray(i,j,4) /= -999) THEN
			    errorTrack(i,j,8)=.TRUE.
			    errorCount=errorCount+1
			END IF

		END DO
	END DO

	! Replace ID'd errors with estimated replacements... (This needs to be an averaging, not freezing
	DO i=1,arraySize
		DO j=2,100
            DO k=2,8
                IF(i == 1) THEN
                    IF(k == 8) THEN
                        IF( dataArray(i,j,8) < 700. .AND. dataArray(i,j,8) > 0. ) THEN
				            dataArray(i,j,8)=1013.25
			            ELSE IF( dataArray(i,j,8) > 1100.0 ) THEN
				            dataArray(i,j,8)=1013.25
			            END IF
                    ELSE
			            IF(errorTrack(i,j,k) .eqv. .TRUE.) THEN
				            IF(errorTrack(i,j+1,k) .eqv. .FALSE.) THEN
					            dataArray(i,j,k)=dataArray(i,j+1,k)
				            ELSE
					            dataArray(i,j,k)=dataArray(i,j+2,k)
				            END IF
			            END IF
                    END IF
                ELSE
                   IF(k == 8) THEN
                        IF( dataArray(i,j,8) < 700. .AND. dataArray(i,j,8) > 0. ) THEN
				            dataArray(i,j,8)=1013.25
			            ELSE IF( dataArray(i,j,8) > 1100.0 ) THEN
				            dataArray(i,j,8)=1013.25
			            END IF
                    ELSE
			            IF(errorTrack(i,j,k) .eqv. .TRUE.) THEN
				            IF(errorTrack(i,j+1,k) .eqv. .FALSE. .and. dataArray(i,j+1,k) /= -999) THEN
					            dataArray(i,j,k)=(dataArray(i,j-1,k)+dataArray(i,j+1,k))/2
				            ELSE
					            dataArray(i,j,k)=dataArray(i,j-1,k)
				            END IF
			            END IF
                    END IF
                END IF
            END DO
		END DO
	END DO
	
	WRITE(*,5001) errorCount,SIZE(dataArray)
5001	FORMAT(/,I5,' errors corrected in ASOS quality control from',I6,' values',//)
	
END SUBROUTINE

!=======================================================
! ASOS Data Wind Direction Mode Calculation
!
! This finds mode wind direction
!
!=======================================================
SUBROUTINE windMode(dataArray,n,dataOut)
	REAL,DIMENSION(n,20,8),INTENT(IN) :: dataArray
	INTEGER,INTENT(IN) :: n

	REAL,DIMENSION(14),INTENT(OUT) :: dataOut

	INTEGER :: i,j,k,mx=0

	REAL,DIMENSION(n,14) :: windArray
	LOGICAL,DIMENSION(n) :: tempArray
	! Mandatory Levels: 925,850,700,500,400,300,250,200,150,100,70,50,30,20 ... No guarantee higher levels are reported
	REAL,DIMENSION(14),PARAMETER :: mandatoryLevels=(/925.,850.,700.,500.,400.,300.,250.,200.,150.,100.,70.,50.,30.,20./)

	windArray=999.	
	dataOut=999.
	DO k=1,14
		DO i=1,n
			DO j=1,20
				IF (dataArray(i,j,1) == mandatoryLevels(k)) THEN
					windArray(i,k)=dataArray(i,j,5)
				END IF
			END DO
		END DO
	END DO
	DO i=1,14
		mx=0
		DO j=0,360
			DO k=1,n
				IF (INT(windArray(k,i)) == j) THEN
					tempArray(k)=.TRUE.
				ELSE
					tempArray(k)=.FALSE.
				END IF
			END DO

			IF (COUNT(tempArray)>mx) THEN
				mx=COUNT(tempArray)
				dataOut(i)=j
			END IF
		END DO
	END DO
END SUBROUTINE

!=======================================================
! ASOS Data Histogram
!
! Fills an ASOS Data Histogram Array
!
!=======================================================
SUBROUTINE asosHistogram(bins,asosData,asosAvg,asosStd,n,times)
	REAL,DIMENSION(n,100,8),INTENT(IN) :: asosData
	REAL,DIMENSION(9),INTENT(IN) :: asosAvg,asosStd
	INTEGER,DIMENSION(n),INTENT(IN) :: times
	INTEGER,INTENT(IN) :: n
	
	INTEGER,DIMENSION(8,2,100),INTENT(OUT) :: bins
	INTEGER :: startValue, endValue, x, i, j, k, xrange

	DO i=3,8
		startValue=INT(asosAvg(i)) - INT(asosStd(i))
		endValue=INT(asosAvg(i)) + INT(asosStd(i))
        xrange = endValue - startValue

        IF(xrange > 100) THEN
            xrange = 100
        END IF

		DO x=1,xrange
			bins(i,1,x) = startValue + (x-1) ! Assign bin to first row of array
			DO j=1,n ! Cycle through Cases and data
				DO k=1,100
					IF(times(j) == asosData(j,k,1) .and. asosData(j,k,1) /= -999.) THEN	
						IF(INT(asosData(j,k,i)) == bins(i,1,x)) THEN
							bins(i,2,x) = bins(i,2,x) + 1
						END IF
					END IF
				END DO
			END DO
		END DO
	END DO

END SUBROUTINE

!=======================================================
! ASOS Delta T Histogram
!
! Fills an ASOS Data Histogram Array
!
!=======================================================
SUBROUTINE asosDTHistogram(bins,asosData,n,times)
	REAL,DIMENSION(n,100,8),INTENT(IN) :: asosData
	INTEGER,DIMENSION(n),INTENT(IN) :: times
	INTEGER,INTENT(IN) :: n
	
	INTEGER,DIMENSION(8,2,100),INTENT(OUT) :: bins
	INTEGER :: startValue, endValue, x, i, j, k

	INTEGER,DIMENSION(n) :: deltaT

	deltaT=0

	DO i=1,n
		DO j=1,100
			IF(asosData(i,j,1) == times(n)) THEN
				deltaT(n) = INT(asosData(i,j,2) - asosData(i,j,3))
			END IF
		END DO	
	END DO

	DO i=1,n
		deltaAvg = deltaAvg + deltaT(n)
	END DO

	deltaAvg = deltaAvg/n
	startValue = INT(deltaAvg)
	endValue = INT(deltaAvg) + 20
	DO j=1,21
	x=startValue + (j-1)
	bins(2,1,j) = x
		DO i=1,n
			DO k=1,100
				IF(INT(asosData(i,k,1)) == times(i) .and. asosData(j,k,1) /= -999.) THEN
					y = asosData(i,k,2) - asosData(i,k,3)
					IF(INT(y) == x) THEN
						bins(2,2,j) = bins(2,2,j) + 1
					END IF
				END IF
			END DO
		END DO
	END DO
END SUBROUTINE

! ------------------------------------------------------------------------------------------------------------------------
! FUNCTIONS
! ------------------------------------------------------------------------------------------------------------------------

! Combine year, month, day strings to create a filename
 CHARACTER(len=8) FUNCTION dateFileName(year,month,day)
	INTEGER,INTENT(IN) :: year,day,month
	CHARACTER(len=4) :: strYear
	CHARACTER(len=2) :: strDay,strMonth

	WRITE(strYear, "(I4)") year

	IF (month < 10) THEN
		WRITE(strMonth, "(A1,I1)") "0", month
	ELSE
		WRITE(strMonth, "(I2)") month
	END IF

	IF (day < 10) THEN
		WRITE(strDay, "(A1,I1)") "0", day		
	ELSE
		WRITE(strDay, "(I2)") day
	END IF
     
	dateFileName = 	strYear//strMonth//strDay

END FUNCTION dateFileName

! Average an array
REAL FUNCTION averageArray(array,n)
	INTEGER,INTENT(IN) :: n
	REAL,DIMENSION(n,100),INTENT(IN) :: array
	INTEGER :: i,total
	total = 0
	DO i=1,n
		DO j=1,100
			IF(array(i,j) < 999) THEN
				total=total+1
			END IF
		END DO
	END DO
	averageArray=SUM(array)/total
END FUNCTION averageArray

! Find the average array using a certain time in dataset (more valid)
REAL FUNCTION eventOnsetAvg(array,n,times,arrayTimes,amUsingDates)
	INTEGER,INTENT(IN) :: n, amUsingDates
	REAL,DIMENSION(n,100),INTENT(IN) :: array, arrayTimes
	INTEGER,DIMENSION(n),INTENT(IN) :: times
	!INTEGER,DIMENSION(n) :: newTimes
	INTEGER :: i,total
	REAL :: summation
	!DO i=1,n
	!	READ(times(i),'(I4)') newTimes(i)
	!END DO
	summation = 0
	total = 0
	DO i=1,n
		DO j=1,100
                IF(array(i,j) /= -999 .and. arrayTimes(i,j) /= -999) THEN ! Check to omit missing/failed data
			        IF(amUsingDates == 1 .and. times(i) == arrayTimes(i,j) .or. amUsingDates == 0) THEN
				        total=total+1
				        summation=summation+array(i,j)
			        END IF
                END IF
		END DO
	END DO
	eventOnsetAvg=summation/total

END FUNCTION

! Find the standard deviation of an array given the average
REAL FUNCTION stdArray(array,avg,n,times,arrayTimes,amUsingDates)
	INTEGER,INTENT(IN) :: n, amUsingDates
	REAL,DIMENSION(n,100),INTENT(IN) :: array, arrayTimes
    INTEGER,DIMENSION(n),INTENT(IN) :: times
	REAL,INTENT(IN) :: avg
	INTEGER :: i,j,c
	REAL :: total
	total=0
	c=0
	DO i=1,n
		DO j=1,100
            IF(array(i,j) /= -999 .and. array(i,j) > -900 .and. arrayTimes(i,j) /= -999 .and. array(i,j) /= 999) THEN ! Check to omit missing/failed data
			    IF(amUsingDates == 1 .and. times(i) == arrayTimes(i,j) .or. amUsingDates == 0) THEN
				    c=c+1
				    total=((array(i,j)-avg)**2)+total
			    END IF
            END IF
		END DO
	END DO
	total=total/c
	stdArray=total**0.5
END FUNCTION stdArray

!Find highest value in array
REAL FUNCTION hiArray(array,n,times,arrayTimes,amUsingDates)
	INTEGER,INTENT(IN) :: n, amUsingDates
	REAL,DIMENSION(n,100),INTENT(IN) :: array, arrayTimes
    INTEGER,DIMENSION(n),INTENT(IN) :: times
	INTEGER :: i,j, countMe
    REAL :: hi
	hi=-1000.
    countMe=0
	DO i=1,n
		DO j=1,100
            IF(array(i,j) /= -999 .and. arrayTimes(i,j) /= -999 .and. array(i,j) /= 999) THEN ! Check to omit missing/failed data
			    IF(amUsingDates == 1 .and. array(i,j) > hi .and. times(i) == arrayTimes(i,j) .or.&
amUsingDates == 0 .and. array(i,j) > hi) THEN
				    hi=array(i,j)
			    END IF
            END IF
		END DO
	END DO
	hiArray = hi
END FUNCTION

!Find lowest value in array
REAL FUNCTION loArray(array,n,times,arrayTimes,amUsingDates)
	INTEGER,INTENT(IN) :: n, amUsingDates
	REAL,DIMENSION(n,100),INTENT(IN) :: array, arrayTimes
    INTEGER,DIMENSION(n),INTENT(IN) :: times
	INTEGER :: i,j
    REAL :: lo
	lo=2000.
	DO i=1,n
		DO j=1,100
            IF(array(i,j) /= -999 .and. array(i,j) > -900 .and. arrayTimes(i,j) /= -999 .and. array(i,j) /= 999) THEN
			    IF(amUsingDates == 1 .and. array(i,j) < lo .and. times(i) == arrayTimes(i,j) .or.&
amUsingDates == 0 .and. array(i,j) < lo) THEN
				    lo=array(i,j)
			    END IF
            END IF
		END DO
	END DO
	loArray = lo
END FUNCTION


