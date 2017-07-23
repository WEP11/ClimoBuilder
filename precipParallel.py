'''
precip.py
This script will pull all ASOS data, ID peak fog events, then find the
6,12,and 24 hr precip values
Secondarily it will ID fog temperature rates before onset.
'''

import matplotlib
matplotlib.use('Agg') #headless script
import matplotlib.pyplot as plt
import csv
import time

from multiprocessing import Process
from multiprocessing import Queue

from datetime import datetime
from datetime import timedelta
from dateutil.relativedelta import relativedelta

import numpy as np

'''
findTimeDiff : Later Time , Starting Time (Both are datetime objects)
This will find the total difference in time (hours) between
two datetime objects.
'''
def findTimeDiff(endTime,startTime):
    timeDiffYears = relativedelta(startTime,endTime).years
    timeDiffMonths = relativedelta(startTime,endTime).months
    timeDiffDays = relativedelta(startTime,endTime).days
    timeDiffHours = relativedelta(startTime,endTime).hours
    timeDiffMinutes = relativedelta(startTime,endTime).minutes

    years = timeDiffYears * 365.0 * 24.0
    months = timeDiffMonths * 30.0 * 24.0
    days = timeDiffDays * 24.0
    minutes = timeDiffMinutes / 60.0
    hours = timeDiffHours + years + months + days + minutes
    
    return hours

'''
findHourPcp : Case Number, Precip Values List, Timestamp List, Precip Accumulation Time (hours)
This finds the total precip for a given case for a given accumulation time.
'''
def findHourPcp(caseNum,precipList,timestampList,hourInterval) :
    i = caseNum    
    totalSix = 0
    startTime = datetime.strptime(timeStamps[caseNum], '%Y-%m-%d %H:%M')

    for pcp in reversed(precipList[:caseNum]):
        endTime = datetime.strptime(timestampList[i], '%Y-%m-%d %H:%M')

        hours = findTimeDiff(endTime,startTime)
        if ((hours > 0) and (hours <= hourInterval) and (pcp != 99999.0)):          
            totalSix = pcp + totalSix
        elif (hours > hourInterval):
            break

        i -= 1
    return totalSix

'''
findMonthlyPcp : Month Index, Date List, Times List, Precip Values List, Case ID List, Time Stamp List
This will return the 24 hour box plots for a single month as a matplotlib axarr object
'''
def findMonthlyPcp(month,date,time,precip,caseIDs,timeStamps,data_queue):
    monthName = ['Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec']
    hours = np.arange(24)
    print("Building Monthly Totals for ", monthName[month])
    monthCount = month + 1
    hourly = []
    for hour in hours: 
        data = []     
        for num in caseIDs:
            day = date[num]
            clock = time[num]
            caseMon = int(day[5:7])
            if caseMon == monthCount and int(clock[0:2]) == hour:
                precipTotal = findHourPcp(num,precip,timeStamps,24)
                data.append(precipTotal)
        hourly.append(data)
    data_queue.put(hourly)
    #print(hourly)
    return

'''
Main Program
'''
#Initialize lists...
timeStamps = []
date = []
time = []
temp = []
dewp = []
rh = []
winddir = []
windspd = []
pres = []
vis = []
metar = []
precip = []

# Read da data...
print("Reading Data...")
with open('asosData.SFC', 'r') as csvfile:
    reader = csv.reader(csvfile, delimiter=',')
    # Skip Header ...
    for i in range(6):
        next(reader)
    for row in reader:
        timeStamp = row[1]
        timeStamps.append(row[1])
        date.append(timeStamp[0:10])
        time.append(timeStamp[-5:])

        if row[2] != "M":
            temp.append(float(row[2]))
        else:
            temp.append(99999.0)

        if row[3] != "M":
            dewp.append(float(row[3]))
        else:
            dewp.append(99999.0)

        if row[4] != "M":
            rh.append(float(row[4]))
        else:
            rh.append(99999.0)

        if row[5] != "M":
            winddir.append(float(row[5]))
        else:
            winddir.append(99999.0)

        if row[6] != "M":
            windspd.append(float(row[6]))
        else:
            windspd.append(99999.0)

        if row[9] != "M":
            pres.append(float(row[9]))
        else:
            pres.append(99999.0)

        if row[10] != "M":
            vis.append(float(row[10]))
        else:
            vis.append(99999.0)
        
        if row[20] != "M":
            metar.append(row[20])
        else:
            metar.append(99999.0)

        if row[7] != "M":
            precip.append(float(row[7]))
        else:
            precip.append(99999.0)

# Find fog onsets...
print("Find fog cases and onset times...")
caseIDs = []
i=0
cases = 0
start = datetime.strptime(timeStamps[0], '%Y-%m-%d %H:%M')
for text in metar:

    end = datetime.strptime(timeStamps[i], '%Y-%m-%d %H:%M')
    timeDiff = findTimeDiff(start,end)
    
    if text == "FG" and vis[i] <= 0.62 and timeDiff > 12:
        caseIDs.append(i)
        start = datetime.strptime(timeStamps[i], '%Y-%m-%d %H:%M')
        cases = cases + 1
    i += 1
print(cases, " Cases were found!")

# Find 6 hour precip values preceding fog onsets...
print("Building 6-Hour Precip Values ...")
monthData = []

q1 = Queue()
q2 = Queue()
q3 = Queue()
q4 = Queue()
data1 = Process(target=findMonthlyPcp, args=(0,date,time,precip,caseIDs,timeStamps,q1))
data2 = Process(target=findMonthlyPcp, args=(1,date,time,precip,caseIDs,timeStamps,q2))
data3 = Process(target=findMonthlyPcp, args=(2,date,time,precip,caseIDs,timeStamps,q3))
data4 = Process(target=findMonthlyPcp, args=(3,date,time,precip,caseIDs,timeStamps,q4))

data1.start()
data2.start()
data3.start()
data4.start()

data1.join()
data2.join()
data3.join()
data4.join()

monthData.append(q1.get())
monthData.append(q2.get())
monthData.append(q3.get())
monthData.append(q4.get())

q5 = Queue()
q6 = Queue()
q7 = Queue()
q8 = Queue()
data5 = Process(target=findMonthlyPcp, args=(4,date,time,precip,caseIDs,timeStamps,q5))
data6 = Process(target=findMonthlyPcp, args=(5,date,time,precip,caseIDs,timeStamps,q6))
data7 = Process(target=findMonthlyPcp, args=(6,date,time,precip,caseIDs,timeStamps,q7))
data8 = Process(target=findMonthlyPcp, args=(7,date,time,precip,caseIDs,timeStamps,q8))

data5.start()
data6.start()
data7.start()
data8.start()

data5.join()
data6.join()
data7.join()
data8.join()

monthData.append(q5.get())
monthData.append(q6.get())
monthData.append(q7.get())
monthData.append(q8.get())

q9 = Queue()
q10 = Queue()
q11 = Queue()
q12 = Queue()
data9 = Process(target=findMonthlyPcp, args=(8,date,time,precip,caseIDs,timeStamps,q9))
data10 = Process(target=findMonthlyPcp, args=(9,date,time,precip,caseIDs,timeStamps,q10))
data11 = Process(target=findMonthlyPcp, args=(10,date,time,precip,caseIDs,timeStamps,q11))
data12 = Process(target=findMonthlyPcp, args=(11,date,time,precip,caseIDs,timeStamps,q12))

data9.start()
data10.start()
data11.start()
data12.start()

data9.join()
data10.join()
data11.join()
data12.join()

monthData.append(q9.get())
monthData.append(q10.get())
monthData.append(q11.get())
monthData.append(q12.get())

print("Making the plot...")
#print(monthData)
f, axarr = plt.subplots(6,2)
monthName = ['Jan','Feb','Mar','Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec']
months = np.arange(12)
hours = np.arange(24)

for month in months:

    monthCount = month + 1

    if monthCount%2 > 0:
        i = monthCount / 2
        j = 0
    else:
        i = (monthCount / 2) - 1
        j = 1

    axarr[i,j].set_ylim(0, 1)
    axarr[i,j].boxplot(monthData[month],0,'r.',1)
    axarr[i,j].set_title(monthName[monthCount-1])
    axarr[i,j].set_xticklabels(hours)

f.subplots_adjust(hspace=0.5,wspace=0.25)
f.suptitle('24 Hour Precip Preceding Fog Onset', fontsize=20)
f.set_figheight(11)
f.set_figwidth(17)
#plt.show()
filename = '24hrPrecip.pdf'
plt.savefig(filename,orientation='landscape',dpi=199)
