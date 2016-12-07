;**************************************************
; sounding.ncl
;
; AUTHOR: Warren Pettee
; DATE  : 21-April-2016
;
; This NCAR Command Language program will make a nice
; sounding from the mean data given by the Automated
; Storm Event Climatology Builder
;
;*************************************************
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/gsn_code.ncl"
load "$NCARG_ROOT/lib/ncarg/nclscripts/csm/skewt_func.ncl"
;***********************************************
 begin

  diri = "./"
  fili = "meanSound.csv"
  nlvl = 14
  ncol = 8
  TestData = asciiread (diri+fili , (/nlvl,ncol/), "float") 

  p    = TestData (:,0)
  z    = TestData (:,1)
  q    = TestData (:,5)
  tc   = TestData (:,2)
  tdc  = TestData (:,3)
  wspd = TestData (:,7)
  wdir = TestData (:,6)
 
;*************************
; create plot
;*************************
  wks  = gsn_open_wks ("pdf", "skewt")

; --- Create background skew-T and plot sounding + winds----------------
  skewtOpts                 = True
  ;skewtOpts@DrawColAreaFill = True    ; default is False

                                ; Create a few "pibal" reports
  ;hght = (/ 1000., 3000., 7000., 25000. /)/3.208  ; hgt in M  
  ;hspd = (/   50.,   27.,  123.,    13. /) ;speed at each height
  ;hdir = (/   95.,  185.,  275.,   355. /) ;direction

  dataOpts           = True     ; options describing data and plotin
 ;dataOpts@PlotWindH = True     ; if available, plot wind barbs at height lvls
  ;dataOpts@HspdHdir  = True     ; wind speed and dir [else: u,v]
  dataOpts@colTemperature = "red"   ; default -> "Foreground"
  dataOpts@colDewPt       = "green"   ; default -> "RoyalBlue"

  ;dataOpts@Height    = hght     ; height of wind reports
  ;dataOpts@Hspd      = hspd     ; speed [or u component]
  ;dataOpts@Hdir      = hdir     ; dir   [or v component]

  skewtOpts@tiMainString = "Average Sounding" 
  skewtOpts@DrawFahrenheit=False
  skewt_bkgd = skewT_BackGround (wks, skewtOpts)
  skewt_data = skewT_PlotData   (wks, skewt_bkgd, p,tc,tdc,z \
                                    , wspd,wdir, dataOpts)
  draw (skewt_bkgd)
  draw (skewt_data)
  frame(wks)

 end
