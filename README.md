# ClimoBuilder

An automated long-term climatology builder I made that will work for any upper air station globally. Yeah, yeah, I know it's in Fortran. But it's also in BASH. And NCL. But the point is that it works! Anybody like Python? I might swap the plotting over to MetPy but I'm taking care of core functionality first. More to come soon!

## Installing

To use this you'll need a fortran compiler, NCL, and the ability to execute BASH scripts. Right now the fog detection script is the only one that will work without some editing. I plan to get it universally working soon.

1. Download everything
2. Run `gfortran climoBuilder.f90 -o climoBuilder`
3. Download some data from IA State's ASOS archive for the time periods you want
4. Run the fogDetect script: `./fogDetect.sh <ASOS_ICAO(CAR)> <Start Year> <End Year> [-i]`
  * Use 3-letter ID's for U.S. sites and 4-letters for international. If the site is outside of the U.S. also include the -i flag.
5. Now enjoy your output
