/astro                                                              October 2009

This directory includes about 80 mostly self-contained astronomy utilities.

The procedure UVBYBETA uses the non-standard system variable !TEXTOUT.
The procedure ASTROLIB can be used to add this system variable to one's
session.

The routines JPLEPHREAD and JPLEPHINTERP require a FITS file containing a JPL
(Jet Propulsion Laboratory) ephemeris for solar system bodies.    A copy of the
DE405 ephemeris FITS file  JPLEPH.405 is available in
http://idlastro.gsfc.nasa.gov/ftp/data/ but note that this file is not included
in the IDL Astronomy Library tar files. 

Additional time conversion procedures such as those to convert between 
Julian date and International Atomic Time (TAI) are located in 
 http://sohowww.nascom.nasa.gov/solarsoft/gen/idl/time
Also see the utility procedure CMSYSTIME() in 
http://astrog.physics.wisc.edu/~craigm/idl/misc.html#CMSYSTIME

Astronomical Utilities      pro/astro
________
A_b() - Compute B band interstellar extinction as a function of Galactic coords.
ADSTRING() - Format RA and DEC as a character string
AIRTOVAC - Convert air wavelengths to vacuum wavelengths
AITOFF - Convert longitude,latitude to X,Y using Aitoff equal-area projection
AITOFF_GRID - Create an overlay grid using the AITOFF projection 
ALTAZ2HADEC - Convert Horizon (Alt-Az) coordinates to Hour Angle and Declination
ARCBAR - Draw an arcbar over an image showing the astronomical plate scale
ARROWS - Given a FITS header, display a "weathervane" showing N-E orientation 
ASTDISP - Display formatter for pixel + astronomical coordinates
ASTRO - Interactive driver to compute astronomical precession,
        or coordinate conversions (calls EULER and PRECESS).
BARYVEL - Compute components of barycentric Earth velocity, given Julian date
BPRECESS - Precess coordinates, proper motion from J2000 to B1950
CALZ_UNRED - Deredden a galaxy spectrum using the Calzetti et al. (2000) formula
CCM_UNRED - Deredden a spectrum using the Cardelli, Clayton and Mathis (1989)
         parameterization.
CO_ABERRATION - Calculate changes to Ra and Dec due to aberration effects
CO_NUTATE - Calculate changes in RA and Dec due to nutation of the Earth's rotation
CO_REFRACT() - Calculate correction to altitude due to atmospheric refraction
COSMO_PARAM - Derive a full set of cosmological parameters given a subset
CT2LST- Convert from civil time to local sidereal time
DATE()  - Convert day of year to a DY-MON-CCYY string (FITS standard)
DATE_CONV() - Function to perform various date format conversions
DAYCNV- Convert from Julian Date to calendar date.
DEREDD- Deredden Stromgren indices (called by UVBYBETA)
ECI2GEO() - Convert Earth-centered inertial coordinates to geographic  coords
EQ2HOR - Convert celestial  (ra-dec) coords to local horizon coords (alt-az).
EQPOLE - Convert longitude,latitude to X,Y using polar equal-area projection
EQPOLE_GRID - Create overlay grid using polar equal-area projection
EULER - Astronomical coordinate system conversions
FLUX2MAG() - Convert from flux units to magnitudes
FM_UNRED - Deredden a spectrum using the Fitzpatrick & Massa (1998)
         parameterization.
GAL_UVW - Calculate the Galactic space velocity (U,V,W) of a star
GAL_FLAT()  - Correct a galaxy image for inclination effects.
GALAGE - Derive a galaxy age as a function of redshift for a cosmological model
GCIRC - Compute rigorous great circle distance
GEO2ECI() - Convert geographic coordinates to Earth-centered inertial coords 
GEO2GEODETIC() - Convert from geographic to geodetic coordinates
GEO2MAG() - Convert from geographic to geomagnetic coordinates
GEODETIC2GEO() - Convert from geodetic to geographic coordinates
GET_COORDS - Read in angular input in decimal or sexigesimal format
GET_DATE - Get the current date in CCYY-MM-DD format (FITS standard)
GET_JULDATE - Get the current Julian date as a double precision scalar
GLACTC- Convert between Galactic and equatorial coordinates at any 
        equinox   
GLACTC_PM - Convert between celestial and Galactic (or Supergalactic) proper motion
HADEC2ALTAZ -  Converts Hour Angle and Declination to Horizon (alt-az) coordinates                                 
HELIO - Compute (low-precision) heliocentric coordinates of the planets 
HELIO_JD() - Convert geocentric (reduced) Julian date to heliocentric Julian date
HELIO_RV() - Compute radial velocity given binary star orbit parameters
HOR2EQ - Convert local horizon coords (alt-az) to equatorial (ra-dec).
IMCONTOUR - Contour plots with astronomical labeling (either RA,Dec or 
            arc distance from the image center
IMF() - Return values for a multi-component power law initial mass function
ISMEUV() - Compute EUV optical depth due to photoionization of HI, HeI and HeII
JDCNV - Convert from calendar date to Julian date.
JPLEPHINTERP - Interpolate position and motion of planetary bodies (JPL Ephemeris)
JPLEPHREAD - Open and read JPL DE200 or DE405 Ephemeride FITS File
JPRECESS - Precess positions & proper motions from B1950 to J2000
JULDATE-Convert from calendar date to reduced Julian date. 
LSF_ROTATE - Create a 1-d convolution kernel to broaden a spectrum from a rotating star
LUMDIST - Return luminosity distance for a given redshift & cosmological model
MAG2GEO() - Convert from geomagnetic to geographic coordinates
MAG2FLUX() - Convert from magnitudes to flux units
MONTH_CNV() Convert a month name to the equivalent number or vice-versa 
MOONPOS- Compute the RA and Dec (and distance) of the Moon at a given date
MPHASE - Compute illuminated fraction of the Moon's disk for given Julian dates
NUTATE - Compute the nutation in longitude and latitude for given Julian date(s)
OBSERVATORY - Return coordinates, altitude & time zones of an observatory
PLANCK() - Returns a blackbody flux for a given effective temperature
PLANET_COORDS - Return low-precision RA and Dec of planets give a date(s)
POSANG - Compute the position angle between sources of specified RA and Dec
PRECESS - Precess RA and Dec to a new equinox
PRECESS_CD - Precess the PC (or CD) matrix in a FITS header to a new equinox
PRECESS_XYZ - Precess equatorial geocentric rectangular coordinates
PREMAT() - Returns precession matrix from equinox 1 to equinox 2
QDCB_GRID - Create overlay grid using COBE quad cube database coordinates
RADEC - Format RA, Dec as Hours,Min,Sec,Deg,Min,Sec
REDSHIFT - Interactively convert between redshift, distance, and velocity
           (in /jhuapl)
SIXTY - Convert decimal number to sexigesimal                 
SPHDIST() - Find angular distance on a sphere (in /jhuapl)
SUNPOS - Compute the RA and Dec of the Sun at a given date
TDB2TDT() - Relativistic clock corrections due to Earth motion in solar system
TEN() - Convert sexigesimal number to decimal             
TENV() -  Like TEN but will work on a vector of sexigesimal numbers.
TICPOS - Specify distance between tic marks for astronomical coordinates
TICLABELS - Create labels for astronomical coordinate tick marks 
TICS - Compute the optimum distance between tic marks for astronomical labeling
TIC_ONE - Determine optimum position of the first tic in astronomical labeling  
UVBYBETA - Use Stromgren indices to derive dereddened colors, metallicity,
        and effective Temperature.
VACTOAIR - Convert vacuum wavelengths to air wavelengths.
WFPC2_METRIC -  Compute the distortion in a WFPC2 image and return coordinates
XYZ - Compute heliocentric rectangular coordinates at given Julian date.
YMD2DN() - Convert year,month,day to day number of the year (in /jhuapl)
YDN2MD - Convert day number of the year to year, month,day
ZANG() - Compute angular size as a function of redshift in a Friedman cosmology
ZENPOS - Compute the RA and Dec of the local zenith at a given date
