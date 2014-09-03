pro nutate, jd, nut_long, nut_obliq
;+
; NAME:
;       NUTATE
; PURPOSE:
;       Return the nutation in longitude and obliquity for a given Julian date
;
; CALLING SEQUENCE:
;       NUTATE, jd, Nut_long, Nut_obliq
;
; INPUT:
;       jd - Julian ephemeris date, scalar or vector, double precision  
; OUTPUT:
;       Nut_long - the nutation in longitude, same # of elements as jd
;       Nut_obliq - nutation in latitude, same # of elements as jd
;
; EXAMPLE:
;       (1) Find the nutation in longitude and obliquity 1987 on Apr 10 at Oh.
;              This is example 22.a from Meeus
;        IDL> jdcnv,1987,4,10,0,jul
;        IDL> nutate, jul, nut_long, nut_obliq
;             ==> nut_long = -3.788    nut_obliq = 9.443
;            
;       (2) Plot the large-scale variation of the nutation in longitude 
;               during the 20th century
;
;       IDL> yr = 1900 + indgen(100)     ;Compute once a year        
;       IDL> jdcnv,yr,1,1,0,jul          ;Find Julian date of first day of year
;       IDL> nutate,jul, nut_long        ;Nutation in longitude
;       IDL> plot, yr, nut_long
;
;       This plot will reveal the dominant (18.6 year) period, but a finer
;       grid is needed to display the shorter periods in the nutation.
; METHOD:
;       Uses the formula in Chapter 22 of ``Astronomical Algorithms'' by Jean 
;       Meeus (1998, 2nd ed.) which is based on the 1980 IAU Theory of Nutation
;       and includes all terms larger than 0.0003".
;
; PROCEDURES CALLED:
;       POLY()                       (from IDL User's Library)
;       CIRRANGE, ISARRAY()          (from IDL Astronomy Library)
;
; REVISION HISTORY:
;       Written, W.Landsman (Goddard/HSTX)      June 1996       
;       Converted to IDL V5.0   W. Landsman   September 1997
;       Corrected minor typos in values of d_lng W. Landsman  December 2000
;       Updated typo in cdelt term              December 2000
;       Avoid overflow for more than 32767 input dates W. Landsman January 2005
;-
 compile_opt idl2
 On_error,2
 
 if N_params() LT 2 then begin
        print,'Syntax - NUTATE, jd, nut_long, nut_obliq'
        return
 endif

 dtor = !DPI/180.0d
 ;  form time in Julian centuries from 1900.0

 t = (jd[*] - 2451545.0d)/36525.0d0


; Mean elongation of the Moon

   coeff1 = [297.85036d,  445267.111480d, -0.0019142, 1.d/189474d0 ]
  d = poly(T, coeff1)*dtor
  cirrange,d,/rad

; Sun's mean anomaly

   coeff2 = [357.52772d, 35999.050340d, -0.0001603d, -1.d/3d5 ]
   M = poly(T,coeff2)*dtor
   cirrange, M,/rad

; Moon's mean anomaly

   coeff3 = [134.96298d, 477198.867398d, 0.0086972d, 1.0/5.625d4 ]
   Mprime = poly(T,coeff3)*dtor
   cirrange, Mprime,/rad

; Moon's argument of latitude

    coeff4 = [93.27191d, 483202.017538d, -0.0036825, -1.0d/3.27270d5 ]
    F = poly(T, coeff4 )*dtor 
    cirrange, F,/RAD

; Longitude of the ascending node of the Moon's mean orbit on the ecliptic,
;  measured from the mean equinox of the date

  coeff5 = [125.04452d, -1934.136261d, 0.0020708d, 1.d/4.5d5]
  omega = poly(T, coeff5)*dtor
  cirrange,omega,/RAD

 d_lng = [0,-2,0,0,0,0,-2,0,0,-2,-2,-2,0,2,0,2,0,0,-2,0,2,0,0,-2,0,-2,0,0,2,$
   -2,0,-2,0,0,2,2,0,-2,0,2,2,-2,-2,2,2,0,-2,-2,0,-2,-2,0,-1,-2,1,0,0,-1,0,0, $
     2,0,2]

 m_lng = [0,0,0,0,1,0,1,0,0,-1,intarr(17),2,0,2,1,0,-1,0,0,0,1,1,-1,0, $
  0,0,0,0,0,-1,-1,0,0,0,1,0,0,1,0,0,0,-1,1,-1,-1,0,-1]

 mp_lng = [0,0,0,0,0,1,0,0,1,0,1,0,-1,0,1,-1,-1,1,2,-2,0,2,2,1,0,0,-1,0,-1, $
   0,0,1,0,2,-1,1,0,1,0,0,1,2,1,-2,0,1,0,0,2,2,0,1,1,0,0,1,-2,1,1,1,-1,3,0]

 f_lng = [0,2,2,0,0,0,2,2,2,2,0,2,2,0,0,2,0,2,0,2,2,2,0,2,2,2,2,0,0,2,0,0, $
   0,-2,2,2,2,0,2,2,0,2,2,0,0,0,2,0,2,0,2,-2,0,0,0,2,2,0,0,2,2,2,2]

 om_lng = [1,2,2,2,0,0,2,1,2,2,0,1,2,0,1,2,1,1,0,1,2,2,0,2,0,0,1,0,1,2,1, $
   1,1,0,1,2,2,0,2,1,0,2,1,1,1,0,1,1,1,1,1,0,0,0,0,0,2,0,0,2,2,2,2]

 sin_lng = [-171996, -13187, -2274, 2062, 1426, 712, -517, -386, -301, 217, $
    -158, 129, 123, 63, 63, -59, -58, -51, 48, 46, -38, -31, 29, 29, 26, -22, $
     21, 17, 16, -16, -15, -13, -12, 11, -10, -8, 7, -7, -7, -7, $
     6,6,6,-6,-6,5,-5,-5,-5,4,4,4,-4,-4,-4,3,-3,-3,-3,-3,-3,-3,-3 ]
 
 sdelt = [-174.2, -1.6, -0.2, 0.2, -3.4, 0.1, 1.2, -0.4, 0, -0.5, 0, 0.1, $
     0,0,0.1, 0,-0.1,dblarr(10), -0.1, 0, 0.1, dblarr(33) ] 


 cos_lng = [ 92025, 5736, 977, -895, 54, -7, 224, 200, 129, -95,0,-70,-53,0, $
    -33, 26, 32, 27, 0, -24, 16,13,0,-12,0,0,-10,0,-8,7,9,7,6,0,5,3,-3,0,3,3,$
     0,-3,-3,3,3,0,3,3,3, intarr(14) ]

 cdelt = [8.9, -3.1, -0.5, 0.5, -0.1, 0.0, -0.6, 0.0, -0.1, 0.3, dblarr(53) ]


; Sum the periodic terms 

 n = N_elements(jd)
 nut_long = dblarr(n)
 nut_obliq = dblarr(n)
 arg = d_lng#d + m_lng#m +mp_lng#mprime + f_lng#f +om_lng#omega
 sarg = sin(arg)
 carg = cos(arg)
 for i=0L,n-1 do begin
        nut_long[i] =  0.0001d*total( (sdelt*t[i] + sin_lng)*sarg[*,i] )
        nut_obliq[i] = 0.0001d*total( (cdelt*t[i] + cos_lng)*carg[*,i] )
 end
 if ~isarray(jd) then begin
        nut_long = nut_long[0]
        nut_obliq = nut_obliq[0]
 endif

 return
 end
