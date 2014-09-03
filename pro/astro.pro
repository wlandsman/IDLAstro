pro astro, selection, EQUINOX = equinox, FK4 = FK4   
;+
; NAME:
;     ASTRO
; PURPOSE:
;     Interactive utility for precession and coordinate conversion.
;
; CALLING SEQUENCE:
;     ASTRO, [ selection, EQUINOX =, /FK4]
;
; OPTIONAL INPUT:
;      SELECTION - Scalar Integer (0-6) giving the the particular astronomical
;              utility to be used.  (0) Precession, (1) RA, Dec (2000) to Galactic 
;              coordinates, (2) Galactic to RA,Dec (2000) (3) RA,Dec (2000) to 
;              Ecliptic, (4) Ecliptic to RA, Dec, (5) Ecliptic to Galactic, (6) Galactic
;              to Ecliptic.   Program will prompt for SELECTION if this 
;              parameter is omitted.
;
; OPTIONAL KEYWORD INPUT:
;       EQUINOX - numeric scalar specifying the equinox to use when converting 
;               between celestial and other coordinates.    If not supplied, 
;               then the RA and Dec will be assumed to be in EQUINOX J2000.   
;               This keyword is ignored by the precession utility.   For 
;               example, to convert from RA and DEC (J1975) to Galactic 
;               coordinates:
;
;               IDL> astro, 1, E=1975
;       /FK4 - If this keyword is set and nonzero, then calculations are done
;              in the FK4 system.    For example, to convert from RA and Dec
;              (B1975) to Galactic coordinates
;
;               IDL> astro,1, E=1975,/FK4 
; METHOD:
;      ASTRO uses PRECESS to compute precession, and EULER to compute
;      coordinate conversions.   The procedure GET_COORDS is used to
;      read the coordinates, and ADSTRING to format the RA,Dec output.
;
; NOTES:
;      (1) ASTRO temporarily sets !QUIET to suppress compilation messages and
;      keep a pretty screen display.   
;
;      (2) ASTRO was changed in December 1998 to use J2000 as the default 
;      equinox, **and may be incompatible with earlier calls.***
;      
;      (3) A nice online page for coordinate conversions is available at
;       http://heasarc.gsfc.nasa.gov/cgi-bin/Tools/convcoord/convcoord.pl   
; PROCEDURES USED:
;      Procedures: GET_COORDS, EULER       Function: ADSTRING
; REVISION HISTORY
;      Written, W. Landsman November 1987
;      Code cleaned up       W. Landsman   October 1991
;      Added Equinox keyword, call to GET_COORDS, W. Landsman   April, 1992
;      Allow floating point equinox input J. Parker/W. Landsman  July 1996
;      Make FK5 the default, add FK4 keyword
;-
 On_error,2                    ;Return to caller

 input_type =   [0,0,1,0,2,2,1]     ;0= RA,Dec  1= Galactic   2 = Ecliptic
 output_type =  [0,1,0,2,0,1,2]        

 sv_quiet = !quiet & !quiet = 1 ;Don't display compiled procedures


 if keyword_set(FK4) then begin
       if not keyword_set(EQUINOX) then equinox = 1950
       fk = 'B'
       ref_year = 1950  
       yeari = 1950 & yearf = 1950
 endif else begin
       if not keyword_set(EQUINOX) then equinox = 2000
       fk = 'J'  
       ref_year = 2000 
       yeari = 2000 & yearf = 2000
 endelse
      eqname = fk + string(equinox,f='(f6.1)') + ')'

 select = ['(0) Precession: (RA, Dec)',                  $
           '(1) Conversion: (RA, Dec ' + eqname + ' --> Galactic', $
           '(2) Conversion: Galactic --> (RA, Dec ' + eqname, $
           '(3) Conversion: (RA, Dec ' + eqname + ' --> Ecliptic', $
           '(4) Conversion: Ecliptic --> (RA, Dec ' + eqname, $
           '(5) Conversion: Ecliptic --> Galactic',       $
           '(6) Conversion: Galactic --> Ecliptic']

 npar = N_params()       

 SELECTOR: if (npar EQ 0 ) then begin

        print,'Select astronomical utility'
        for i = 0,6 do print, select[i]
        selection = 0
        print,' '
        read,'Enter Utility Number: ',selection 
        print,' '

     endif

 if ( selection LT 0 ) or ( selection GT 6 ) then begin

       print,selection,' is not an available option'
       npar = 0
       goto, SELECTOR

 endif

 print, select[selection]

 if keyword_set(EQUINOX) and (input_type[selection] EQ 0) then yeari =equinox
 if keyword_set(EQUINOX) and (output_type[selection] EQ 0) then yearf = equinox

 if ( selection EQ 0 ) then read, $
     'Enter initial and final equinox (e.g. 1975,2000): ',yeari,yearf


 case output_type[selection] of

   0:  OutName = " RA Dec (" + fk + string( yearf, f= "(F6.1)" ) + "):  "
   1:  OutName = " Galactic longitude and latitude: "
   2:  OutName = " Ecliptic longitude and latitude: (" +  $
                  fk + string( yearf, f= "(F6.1)" ) + ")"
 endcase 

 case input_type[selection] of 

  0:  InName = "RA Dec (" + fk + string(yeari ,f ='(F6.1)' ) + ')'
  1:  InName = "Galactic longitude and latitude: "
  2:  InName = "Ecliptic longitude and latitude: (" + fk + $
                string(yeari ,f ='(F6.1)' ) + ')'

 endcase
 
 HELP_INP: if ( input_type[selection] EQ 0 ) then begin

  print,format='(/A)',' Enter RA, DEC with either 2 or 6 parameters '
  print,format='(A/)',' Either RA, DEC (degrees) or HR, MIN, SEC, DEG, MIN SEC'

 endif

 READ_INP: 

     get_coords,coords,'Enter '+ InName, Numcoords 

 if ( coords[0] EQ -999 ) then begin        ;Normal Return
        print,' '
        if Numcoords GT 0 then goto, READ_INP
        !quiet = sv_quiet
        return
 endif

 ra = coords[0] & dec = coords[1]
 if Numcoords EQ 6 then ra = ra*15.

 if ( selection EQ 0 ) then begin 

         precess, ra , dec , yeari, yearf, FK4 = fk4    ;Actual Calculations
         newra = ra & newdec = dec

 endif else begin 
       if yeari NE ref_year then precess, ra, dec, yeari, ref_year,FK4=fk4
       euler, ra, dec, newra, newdec, selection, fk4 = FK4
       if yearf NE ref_year then precess, newra,newdec, ref_year, yearf,FK4=fk4
 endelse

 if newra LT 0 then newra = newra + 360.

 if output_type[selection] EQ 0 then $
     print, outname + adstring( [newra,newdec], 1) $

 else  print, FORM = '(A,2F7.2,A,F7.2 )', $
      outname, newra, newdec

 print,' '
 goto, READ_INP      

 end            
