pro observatory,obsname,obs_struct, print = print
;+
; NAME:
;       OBSERVATORY
; PURPOSE:
;       Return longitude, latitude, altitude & time zones of an observatory
; EXPLANATION:
;       Given an observatory name, returns a structure giving the longitude,
;       latitude, altitude, and time zone 
;
; CALLING SEQUENCE:
;       Observatory, obsname, obs_struct, [ /PRINT ]
;
; INPUTS:
;       obsname - scalar or vector string giving abbreviated name(s) of 
;             observatories for which location or time information is requested.
;             If obsname is an empty string, then information is returned for 
;             all observatories in the database.     See the NOTES: section
;             for the list of 41 recognized observatories.   The case of the 
;             string does not matter  
; OUTPUTS:
;       obs_struct - an IDL structure containing information on  the specified
;                 observatories.   The structure tags are as follows: 
;       .observatory - abbreviated observatory name
;       .name - full observatory name  
;       .longitude - observatory longitude in degrees *west* 
;       .latitude - observatory latitude in degrees
;       .altitude - observatory altitude in meters above sea level
;       .tz - time zone, number of hours *west* of Greenwich
;
; OPTIONAL INPUT KEYWORD:
;     /PRINT - If this keyword is set, (or if only 1 parameter is supplied)
;             then OBSERVATORY will display information about the specified
;             observatories at the terminal
; EXAMPLE:
;     Get the latitude, longitude and altitude of Kitt Peak National Observatory
;
;     IDL> observatory,'kpno',obs
;     IDL> print,obs.longitude  ==> 111.6 degrees west 
;     IDL> print,obs.latitude  ==> +31.9633 degrees
;     IDL> print,obs.altitude  ==> 2120 meters above sea level
;
; NOTES:
;   Observatory information is taken from noao$lib/obsdb.dat file in IRAF 2.11
;   Currently recognized observatory names are as follows:
;
;  'kpno': Kitt Peak National Observatory
;  'ctio': Cerro Tololo Interamerican Observatory
;  'eso': European Southern Observatory
;  'lick': Lick Observatory
;  'mmto': MMT Observatory
;  'cfht': Canada-France-Hawaii Telescope
;  'lapalma': Roque de los Muchachos, La Palma
;  'mso': Mt. Stromlo Observatory
;  'sso': Siding Spring Observatory
;  'aao': Anglo-Australian Observatory
;  'mcdonald': McDonald Observatory
;  'lco': Las Campanas Observatory
;  'mtbigelow': Catalina Observatory: 61 inch telescope
;  'dao': Dominion Astrophysical Observatory
;  'spm': Observatorio Astronomico Nacional, San Pedro Martir
;  'tona': Observatorio Astronomico Nacional, Tonantzintla
;  'Palomar': The Hale Telescope
;  'mdm': Michigan-Dartmouth-MIT Observatory
;  'NOV': National Observatory of Venezuela
;  'bmo': Black Moshannon Observatory
;  'BAO': Beijing XingLong Observatory
;  'keck': W. M. Keck Observatory
;  'ekar': Mt. Ekar 182 cm. Telescope
;  'apo': Apache Point Observatory
;  'lowell': Lowell Observatory
;  'vbo': Vainu Bappu Observatory
;  'flwo': Whipple Observatory
;  'oro': Oak Ridge Observatory
;  'lna': Laboratorio Nacional de Astrofisica - Brazil
;  'saao': South African Astronomical Observatory
;  'casleo': Complejo Astronomico El Leoncito, San Juan
;  'bosque': Estacion Astrofisica Bosque Alegre, Cordoba
;  'rozhen': National Astronomical Observatory Rozhen - Bulgaria
;  'irtf': NASA Infrared Telescope Facility
;  'bgsuo': Bowling Green State Univ Observatory
;  'ca': Calar Alto Observatory
;  'holi': Observatorium Hoher List (Universitaet Bonn) - Germany
;  'lmo': Leander McCormick Observatory
;  'fmo': Fan Mountain Observatory
;  'whitin': Whitin Observatory, Wellesley College
;  'mgio': Mount Graham International Observatory
;
; PROCEDURE CALLS:
;    TEN()             
; REVISION HISTORY:
;    Written   W. Landsman                 July 2000
;    Corrected sign error for 'holi'   W.L/ Holger Israel    Mar 2008
;    Correctly terminate when observatory name not recognized 
;                                              S. Koposov, July 2008
;-

 On_error,2                                  ;Return to caller
 compile_opt idl2

 if N_params() LT 1 then begin
    print,'Observatory, obsname, obs_struct, [/print]'
    return
 endif
 
obs=[ 'kpno','ctio','eso','lick','mmto','cfht','lapalma','mso','sso','aao', $
  'mcdonald','lco','mtbigelow','dao','spm','tona','Palomar','mdm','NOV','bmo',$
  'BAO','keck','ekar','apo','lowell','vbo','flwo','oro','lna','saao','casleo', $
  'bosque','rozhen','irtf','bgsuo','ca','holi','lmo','fmo','whitin','mgio']

 if N_elements(obsname) EQ 1 then if obsname eq '' then obsname = obs
 nobs = N_elements(obsname)
 obs_struct = {observatory:'',name:'', longitude:0.0, latitude:0.0, $
   altitude:0.0, tz:0.0}
 if Nobs GT 1 then obs_struct = replicate(obs_struct,Nobs)
 obs_struct.observatory = obsname


for i=0,Nobs-1 do begin
case strlowcase(obsname[i]) of 
"kpno": begin
	name = "Kitt Peak National Observatory"
	longitude = [111,36.0]
	latitude = [31,57.8]
	altitude = 2120.
	tz = 7
        end
"ctio": begin
	name = "Cerro Tololo Interamerican Observatory"
	longitude = 70.815
	latitude = -30.16527778
	altitude = 2215.
	tz = 4
        end
"eso":  begin
	name = "European Southern Observatory"
	longitude = [70,43.8]
	latitude =  [-29,15.4]
	altitude = 2347.
	tz = 4
        end
"lick": begin
	name = "Lick Observatory"
	longitude = [121,38.2]
	latitude = [37,20.6]
	altitude = 1290.
	tz = 8
        end
"mmto": begin
	name = "MMT Observatory"
	longitude = [110,53.1]
	latitude = [31,41.3]
	altitude = 2600.
	tz = 7
        end
"cfht": begin
	name = "Canada-France-Hawaii Telescope"
	longitude = [155,28.3]
	latitude = [19,49.6]
	altitude = 4215.
	tz = 10
        end        
"lapalma": begin
	name = "Roque de los Muchachos, La Palma"
	longitude = [17,52.8]
	latitude = [28,45.5]
	altitude = 2327
	tz = 0
        end
"mso":  begin
	name = "Mt. Stromlo Observatory"
	longitude = [210,58,32.4]
	latitude = [-35,19,14.34]
	altitude = 767
	tz = -10
        end
"sso":  begin
	name = "Siding Spring Observatory"
	longitude = [210,56,19.70]
	latitude = [-31,16,24.10]
	altitude = 1149
	tz = -10
        end
"aao":  begin
	name = "Anglo-Australian Observatory"
	longitude = [210,56,2.09]
	latitude = [-31,16,37.34]
	altitude = 1164
	tz = -10
        end
"mcdonald": begin
	name = "McDonald Observatory"
	longitude = 104.0216667
	latitude = 30.6716667
	altitude = 2075
	tz = 6
        end
"lco":  begin
	name = "Las Campanas Observatory"
	longitude = [70,42.1]
	latitude = [-29,0.2]
	altitude = 2282
	tz = 4
        end
"mtbigelow": begin
	name = "Catalina Observatory: 61 inch telescope"
	longitude = [110,43.9]
	latitude = [32,25.0]
	altitude = 2510.
	tz = 7
        end
"dao":  begin
	name = "Dominion Astrophysical Observatory"
	longitude = [123,25.0]
	latitude = [48,31.3]
	altitude = 229.
	tz = 8
        end
 "spm":  begin
	name = "Observatorio Astronomico Nacional, San Pedro Martir"
	longitude = [115,29,13]
	latitude = [31,01,45]
	altitude = 2830.
	tz = 7
        end
 "tona": begin
	name = "Observatorio Astronomico Nacional, Tonantzintla"
	longitude = [98,18,50]
	latitude = [19,01,58]
	tz = 8
        altitude = -999999    ; Altitude not supplied
        end
 "palomar": begin
	name = "The Hale Telescope"
	longitude = [116,51,46.80]
	latitude = [33,21,21.6]
	altitude = 1706.
	tz = 8
        end
 "mdm": begin
	name = "Michigan-Dartmouth-MIT Observatory"
	longitude = [111,37.0]
	latitude = [31,57.0]
	altitude = 1938.5
	tz = 7
        end
 "nov": begin
	name = "National Observatory of Venezuela"
	longitude = [70,52.0]
	latitude = [8,47.4]
	altitude = 3610
	tz = 4
        end
 "bmo": begin
	name = "Black Moshannon Observatory"
	longitude = [78,00.3]
	latitude = [40,55.3]
	altitude = 738.
	tz = 5
         end
 "bao": begin
	name = "Beijing XingLong Observatory"
	longitude = [242,25.5]
	latitude = [40,23.6]
	altitude = 950.
	tz = -8
        end
 "keck": begin
	name = "W. M. Keck Observatory"
	longitude = [155,28.7]
	latitude = [19,49.7]
	altitude = 4160.
	tz = 10
        end
 "ekar": begin
	name = "Mt. Ekar 182 cm. Telescope"
	longitude = [348,25,07.92]
	latitude = [45,50,54.92]
	altitude = 1413.69
	tz = -1
        end
 "apo":  begin
	name = "Apache Point Observatory"
	longitude = [105,49.2]
	latitude = [32,46.8]
	altitude = 2798.
	tz = 7 
        end
 "lowell": begin
	name = "Lowell Observatory"
	longitude = [111,32.1]
	latitude = [35,05.8]
	altitude = 2198. 
	tz = 7 
        end
 "vbo": begin
	name = "Vainu Bappu Observatory"
	longitude = 281.1734
	latitude = 12.57666
	altitude = 725. 
	tz = -5.5
         end
 "flwo": begin
        name = "Whipple Observatory"
        longitude = [110,52,39]
        latitude = [31,40,51.4]
        altitude = 2320.
        tz = 7
        end
 "oro": begin
	name = "Oak Ridge Observatory"
        longitude = [71,33,29.32]
        latitude =  [42,30,18.94]
        altitude =  184.
        tz = 5
        end

 "lna":  begin
        name = "Laboratorio Nacional de Astrofisica - Brazil"
        longitude = 45.5825
        latitude = [-22,32,04]
        altitude = 1864.
        tz = 3
        end

 "saao": begin
	name = "South African Astronomical Observatory"
	longitude = [339,11,21.5]
	latitude =  [-32,22,46]
	altitude =  1798.
	tz = -2
         end
 "casleo": begin
        name = "Complejo Astronomico El Leoncito, San Juan"
        longitude = [69,18,00] 
        latitude = [-31,47,57]
        altitude = 2552
        tz = 3
        end
 "bosque": begin
        name = "Estacion Astrofisica Bosque Alegre, Cordoba"
        longitude = [64,32,45]
        latitude = [-31,35,54]
        altitude = 1250
        tz = 3
        end
 "rozhen": begin
        name = "National Astronomical Observatory Rozhen - Bulgaria"
	longitude = [335,15,22]
	latitude = [41,41,35]
	altitude = 1759
	tz = -2
        end
 "irtf": begin
	name        = "NASA Infrared Telescope Facility"
	longitude   = 155.471999
	latitude    = 19.826218
	altitude    = 4168
	tz    = 10
        end
 "bgsuo": begin
        name = "Bowling Green State Univ Observatory"
        longitude = [83,39,33]
        latitude = [41,22,42]
        altitude = 225.
        tz = 5
        end
 "ca":   begin
	name = "Calar Alto Observatory"
	longitude = [2,32,46.5]
	latitude = [37,13,25]
	altitude = 2168
	tz = -1
        end
 "holi": begin
        name = "Observatorium Hoher List (Universitaet Bonn) - Germany"
        longitude = 353.15     ;Corrected sign error March 2008
        latitude = 50.16276
        altitude = 541
        tz = -1
       end
 "lmo":  begin
        name = "Leander McCormick Observatory"
        longitude = [78,31,24]
        latitude =  [38,02,00]
        altitude = 264
        tz = 5
        end
 "fmo": begin
        name = "Fan Mountain Observatory"
        longitude = [78,41,34]
        latitude =  [37,52,41]
        altitude = 556 
        tz = 5
       end
 "whitin": begin
	name = "Whitin Observatory, Wellesley College"
	longitude = 71.305833
	latitude = 42.295
	altitude = 32
	tz = 5
        end
 "mgio": begin
	name = "Mount Graham International Observatory"
	longitude = [109,53,31.25]
	latitude = [32,42,04.69]
	altitude = 3191.0
	tz = 7
        end
 else: message,'Unable to find observatory ' + obsname + ' in database'
 endcase

 obs_struct[i].longitude = ten(longitude)
 obs_struct[i].latitude = ten(latitude)
 obs_struct[i].tz = tz
 obs_struct[i].name = name
 obs_struct[i].altitude = altitude

 if N_params() EQ 1 or keyword_set(print) then begin
     print,' '
     print,'Observatory: ',obsname[i]
     print,'Name: ',name
     print,'longitude:',obs_struct[i].longitude
     print,'latitude:',obs_struct[i].latitude
     print,'altitude:',altitude
     print,'time zone:',tz
  endif
 endfor

 return
 end
