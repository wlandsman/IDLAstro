                                                         December 2017
 
This directory contains procedures useful for querying the Web.      Earlier versions of
these routines used the IDL SOCKET command to access Web servers, but they now 
(except for querygsc.pro) use the more robust IDLnetURL object.  

The directory  includes IDL procedures from the Infrared Science Archive (IRSA)
( http://irsa.ipac.caltech.edu/tools/irsa_idl.html )

WEBGET() - Use the IDL SOCKET procedure to get data from http servers
QUERYGSC() - Query the Guide Star Catalog (GSC V2.3.2) at the Space
    Telescope Science Institute by position  
QUERYDSS - Query the digital sky survey (DSS) on-line at the European
     Space Observatory (ESO) or STSCI servers
QUERYSIMBAD - Query the SIMBAD or NED name resolvers to obtain J2000
     coordinates
QUERYVIZIER - Positional query of any catalog in the VIZIER database
QUERY_IRSA_CAT - queries IRSA catalogs, returning an IDL structure
READ_IPAC_TABLE - reads an IPAC Table file into an IDL structure
READ_IPAC_VAR -   converts an IPAC table internal variable to an IDL structure
WRITE_IPAC_TABLE - writes an IDL structure to an IPAC Table file

*************
IRSA Query Routines

IDL> .run read_ipac_var.pro,query_irsa_cat.pro
IDL> info = query_irsa_cat([150.11917,2.205833], catalog='irasfsc', radius=1, radunits='deg')

QUERY_IRSA_CAT.pro performs a query of a catalog in the IRSA holdings.
The default is to query the 2MASS Point Source Catalog ('fp_psc').  It
returns an IDL structure containing the contents of the downloaded
table. If "outfile" is present, it will write the incoming table to a
file.  This can be slow, so only do so if needed.

Queries can by made by RA,Dec or by object name (resolved by NED
or SIMBAD).  The radius of the search may be specified; the default
units are arcsec.  See the file header for further details.


In order to query other IRSA catalogs, the program needs to know
the identifier string for the IRSA Program Interface.  The complete
list of current catalogs (with the needed string) is available in 
XML format at

http://irsa.ipac.caltech.edu/cgi-bin/Gator/nph-scan?mode=xml

or as an IPAC Table (ascii) at

http://irsa.ipac.caltech.edu/cgi-bin/Gator/nph-scan?mode=ascii

The identifier string needed for the IDL program is the "catname"
column of the table.

Changes:

Dec 2017: Updated to V2.0 of IPAC query routines

Sep 2013:  Added IPAC query procedures

Dec 2007: QUERYUSNO (to query USNO-A2 catalog) has been removed since the newer
USNO-B1 catalog can be queried with QUERYIVIZIER, e.g.  
IDL> info = queryvizier('usno-b1','m13',5)
