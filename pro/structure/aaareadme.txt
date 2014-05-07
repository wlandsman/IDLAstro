pro/structure                                                 July 2009

  This directory contains IDL procedures for working with IDL structure 
variables.    The *procedure* CREATE_STRUCT will dynamically create an IDL 
structure and is useful when the structure properties are not known before run
time.   The intrinsic *function* CREATE_STRUCT() in IDL  has a similar purpose,
but requires that each tag value be input as a separate parameter.    This makes
the intrinisc function awkward to use when the *number* of tags is not known
prior to run time.   The procedure  MRD_STRUCT is similar to CREATE_STRUCT and
was developed for the MRDFITS  procedure.

This directory also contains following IDL routines for working with 
structures, written by Frank Varosi (Hughes STX):   


  COPY_STRUCT,  struct_FROM,  struct_TO,  NF_copied,  EXCEPT=["except_Tags"]  ,$
						 /RECUR_FROM	 ,$
						 /RECUR_TO	 ,$
						 /RECUR_TANDEM

 	Copies all Fields with matching Tag names (except for "except_Tags")
		from one structure array to a different structure array.
	Keyword options /RECUR_xxxx will cause recursive calls,
		in order to copy from/to nested sub-structures.
	NF_copied is incremented by # fields actually copied.



  diff_List = COMPARE_STRUCT( struct_A,  struct_B,   EXCEPT=["except_Tags"]   ,$
					             /RECUR_A, /RECUR_B   )

 	Compares all matching Tag names (except for "except_Tags")
		between two structure arrays (may be different struct.defs.).
	Returned Diff_List is a structure containing field names and # diffs.



  nbytes = SIZE_STRUCT( structure,  /PRINT )

	Obtain the size in bytes of an IDL structure definition.
	/PRINT = to print all sub-structure sizes.


  ns = N_STRUCT( structure,  Ntags )

	Return number of elements in array, if structured, and number of tags.
	Returns zero if not structured, also works on file assoc. structs.


  PRINT_STRUCT,  structure,  ["tags_print"],  LUN_OUT=Lun
		Print specified tags from structure (to LUN if given).


  wsubs = WHERE_TAG( Struct,  Nfound,  TAG_NAME="Tag_Name",  $
					ISELECT=ipart,  /NOPRINT,  $
					RANGE=[min,max],  VALUES=[values] )

	Obtain subscripts of elements in structure array for which
	specified Tag has values in a RANGE or matching specified VALUES.
	Useful in programming when Tag_Name is a variable.
