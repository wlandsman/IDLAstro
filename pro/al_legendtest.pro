
;+
; NAME:
;	AL_LEGENDTEST
; PURPOSE:
;	Demo program to show capabilities of  the al_legend procedure.
; CALLING SEQUENCE:
;	al_legendtest
; INPUTS:
;	none
; OPTIONAL INPUTS:
;	none
; KEYWORDS:
;	none
; OUTPUTS:
;	legends of note
; COMMON BLOCKS:
;	none
; SIDE EFFECTS:
;	Sets !20 font to symbol if PostScript and !p.font=0.
; RESTRICTIONS:
;	With the vectorfont test, you'll get different results for PostScript
;	depending on the value of !p.font.
; MODIFICATION HISTORY:
;	write, 27 Aug 92, F.K.Knight (knight@ll.mit.edu)
;	add test of /left,/right,/top,/bottom keywords, 21 June 93, FKK
;	update based on recent changes to legend, 7 Feb 94, FKK
;       Fix ambiguous CHAR keyword  W. Landsman Sep 2007
;       Use Coyote graphics routines  W. Landsman Jan 2011
;-
pro al_legendtest
if (!d.name eq 'PS') && (!p.font eq 0) then device,/Symbol,font_index=20
items = ['diamond','asterisk','square']
explanation = ['The al_legend procedure annotates plots---' $
  ,'  either using text alone,' $
  ,'  or text with plot symbols, lines, and special characters.' $
  ,'The following are some examples.' $
  ,'Hit return to continue.']
psym = [4,2,6]
lineitems = ['solid','dotted','DASHED']
linestyle = [0,1,2]
citems = 'color '+strtrim(string(indgen(8)),2)
colors = ['red','blue','violet','green','yellow','brown','black','cyan']
fillcolors = ['pink', 'purple', '', '', 'black', '', '', 'gray']
orientation = [-45, 45, 0, 0, 0, 0, 0, -999]
usersym,[-1,1,1,-1,-1],[-1,-1,1,1,-1],/fill
z = ['al_legend,explanation,charsize=1.5' $
	,'al_legend,items,psym=[4,2,6]' $
	,'cgplot,findgen(10) & al_legend,items,psym=[4,2,6] & al_legend,items,psym=[4,2,6],/bottom,/right' $
	,'al_legend,lineitems,linestyle=linestyle,/right,/bottom' $
	,'al_legend,items,psym=psym,/horizontal,chars=1.5	; horizontal format' $
	,'al_legend,[items,lineitems],psym=[psym,0,0,0],linestyle=[0,0,0,linestyle],/center,box=0		; sans border' $
	,'al_legend,items,psym=psym,margin=1,spacing=2,chars=2,delimiter="=",/top,/center; delimiter & larger margin' $
	,'al_legend,lineitems,linestyle=linestyle,pos=[.3,.5],/norm,chars=2,number=4	; position of legend' $
	,'al_legend,items,psym=-psym,number=2,linestyle=linestyle,/right; plot two symbols, not one' $
	,'al_legend,citems,/fill,psym=15+intarr(8),colors=colors,chars=2; 8 filled squares' $
	,'al_legend,citems,/poly_fill,colors=colors,polycolor=fillcolors,line_orientation=orientation' $
 ,'al_legend,citems,colors=colors,polycolor=fillcolors' $
 ,'al_legend,[citems[0:4],lineitems],/fill,psym=[15+intarr(5),0*psym],linestyle=[intarr(5),linestyle],colors=colors,chars=2,text=colors' $
	,"al_legend,['Absurd','Sun Lover','Lucky Lady','Fishtail Palm'],vector=['ab!9r!3','!9nu!3','!9Wf!3','!9cN!20K!3'],charsize=2,/pos,psp=3"$
	]
prompt = 'Hit return to continue:'
for i = 0,n_elements(z) - 1 do begin
  cgerase
  stat = execute(z[i])
  cgtext,.01,.15,'COMMAND TO MAKE LEGEND:',charsize=1.7,/norm
  cgtext,.01,.05,z[i],/norm,charsize=1.2
  print,'Command: ',z[i]
  print,prompt,format='($,a)'
  a = get_kbrd(1)
  print
  endfor
;stop
cgerase
!p.charsize=2
c1_items = ['Plus','Asterisk','Period','Diamond','Triangle','Square','X']
c1_psym = indgen(7)+1
c2_items = ['Solid','Dotted','Dashed','Dash Dot','Dash Dot Dot Dot','Long Dashes']
c2_line = indgen(6)
al_legend,c1_items,psym=c1_psym,corners=c1,box=0
al_legend,c2_items,linestyle=c2_line,corners=c2,box=0,pos=[c1[2],c1[3]],/norm
c = [c1[0]<c2[0],c1[1]<c2[1],c1[2]>c2[2],c1[3]>c2[3]]
cgplots,[c[0],c[0],c[2],c[2],c[0]],[c[1],c[3],c[3],c[1],c[1]],/norm
!p.charsize=0
cgtext,.01,.05,$
  'Multiple columns---type "al_legend,/help" for details.',/norm,charsize=1.2
return
end

