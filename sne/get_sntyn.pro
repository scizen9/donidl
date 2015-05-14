function get_sntyn,tystr
;+
;	convert tystr into a numeric equivalent for the SN type
;-
;
; SN types
tylist=[ $
	'I', $
	'Ia', $
	'Ib', 'Ib/c', 'Ic', $
	'IIn', 'IIb', $
	'IIL', 'IIP', $
	'II' ]
nty = n_elements(tylist)
tyn = indgen(nty)
tmods = ['pec',':','?','*']	; type modifiers
;
; get numeric type
ty	= tystr
;
; strip off mods
for j=0,n_elements(tmods)-1 do $
	if strpos(ty,tmods[j]) ge 0 then $
		ty	= strmid(ty,0,strpos(ty,tmods[j]))
;
; compare with tylist
t=where(strcmp(tylist,ty) eq 1, n)
;
return,t[0]
end
