pro gx_flag2reg,ifil,xno,ofile
;
; reads a GALEX flag file and writes out a ds9 reg file
;
flags=mrdfits(ifil,xno)
nf=n_elements(flags)
coos=adstring(flags.ra,flags.dec)
;
openw,ol,ofile,/get_lun
;
; loop over flags
for i=0,nf-1 do begin
	d=coos(i)
	a=strtrim(gettok(d,'  '),2)
	d=strtrim(d,2)
	while strpos(a,' ') ge 0 do strput,a,':',strpos(a,' ')
	while strpos(d,' ') ge 0 do strput,d,':',strpos(d,' ')
	printf,ol,'fk5;circle('+a+','+d+',10.0") # text={'+$
		string(flags[i].id,form='(f5.2)')+ $
		'} color=red width=2 font="helvetica 18 bold"'
endfor
;
free_lun,ol
;
return
end
