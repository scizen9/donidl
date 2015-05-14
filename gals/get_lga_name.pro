function get_lga_name,inm,flist,silent=silent
;
; get LGA name
host=strlowcase(inm)
gal=host
p=stregex(host,'[0-9]')
if p ge 0 then begin
    cat=strmid(host,0,p)
    if strcmp(cat,'e') then begin	; ESO galaxy catalog
	cat='eso'
	b=stregex(host,'-')
	c=stregex(host,'g')
	n1=fix(strmid(host,p,b-p))
	n2=fix(strmid(host,c+1))
	mdl=strmid(host,b,(c-b)+1)
	gal=cat+string(n1,form='(i03)')+mdl+string(n2,form='(i03)')
    endif else if strcmp(cat,'m+') or strcmp(cat,'m-') then begin
	cat='mcg'
	sta=strsplit(strmid(host,p),'-',/extract)
	gal=cat+'-'+string(fix(sta(0)),form='(i02)') + $
		'-'+string(fix(sta(1)),form='(i02)') + $
		'-'+string(fix(sta(2)),form='(i03)')
    endif else begin
	n1=long(strmid(host,p))
	gal=cat+strn(n1)
    endelse
endif
;
; check for messier objects
	mess=mcross(strupcase(gal),/silent)
	if strlen(mess) gt 1 then gal=strlowcase(mess)
;
; check for LGA entry
if n_params(0) gt 1 then begin
	flist=file_search(!SNE_HOSTS+'2MASS/LGA/'+gal+'_*',count=nf)
	if not keyword_set(silent) then $
		print,nf,' LGA files found.'
endif
;
return,gal
end
