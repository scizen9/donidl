function get_nga_name,inm,flist,silent=silent
;
; get NGA name
host=inm
onm=strtrim(inm,2)
p=stregex(host,'[0-9]')
cat=strmid(host,0,p)
if strcmp(cat,'E') or strcmp(cat,'ESO') then begin	; ESO galaxy catalog
	cat='ESO'
	b=stregex(host,'-')
	c=stregex(host,'G')
	if c lt 0 then c = b
	n1=fix(strmid(host,p,b-p))
	n2=fix(strmid(host,c+1))
	mdl=strmid(host,b,(c-b)+1)
	onm=cat+string(n1,form='(i03)')+mdl+string(n2,form='(i03)')
endif else if strcmp(cat,'IC') or $
	      strcmp(cat,'FGC') or $
	      strcmp(cat,'NGC') then begin
	p2=stregex(strmid(host,p),'[A-Za-z]')
	if p2 ge 0 then $
		tl = strupcase(strmid(host,p+p2,1)) $
	else	tl =''
	n1=fix(strmid(host,p))
	onm=cat+string(n1,form='(i04)')+tl
endif else if strcmp(cat,'PGC') or $
	      strcmp(cat,'UGC') or $
	      strcmp(cat,'LEDA') then begin
	n1=long(strmid(host,p))
	if n1 le 99999L then $
		fmt = '(i05)' $
	else if n1 le 999999L then $
		fmt = '(i06)' $
	else	fmt = '(i07)'
	onm=cat+string(n1,form=fmt)
endif
;
; check for NGA entry
if n_params(0) gt 1 then begin
	flist=file_search(!NGA_DATA+onm+'-prof_*',count=nf)
	if not keyword_set(silent) then $
		print,nf,' NGA files found.'
endif
;
return,onm
end
