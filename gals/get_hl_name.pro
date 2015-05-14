function get_hl_name,inm,silent=silent
;
; get Hyper LEDA name
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
	mdl='-'
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
endif else if strcmp(cat,'UGC') then begin
	p2=stregex(strmid(host,p),'[A-Za-z]')
	if p2 ge 0 then $
		tl = strupcase(strmid(host,p+p2,1)) $
	else	tl =''
	n1=long(strmid(host,p))
		if n1 le 99999L then $
		fmt = '(i05)' $
	else if n1 le 999999L then $
		fmt = '(i06)' $
	else	fmt = '(i07)'
	onm=cat+string(n1,form=fmt)+tl
endif else if strcmp(cat,'AGC') then begin
	p2=stregex(strmid(host,p),'[A-Za-z]')
	if p2 ge 0 then $
		tl = strupcase(strmid(host,p+p2,1)) $
	else	tl =''
	n1=long(strmid(host,p))
	onm=cat+string(n1,form='(i06)')+tl
endif else if strcmp(cat,'ARP') then begin
	p2=stregex(strmid(host,p),'[A-Za-z]')
	if p2 ge 0 then $
		tl = strupcase(strmid(host,p+p2,1)) $
	else	tl =''
	n1=long(strmid(host,p))
	onm=cat+string(n1,form='(i03)')+tl
endif else if strcmp(cat,'PGC') then begin
	n1=long(strmid(host,p))
	if n1 le 999999L then $
		fmt = '(i06)' $
	else	fmt = '(i07)'
	onm=cat+string(n1,form=fmt)
endif else if strcmp(cat,'M+') or strcmp(cat,'M-') then begin
	cat='MCG'+strmid(cat,1)
	sta=strsplit(strmid(host,p),'-',/extract,count=nn)
	if nn gt 0 then $
		n1=fix(sta[0]) $
	else	n1=0
	if nn gt 1 then $
		n2=fix(sta[1]) $
	else	n2=0
	if nn gt 2 then begin
		p2=stregex(sta[2],'[A-Za-z]')
		if p2 gt 0 then $
			tl = strupcase(strmid(sta[2],p2,1)) $
		else	tl = ''
		n3=fix(sta[2])
	endif else begin
		tl = ''
		n3 = 0
	endelse
	onm=cat+string(n1,form='(i02)')+'-'+string(n2,form='(i02)')+'-'+ $
		string(n3,form='(i03)')+tl
endif else if strcmp(cat,'CGCG') then begin
	sta=strsplit(strmid(host,p),'-',/extract,count=nn)
	if nn gt 0 then $
		n1=fix(sta[0]) $
	else	n1=0
	if nn gt 1 then begin
		p2=stregex(sta[1],'[A-Za-z]')
		if p2 gt 0 then $
			tl = strupcase(strmid(sta[1],p2,1)) $
		else	tl = ''
		n2=fix(sta[1])
	endif else begin
		tl = ''
		n2 = 0
	endelse
	onm=cat+string(n1,form='(i03)')+'-'+string(n2,form='(i03)')+tl
endif
;
return,onm
end
