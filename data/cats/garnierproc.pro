pro garnierproc
;
openr,il,'garnier96.txt',/get_lun
openw,ol,'garnier96.dat',/get_lun
;
pgc=0L
anm=''
ra=''
dec=''
acc=''
pa=0.
epa=0.
bt=0.
ebt=0.
ld25=0.
eld25=0.
lr25=0.
elr25=0.
while not eof(il) do begin
	readf,il,pgc,anm,ra,dec,acc,pa,epa,bt,ebt,ld25,eld25,lr25,elr25, $
    format='(i6,2x,a14,1x,a8,a7,a1,f6.0,f4.0,f7.2,f4.2,f6.2,f4.2,f6.2,f4.2)'
    name = 'PGC'+string(pgc,form='(i07)')
    cat = gettok(anm,' ')
    if strcmp(cat,'NGC') or strcmp(cat,'UGC') or strcmp(cat,'IC') then begin
	    name = cat+gettok(anm,' ')
    endif else if strcmp(cat,'ESO') then begin
	    b = gettok(anm,' ')
	    c = fix(strmid(anm,0,2))
	    e = strmid(anm,2,1)
	    name = 'E'+b+string(c,format='(i02)')+e
    endif else if strcmp(cat,'MCG') then begin
	    bs = gettok(anm,' ')
	    b = fix(strmid(bs,0,2))
	    if b ge 0 then sg='+' else sg='-'
	    c = fix(gettok(anm,'-'))
	    d = fix(strmid(anm,0,3))
	    if d gt 99 then $
		    ds = string(d,form='(i3)') $
	    else    ds = string(d,form='(i02)')
	    e = strmid(anm,3,1)
	    name = 'M'+sg+string(abs(b),form='(i02)')+'-'+$
		    string(c,form='(i02)')+'-'+ds+e
    endif else if strcmp(cat,'CGCG') then begin
	    b = gettok(anm,' ')
	    c = fix(gettok(anm,' '))
	    name = cat+b+string(c,form='(i03)')
    endif
    printf,ol,name,pa,epa,bt,ebt,ld25,eld25,lr25,elr25, $
	    format='(a-14,2x,f6.0,f4.0,f7.2,f6.2,f7.2,f6.2,f7.2,f6.2)'
endwhile
;
free_lun,il,ol
;
return
end
