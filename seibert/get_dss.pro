pro get_dss, id,  ra, dec, diam, survey, outdir=outdir, status=status

; id = output prefix
; ra,dec = decimal degrees (scalar)
; diam = arcmin (scalar)
; survey = 'red','blue','ir'
; outdir = directory to place data
; status = 1 (success) or 0 (fail)


w=ceil(diam)<60
h=w

cmda="wget 'http://archive.stsci.edu/cgi-bin/dss_search?v="

if survey eq 'red' then begin
 cmda=cmda+"poss2ukstu_red&r="
 filename=id+'_dss2_red.fits'
endif

if survey eq 'blue' then begin
 cmda=cmda+"poss2ukstu_blue&r="
 filename=id+'_dss2_blue.fits'
endif

if survey eq 'ir' then begin
 cmda=cmda+"poss2ukstu_ir&r="
 filename=id+'_dss2_ir.fits'
endif

cmdb="&f=fits&c=none&fov=NONE&v3='"

r=sixty(ra/15)
d=sixty(dec)
sign='+'
if d[0] lt 0 or d[1] lt 0 or d[2] lt 0 then sign='-'
d=abs(d)
 
p=strn(fix(r[0]))+'+'+strn(fix(r[1]))+'+'+strn(r[2],format='(F5.2)')
p=p+'&d='+sign+strn(fix(d[0]))+'+'+strn(fix(d[1]))+'+'+strn(d[2],$
    format='(F4.1)')
p=p+'&e=J2000&h='+strn(h)+'&w='+strn(w)


cmd = cmda+p+cmdb

;clean out old stuff
spawn,'ls dss*',result,err
if result ne '' then spawn,"rm '"+result+"' " 

spawn,cmd,result,err
spawn,'ls dss*',result,err

if result eq '' then begin
 status=0
 return
endif

spawn,"mv '"+result+"' "+filename

dss=mrdfits(filename,0,status=status,/silent)

if status lt 0 then begin
 spawn,'rm '+filename 
 status=0
 return
endif


spawn,'mv '+filename+' '+outdir+'/'+filename
spawn,'gzip -f '+outdir+'/'+filename
status=1

end
