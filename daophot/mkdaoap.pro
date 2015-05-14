pro mkdaoap,pfile,nx,ny,ap1,lbad,hbad,th,phadu,rn,frad,magbest=magbest
;
;
np=n_params(0)
switch np of
	0:
	1:
	2: begin
		print,'MKDAOAP: Usage - mkdaoap, photfile, nx, ny, ap1, lowbad, highbad, thresh, ph_adu, readnoise, fitrad, /magbest'
	   return
	   break
	end
	3: ap1=25.0
	4: lbad=10.0
	5: hbad=55000.0
	6: th=10.0
	7: phadu=2.0
	8: rn=5.0
	9: frad=5.0
endswitch
;
temp=pfile
rute=gettok(temp,'.')
if keyword_set(magbest) then $
	mbest=(1 eq 1) $
else	mbest=(1 eq 0)
if temp eq 'iap' then begin
	ext='.diap'
	ptype=1
endif else if temp eq 'sex' then begin
	if mbest then $
		ext='.sbap' $
	else	ext='.saap'
	ptype=2
endif else begin
	print,'MKDAOAP: Error - unknown phot file extension: ',temp
	return
endelse
;
ofile=rute+ext
openw,ol,ofile,/get_lun
;
printf,ol, $
	' NL   NX   NY  LOWBAD HIGHBAD  THRESH     AP1  PH/ADU  RNOISE    FRAD'
printf,ol,2,nx,ny,lbad,hbad,th,ap1,phadu,rn,frad,format='(i3,2i5,2f8.1,5f8.2)'
printf,ol,''
;
id=0L
x=0.
y=0.
m1=0.
m1e=0.
m2=0.
m2e=0.
sky=0.
skye=0.
skys=0.
rec=' '
openr,il,pfile,/get_lun
;
case ptype of
;	IRAF *.iap files
	1: begin
	    while not eof(il) do begin
		readf,il,rec
		if strpos(rec,'INDEF') lt 0 then begin
		    reads,rec,id,x,y,m1,m1e,m2,m2e,sky,skye
		    if m2 lt 0. then m2 = 25.0 + m2
		    printf,ol,''
		    printf,ol,id,x,y,m2,form='(i6,3f9.3)'
		    printf,ol,sky,skye,skys,m2e,form='(f13.3,2f6.2,f8.4)'
		endif
	    endwhile
	    free_lun,il,ol
	end
;	SEXtractor *.sex files
	2: begin
	    id=1L
	    while not eof(il) do begin
	    	readf,il,rec
		if strmid(rec,0,1) ne '#' then begin
		    reads,rec,x,y,m1,m1e,m2,m2e,sky
		    if mbest then begin
		    	mo = m1
			moe= m1e
		    endif else begin
		    	mo = m2
			moe= m2e
		    endelse
		    if mo lt 0. then mo = 25.0 + mo
		    printf,ol,''
		    printf,ol,id,x,y,mo,form='(i6,3f9.3)'
		    printf,ol,sky,skye,skys,moe,form='(f13.3,2f6.2,f8.4)'
		    id=id+1L
		endif; else print,rec
	    endwhile
	    free_lun,il,ol
	end
endcase
;
return
end
