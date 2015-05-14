pro mkdaocoo,pfile,nx,ny,ap1,lbad,hbad,th,phadu,rn,frad
;
;
np=n_params(0)
switch np of
	0:
	1:
	2: begin
		print,'MKDAOCOO: Usage - mkdaocoo, coofile, nx, ny, ap1, lowbad, highbad, thresh, ph/adu, readnoise, fitrad'
	   return
	   break
	end
	3: ap1=0.
	4: lbad=10.0
	5: hbad=55000.0
	6: th=10.0
	7: phadu=1.585
	8: rn=3.2
	9: frad=5.0
endswitch
;
temp=pfile
rute=gettok(temp,'.')
if temp eq 'iso' then begin
	ext='.icoo'
	ptype=1
endif else if temp eq 'sex' then begin
	ext='.scoo'
	ptype=2
endif else if temp eq 'reg' then begin
	ext='.rcoo'
	ptype=3
endif else begin
	print,'MKDAOCOO: Error - unknown coo file extension: ',temp
	return
endelse
;
ofile=rute+ext
openw,ol,ofile,/get_lun
;
printf,ol, $
	' NL   NX   NY  LOWBAD HIGHBAD  THRESH     AP1  PH/ADU  RNOISE    FRAD'
printf,ol,1,nx,ny,lbad,hbad,th,ap1,phadu,rn,frad,format='(i3,2i5,2f8.1,5f8.2)'
printf,ol,''
;
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
;	findiso *.iso files
	1: begin
	    id=1L
	    while not eof(il) do begin
		readf,il,x,y,m1,sky,skye
		printf,ol,id,x,y,m1,0.5,0.,0.,form='(i6,6f9.3)'
		id=id+1L
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
		    printf,ol,id,x,y,m2,0.5,0.,0.,form='(i6,6f9.3)'
		    id=id+1L
		endif; else print,rec
	    endwhile
	    free_lun,il,ol
	end
;	ds9 *.reg files
	3: begin
	    id=1L
	    while not eof(il) do begin
	    	readf,il,rec
		if strmid(rec,0,1) ne '#' and strmid(rec,0,1) ne 'g' then begin
		    jnk=gettok(rec,'(')
		    x=float(gettok(rec,','))
		    y=float(gettok(rec,','))
		    m1=-1.0
		    printf,ol,id,x,y,m1,0.5,0.,0.,form='(i6,6f9.3)'
		    id=id+1L
		endif; else print,rec
	    endwhile
	    free_lun,il,ol
	end
endcase
;
return
end
