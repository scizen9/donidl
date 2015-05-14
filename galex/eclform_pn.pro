pro eclform_pn,ifil
;
readcol,ifil,pn,rah,ram,ras,ddeg,dmin,dsec,ep,n,moiii,val, $
	form='a,i,i,f,i,i,f,f,i,f,f'
nrec=n_elements(pn)
;
tmp=ifil
rute=gettok(tmp,'.')
;
; open output file
openw,ol,rute+'_ecl.txt',/get_lun
;
; loop over records
for i=0,nrec-1 do begin
;
; get coords
	dra = ten(rah(i),ram(i),ras(i))*15.d0
	ddec = ten(ddeg(i),dmin(i),dsec(i))
;
; write out eclstat script
       if dra ge 0. then begin
          sra=strtrim(string(dra,form='(f9.5)'),2)
          sdec=strtrim(string(ddec,form='(f9.5)'),2)
          printf,ol,'echo '+pn(i)+'  >> '+rute+'_ecl.out'
          printf,ol,'eclstat -csv -hdr -pos '+sra+','+sdec+' >> '+rute+'_ecl.out'
       endif
endfor
free_lun,ol
;
return
end
