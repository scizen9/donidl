pro correct_astier_sne,mno,write_params=write_params
;
; get sne data
openr,1,'../no_scale/Astier_sne.dat'
hdr=''
readf,1,hdr
names=strarr(115)
nam=''
data=dblarr(12,115)
dat=dblarr(12)
for i=0,114 do begin
    readf,1,nam,dat,format='(a10,12f12.6)'
    names(i)=strtrim(nam,2)
    data(*,i)=dat(*)
endfor
close,1
;
; speed of light
c=299792.458d0
;
rec=''
;
; set up outputs
    odata=data
    dfile='Astier_sne.dat'
;
; get new velocities
    flist=file_search('../lowz_bothzs.txt',count=nf)
    if nf eq 1 then begin
        readcol,flist(0),inam,zhel,zcmb,form='a,f,f'
	nin=n_elements(zcmb)
	for j=0,nin-1 do begin
	    w=where(strpos(names,strmid(inam(j),2,6)) ge 0, n)
	    if n eq 1 then begin
		odata(0,w(0)) = zcmb(j)
		odata(1,w(0)) = zhel(j)
	    endif else print,'Not found: ',iname(j)
	endfor
;
; write out corrected sne data
	openw,2,dfile
	printf,2,hdr
	for k=0,114 do printf,2,names(k),odata(*,k),form='(a10,12f12.6)'
	close,2
	print,'Wrote SN data file.'
    endif else print,' lowz_bothzs.txt not found.'
;
return
end
