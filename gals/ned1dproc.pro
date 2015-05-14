pro ned1dproc
;
ndir=!NED_DATA
openr,il,ndir+'/ned-1d.txt',/get_lun
openw,ol,ndir+'/ned-1d.dat',/get_lun
;
rec=''
for i=0,6 do begin
	readf,il,rec
	printf,ol,rec
endfor

nrec=0L
while not eof(il) do begin
    crec = ''
    for i=0,12 do begin
	readf,il,rec
	rec=strtrim(rec,2)
	if strtrim(rec,2) eq '' then rec='-'
	while strpos(rec,' ') ge 0 do strput,rec,'_',strpos(rec,' ')
	while strpos(rec,',') ge 0 do strput,rec,':',strpos(rec,',')
	crec=crec+rec+' '
    endfor
    nrec = nrec + 1L
    sta=strsplit(crec,/extract,count=ni)
;
; first record?
    if nrec le 3 then begin
	printf,ol,'#',sta(0),sta(1),sta(2),sta(3),sta(4),sta(5),sta(6),sta(7),$
		sta(8),sta(9),sta(10),sta(11),sta(12),$
		format='(1a,1x,a-28,1x,a-7,1x,a-7,1x,a-9,1x,a-10,1x,a-20,1x,a-30,1x,a-11,1x,a-12,1x,a-7,1x,a-7,1x,a-12,1x,a-16)'
    endif else if strpos(crec,'Galaxy') lt 0 and $
	          strpos(crec,'(J2000)') lt 0 and $
		  strpos(crec,'(13)') lt 0 then begin
;
; galaxy name
	name = sta(0)
	while strpos(name,'_') ge 0 do begin
		p=strpos(name,'_')
		name=strmid(name,0,p)+strmid(name,p+1)
	endwhile
;
; m-M
	mu = float(sta(1))
;
; m-M err
	if strpos(sta(2),'...') ge 0 or strpos(sta(2),'-') ge 0 then $
		muerr=-9. $
	else    muerr=float(sta(2))
;
; D
	dmpc=float(sta(3))
;
; glon, glat
	glon=float(sta(9))
	glat=float(sta(10))
;
; Vgsr
	if strpos(sta(12),'-') lt 0 then $
		vgsr=float(sta(12)) $
	else	vgsr=0.
;
; print results
	printf,ol,name,mu,muerr,dmpc,sta(4),sta(5),sta(6),sta(7),sta(8),$
		glon,glat,sta(11),vgsr,$
		format='(a-28,2x,2f8.3,1x,f9.3,1x,a-10,1x,a-20,1x,a-30,1x,a-11,1x,a-12,1x,2f7.1,1x,a-12,1x,f13.1)'
    endif

endwhile
;
free_lun,il,ol
;
return 
end
