pro gx_pnreg,ifil
;
readcol,ifil,pn,rah,ram,ras,ddeg,dmin,dsec,ep,n,moiii,val, $
	form='a,i,i,f,i,i,f,f,i,f,f'
nrec=n_elements(pn)
;
tmp=ifil
rute=gettok(tmp,'.')
;
; open output file
openw,ol,rute+'.reg',/get_lun
;
; loop over records
for i=0,nrec-1 do begin
;
; PN
	sta=strsplit(pn(i),'_',/extract)
	lab = sta(n_elements(sta)-1)
	if strpos(rute,'bulge') ge 0 then $
		lab = 'B'+lab $
	else	lab = 'W'+lab
	printf,ol,'fk5;circle(',rah(i),':',ram(i),':',ras(i),',', $
			       ddeg(i),':',dmin(i),':',dsec(i), $
				',3.0") # text={'+lab+'}', $
		format='(a,i02,a1,i02,a1,f05.2,a1,i02,a1,i02,a1,f05.2,a)'
endfor
;
free_lun,ol
;
return
end
;
