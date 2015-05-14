pro get_2mass,indx,samname
;
;	make a position list for batch image retrieval from 2mass archive
;
; galaxy data
COMMON galdb_info
;
; get data
host	= galdat[indx].id
ra	= galdat[indx].ra
dec	= galdat[indx].dec
siz	= ( galdat[indx].majax * 4.0 ) > 180.
;
; sort
s = sort(ra)
host	= host[s]
ra	= ra[s]
dec	= dec[s]
siz	= siz[s]
;
; output
ofil = samname+'_2mass.tbl'
filestamp,ofil
openw,ol,ofil,/get_lun
;
; header
printf,ol,'\ '+samname+' 2mass batch file made on '+systime(0)
printf,ol,'\ '
printf,ol,'|id            |ra           |dec          |    size|best|'
printf,ol,'|char          |double       |double       |   float|char|'
;
; loop over index list
for i=0,n_elements(indx)-1 do $
	printf,ol,host[i],ra[i],dec[i],siz[i],'Y', $
		format='(1x,a14,1x,f13.7,1x,f13.7,1x,f8.1,1x,a4)'
;
free_lun,ol
;
return
end
