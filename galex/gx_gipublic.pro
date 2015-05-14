function gx_gipublic, gitile, visit, $
	release_num =release_num, $
	release_date=release_date, $
	release_type=release_type
;+
; GX_GIPUBLIC - test GI tile to see if it is public or not
;
; INPUTS:
;	gitile	- tile name
;
; KEYWORDS:
;
; RETURNS:
;	0 - false if not yet public
;	1 - true if public
;-
pub=0
;
; check if this is a GI observation
if strpos(gitile,'GI') lt 0 then begin
	pub=1
endif else begin
;
; check gr4 gi data
	openr,il,!GALEX_DATA+'GR4.txt',/get_lun
	rec=''
	while not eof(il) do begin
		readf,il,rec
		if strpos(rec,gitile) ge 0 then begin
			pub=1
			release_type='GR4'
			release_date='080808'
			break
		endif
	endwhile
	free_lun,il
;
; if still not public check qa reports
	if pub eq 0 then begin
;
; set visit
		if n_params(0) lt 2 then $
			vis = 0 $
		else	vis = visit
		vtile = gitile+'_'+string(vis,format='(i04)')
;
; set limiting release
		if keyword_set(release_num) then $
			lrnum = release_num $
		else	lrnum = 18
;
; check GI releases reports
		rdates=['070322','070517','070522','070607','070807','070904', $
			'070925','071018','071119','080204','080306','080303', $
			'080321','080421','080512' ]
		fspec='/home/ymir/gi_releases/GI_qareports/*visits-qareport.txt'
		flist=file_search(fspec,count=nf)
		for i=0,nf-1 do begin
			sta=strsplit(flist(i),'/',/extract,count=n)
			rnum=fix(strmid(sta(n-1),3,2))
			openr,il,flist(i),/get_lun
			while not eof(il) do begin
				readf,il,rec
				if strpos(rec,vtile) ge 0 then $
					if rnum le lrnum then begin
						pub=1
						release_type='GI'+ $
						    string(rnum,format='(i02)')
						release_date=rdates(rnum-9)
						break
					endif
			endwhile
			free_lun,il
			if pub eq 1 then break
		endfor
	endif
endelse
;
return,pub
end
