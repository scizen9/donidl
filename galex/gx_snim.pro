pro gx_snim,ifile,nox=nox, $
	reg_overwrite=reg_overwrite,img_overwrite=img_overwrite
;+
; gx_snim - Read in images in ifile and organize in subdirs 
;	according to SN type and name
;
; USAGE: gx_snim, ifile, /nox, /reg_overwrite, /img_overwrite
;
; INPUTS:
;	ifile	- list of SNe and their GALEX images from gx_snimlist.pro
;
; KEYWORDS:
;	nox	- set to not use 'X' output device (for use with screen)
;	reg_overwrite - set to overwrite the ds9 reg file
;	img_overwrite - set to overwrite existing galex images
;
; HISTORY:
;	25oct07 jdn	- initial revision
;-
;
if n_params(0) lt 1 then begin
	print,'GX_SNIM Usage: gx_snim,file [, /nox, /reg_overwrite, /img_overwrite]'
	return
endif
;
; open input file
openr,il,ifile,/get_lun
rec=''
ntim=0L
nim=0L
nsn=0L
maxims = 10000
;
; loop over input
while not eof(il) do begin
;
; read record
	readf,il,rec
;
; SN record
	if strpos(rec,'SN') eq 0 then begin
;
; process old SN if any images found
		if nim gt 0 then begin
			nsn=nsn+1L	; count it
;
; get output dir
			odir=ssn+'/'
			if not file_test(odir) then $
				file_mkdir,odir
;
; get coordinate strings for ds9 reg file and gx_subim program
			scoostr=adstring(snra,sndec,1)
			hcoostr=adstring(hra,hdec,1)
			srastr=''
			hrastr=''
			for i=0,2 do begin
				if i ne 2 then begin
					srastr=srastr+gettok(scoostr,' ')+':'
					hrastr=hrastr+gettok(hcoostr,' ')+':'
				endif else begin
					srastr=srastr+gettok(scoostr,' ')
					hrastr=hrastr+gettok(hcoostr,' ')
				endelse
			endfor
			sdecstr=''
			hdecstr=''
			for i=0,2 do begin
				if i ne 2 then begin
					sdecstr=sdecstr+gettok(scoostr,' ')+':'
					hdecstr=hdecstr+gettok(hcoostr,' ')+':'
				endif else begin
					sdecstr=sdecstr+gettok(scoostr,' ')
					hdecstr=hdecstr+gettok(hcoostr,' ')
				endelse
			endfor
;
; write ds9 *.reg file
			if not file_test(odir+ssn+'.reg') or $
				keyword_set(reg_overwrite) then $
				gx_snreg,odir,snhost,hrastr,hdecstr, $
				    sn,srastr,sdecstr,snhtp
;
; prepare to get sub images
			cd,odir
			openw,ll,ssn+'.log',/get_lun,/append
			printf,ll,'# '+sn+', '+snhost+', '+snhtp+', '+systime(0)
			good=where(strlen(imd) gt 0)
			imd=imd(good)
			subv=subv(good)
;
; check for mains (coadds)
			gx_chkmain,imd,subv,mmd,msubv
			for i=0,n_elements(mmd)-1 do begin
				if keyword_set(img_overwrite) then $
				    cmd='gx_subim '+mmd(i)+' '+msubv(i)+' '+ $
					hrastr+' '+hdecstr+' '+hpstr+' ovwrt' $
				else $
				    cmd='gx_subim '+mmd(i)+' '+msubv(i)+' '+ $
					hrastr+' '+hdecstr+' '+hpstr
				print,cmd
				printf,ll,cmd
				spawn,cmd
			endfor
			free_lun,ll
			sn_rgb_galex_asinh,/plotaps,nox=nox
			cd,'..'
		endif	; nim gt 0
;
; get data for new SN
		print,' '
		print,rec
		nim=0L
		imd=strarr(maxims)
		subv=strarr(maxims)
		for i=0,1 do sn=gettok(rec,' ')
		tsn  = sn
		ssn  = gettok(tsn,'?')		; remove ?s
		snra = double(gettok(rec,' '))
		sndec= double(gettok(rec,' '))
		tystr= gettok(rec,' ')
		snhost = gettok(rec,' ')
		hra  = double(gettok(rec,' '))
		hdec = double(gettok(rec,' '))
		hsiz = float(gettok(rec,' ')) ; isophotal diameter (D25, arcsec)
		hpix = ((hsiz * 2.0) / 1.5)>256.0 ; convert to galex pixels
		hpstr= strn(fix(hpix+0.5))    ; string
		hinc = float(gettok(rec,' '))
		snhtp= gettok(rec,' ')
		snhtn= fix(gettok(rec,' '))
		v    = float(gettok(rec,' '))
	endif else begin	; strpos(rec,'SN') eq 0
;
; GALEX image record
	    if strtrim(rec,2) ne '' then begin
		rad = float(gettok(rec,' '))
		subv(nim) = string(fix(gettok(rec,' ')),format='(i02)')
		tile = gettok(rec,' ')
		imd(nim) = gettok(rec,' ')
		nim = nim + 1L
		ntim=ntim + 1L
	    endif
	endelse
endwhile
;
free_lun,il
;
print,'# SN: ',nsn,'  # IM: ',ntim
;
return
end
