pro wise_driz_bp16, name, preserve=preserve
;+
; WISE_DRIZ_BP16 - write out driz images with BITPIX = 16
;
; Usage: wise_driz_bp16,hostname [, /preserve ]
;
; PARAMS:
;	name	- host name for galaxy images.  Looks in 
;		  !GLGA_WISE_DATA+'data/sort/'+name for images.
;
; KEYWORDS:
;	preserve	- set to keep original file in <filename>+'_orig',
;			  else original file is deleted.
;
; HISTORY:
;	13jun2012	- jdn, initial version
;
;-
; loop over names list
for j=0,n_elements(name)-1 do begin
	;
	; look in glga wise data directory for each host
	ddir = !GLGA_WISE_DATA+'data/sort/'+name[j] + '/'
	;
	; get list of files in driz directories
	flist = file_search(ddir+'driz?/'+name[j]+'_*.fit*',count=nf)
	;
	; got files?
	if nf gt 0 then begin
		;
		; loop over files
		for i=0,nf-1 do begin
			file = flist[i]	; this file
			;
			; only gzip mask files (already BITPIX=8)
			if strpos(file,'_msk') ge 0 then begin
			    ;
			    ; only gzip if needed
			    if strpos(file,'.gz') lt 0 then $
				spawn,'gzip '+file
			;
			; not a (BP=8) mask file
			endif else begin
			    hdr = headfits(file)
			    bp = sxpar(hdr,'BITPIX')
			    ;
			    ; BITPIX = 16 already
			    if bp eq 16 then begin
				; only gzip if needed
				if strpos(file,'.gz') lt 0 then $
					spawn,'gzip '+file
			    ;
			    ; not BITPIX = 16, so read in and re-scale
			    endif else begin
			    	data = mrdfits(file,0,hdr,/fscale,/silent)
			    	;
			    	; keep if requested, else remove
			    	if keyword_set(preserve) then $
				    	spawn,'mv '+file+' '+file+'_orig' $
			    	else    spawn,'rm '+file
		    	    	; rescale to BITPIX = 16 and compress
			    	mymwrfits,data,file,hdr, $
					/create,/compress,/iscale
			    endelse
			endelse
		endfor	; loop over files
	; no files
	endif else print,'No files found: '+ddir+'driz?/'+name[j]+'_*.fit*'
endfor	; loop over names list
;
return
end
