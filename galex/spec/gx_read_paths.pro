function gx_read_paths,pfile,npth=npth,disk=disk,vsn=vsn,plan=plan,base=base, $
	owc=owc,pat=pat,vis=vis,try=try
;+
; GX_READ_PATHS
;
;	gx_read_paths, pfile
;
; INPUTS:
;
;	pfile	- list of paths
;
; RETURNS:
;
;	string array of paths
;
; KEYWORDS:
;
;	npth	- return variable for number of paths read in
;
; HISTORY:
;
;	02jun10 jdn	- initial version
;-
;
plist = ''
if n_params(0) lt 1 then begin
	print, 'Usage: plist = gx_read_paths(pfile)'
endif else begin
	if not file_exist(pfile) then begin
		print,'GX_READ_PATHS - Error: file not found: ',pfile
	endif else begin
		readcol,pfile,plist,format='a',/silent,comment='#'
		npth = n_elements(plist)
		disk = strarr(npth)	; home disk
		vsn  = strarr(npth)	; vsn number
		plan = strarr(npth)	; plan id number
		base = strarr(npth)	; tile base name
		owc  = strarr(npth)	; optical wheel char
		pat  = strarr(npth)	; pattern or grid
		vis  = strarr(npth)	; visit directory
		try  = strarr(npth)	; try directory
		for i = 0, npth - 1 do begin
			sta = strsplit(plist[i],'/',/extract)
			disk[i] = sta[1]
			vsn[i]	= sta[4]
			tmp	= sta[5]
			plan[i]	= gettok(tmp,'-')
			if strpos(tmp,'PAT') ge 0 then begin
				base[i] = strmid(tmp,0,strpos(tmp,'PAT')-1)
				pat[i]  = strmid(tmp,strpos(tmp,'PAT'))
			endif else if strpos(tmp,'GRID') ge 0 then begin
				base[i] = strmid(tmp,0,strpos(tmp,'GRID')-1)
				pat[i]  = strmid(tmp,strpos(tmp,'GRID'))
			endif else base[i] = tmp
			owc[i]	= sta[6]
			vis[i]	= sta[8]
			try[i]	= sta[9]
		endfor
	endelse
endelse
;
return,plist
end
