function gx_read_tiles,tfile
;+
; GX_READ_TILES
;
;	gx_read_tiles, tfile
;
; INPUTS:
;
;	tfile	- list of tiles
;
; RETURNS:
;
;	string array of tile names
;
; HISTORY:
;
;	02jun10 jdn	- initial version
;-
;
if n_params(0) lt 1 then begin
	print, 'Usage: tlist = gx_read_tiles(tfile)'
	return,''
endif
;
readcol,tfile,tlist,format='a',/silent,comment='#'
;
return,tlist
end
