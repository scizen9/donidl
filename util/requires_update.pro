function requires_update,fname,dir0,dir1,update=update,missing=missing
;+
; requires_update - Test if file in dir1 needs to be updated from a copy in dir0
;
; RETURNS:
;	0	- if no update needed
;	1	- if update required (dir1 file older or missing)
;
; KEYWORDS:
;	update	- set to only return true for update but not for missing
;	missing - set to only return true if missing, but not for update
;-
;
f0 = file_info(dir0+'/'+fname)
f1 = file_info(dir1+'/'+fname)
;
; file missing in dir1
if not f1.exists and not keyword_set(update) then return,1
;
; file in dir1 needs update
if f0.exists and f1.exists and f0.mtime gt f1.mtime and $
	not keyword_set(missing) then return,1
return,0
end
