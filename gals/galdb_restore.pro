pro galdb_restore
;+
; galdb_restore - restore save file into common variables
;-
; common variable for galdat, snhphsrc
COMMON galdb_info, galdat, gphsrc
;
; test for source file
if n_elements(gphsrc) le 0 then galdb_src_read
;
; test for save file
savfile=!GALS_DATA+'galdb_info.sav'
if file_test( savfile, /read, /regular ) then begin
	restore,savfile
	return
endif
;
; shouldn't get here
print,'SNDB_RESTORE: Error - no save file found: ',savfil,format='(a,a)'
;
return
end
