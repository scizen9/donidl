pro sndb_restore
;+
; sndb_restore - restore save file into common variables
;-
; common variable for sndat, snhphsrc
COMMON sndb_info, sndat
;
; test for save file
savfile=!SNE_DATA+'sndb_info.sav'
if file_test( savfile, /read, /regular ) then begin
	restore,savfile
	return
endif
;
; shouldn't get here
print,'SNDB_RESTORE: Error - no save file found: ',savfile,format='(a,a)'
;
return
end
