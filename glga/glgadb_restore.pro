pro glgadb_restore
;+
; glgadb_restore - restore save file into common variables
;-
; common variable for galdat, snhphsrc
COMMON glgadb_info, glgadat
;
; test for save file
savfile=!GLGA_DATA+'glgadb_info.sav'
if file_test( savfile, /read, /regular ) then begin
	restore,savfile
	return
endif
;
; shouldn't get here
print,'GLGA_STATUS_RESTORE: Error - no save file found: ',savfile
;
return
end
