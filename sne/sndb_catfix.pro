pro sndb_catfix,nosave=nosave
;
;	fix the sndat structure in lowz_sne_info and re-save the file
;
; common variable for sndat
COMMON sndb_info
;
; test for save file
if n_elements(sndat) le 0 then begin
	print,'sndat structure not accessable, returning'
	return
endif
;
; get tag names
tnams=tag_names(sndat)
nt   =n_tags(sndat)
;
; check fix file
fixfile=!SNE_DATA+'/sndb_cat.fix'
if not file_test(fixfile,/read) then begin
	print,'Fixfile not found: ',fixfile
	return
endif
;
; open fix file
openr,il,fixfile,/get_lun
rec=''
;
; loop until eof
while not eof(il) do begin
	readf,il,rec
	if strmid(rec,0,1) ne '#' then begin	; skip comments
		sta=strsplit(rec,/extract,count=nf)	; get required fields
		if nf eq 4 then begin
;
; find sn
			sn=sta(0)
			s=where(strpos(sndat.id,sn) ge 0, n)
			if n eq 1 then begin
;
; find tag
				tag=strupcase(sta(1))
				t=where(strcmp(tnams,tag),nt)
				if nt eq 1 then begin
;
; get format and do the right thing
					case strupcase(sta(2)) of
						'A':	val=sta(3)
						'I':	val=fix(sta(3))
						'F':	val=float(sta(3))
						'D':	val=double(sta(3))
						else:	t=[-1]
					endcase
;
; last check before we fix it
					if t(0) ge 0 then begin
						sndat[s].(t(0)) = val
						;
						; time stamp
						sndat[s].mod_time = systime(1)
					endif else print,'Bad format: ',sta(2)
				endif else print,'Error for tag: ',sta(1)
			endif else print,'Error for SN: ',sn,', nfound: ',n
		endif else print,'Bad record: ',rec
	endif
endwhile
;
; create save file
savfile=!SNE_DATA+'/sndb_info.sav'
if not keyword_set(nosave) then begin
	filestamp,savfile,/verbose,/arch
	print,'Saving lowz SNe info to: ',savfile
	save,sndat,filename=savfile,/verbose
endif
;
return
end
