pro galdb2sqlite
;+
; galdb2sqlite - convert the galdb database into an sqlite database
;-
common glgadb_info
common galdb_info
common sndb_info
;
; GALDAT
;
tags=strlowcase(tag_names(galdat))
ntags=n_elements(tags)
;
print,'Creating galdb2sqlite.sql...'
n=n_elements(galdat)
filestamp,'galdb2sqlite.sql',/arch
openw,ol,'galdb2sqlite.sql',/get_lun
printf,ol,'CREATE TRIGGER insert_gdkey_mod_time AFTER INSERT on galdat'
printf,ol,'BEGIN'
printf,ol,"  UPDATE galdat set mod_time = DATETIME('NOW') WHERE rowid=new.rowid;"
printf,ol,'END;'
printf,ol,'BEGIN TRANSACTION;'
;
c = ','
pre = 'INSERT INTO galdat ('
for i=0,ntags-3 do pre = pre + tags[i] + ','
pre = pre + tags[i] + ') VALUES ('
for i=0L,n-1 do begin
	cmd = pre
	for j=0,ntags-3 do cmd = cmd + sqlite_insert(galdat[i].(j)) + ','
	cmd = cmd + sqlite_insert(galdat[i].(j)) + ');'
	printf,ol,cmd
endfor
printf,ol,'COMMIT;'
free_lun,ol
print,'Done.'
;
; GLGADAT
;
tags=strlowcase(tag_names(glgadat))
ntags=n_elements(tags)
;
print,'Creating glgadb2sqlite.sql...'
n=n_elements(glgadat)
filestamp,'glgadb2sqlite.sql',/arch
openw,ol,'glgadb2sqlite.sql',/get_lun
printf,ol,'BEGIN TRANSACTION;'
;
c = ','
pre = 'INSERT INTO glgadat ('
for i=0,ntags-3 do pre = pre + tags[i] + ','
pre = pre + tags[i] + ') VALUES ('
for i=0L,n-1 do begin
	cmd = pre
	for j=0,ntags-3 do cmd = cmd + sqlite_insert(glgadat[i].(j)) + ','
	cmd = cmd + sqlite_insert(glgadat[i].(j)) + ');'
	printf,ol,cmd
endfor
printf,ol,'COMMIT;'
free_lun,ol
print,'Done.'
;
; SNDAT
;
tags=strlowcase(tag_names(sndat))
ntags=n_elements(tags)
;
print,'Creating sndb2sqlite.sql...'
n=n_elements(sndat)
filestamp,'sndb2sqlite.sql',/arch
openw,ol,'sndb2sqlite.sql',/get_lun
printf,ol,'CREATE TRIGGER insert_sdkey_mod_time AFTER INSERT on sndat'
printf,ol,'BEGIN'
printf,ol,"  UPDATE sndat set mod_time = DATETIME('NOW') WHERE rowid=new.rowid;"
printf,ol,'END;'
printf,ol,'BEGIN TRANSACTION;'
;
c = ','
pre = 'INSERT INTO sndat ('
for i=0,ntags-3 do pre = pre + tags[i] + ','
pre = pre + tags[i] + ') VALUES ('
for i=0L,n-1 do begin
	cmd = pre
	for j=0,ntags-3 do $
		if strcmp(tags[j],'hind') eq 1 then $
			cmd = cmd + sqlite_insert(sndat[i].(j)+1L) + ',' $
		else	cmd = cmd + sqlite_insert(sndat[i].(j)) + ','
	cmd = cmd + sqlite_insert(sndat[i].(j)) + ');'
	printf,ol,cmd
endfor
printf,ol,'COMMIT;'
free_lun,ol
print,'Done.'
return
end
