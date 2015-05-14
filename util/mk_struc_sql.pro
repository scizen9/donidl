pro mk_struc_sql,stc,tabnam,create=create,verbose=verbose
;+
; mk_struc_sql - make a table for input to sql database from an IDL struc
;-
; test structure
	nt = n_tags(stc)
	for i=0,nt-1 do begin
		t=n_tags(stc.(i))
		if t gt 1 then begin
			print,'MK_STRUC_SQL: Error - only un-nested structures can be used'
			return
		endif
	endfor
;
; tag names
	names = tag_names(stc)
;
; get user
	user = getenv('USER')
;
; number of items
	ni = n_elements(stc)
;
; open output file
	ofil = tabnam+'.sql'
	filestamp,ofil,/arch
	openw,ol,ofil,/get_lun
	printf,ol,'--'
	printf,ol,'-- Created by MK_STRUC_SQL.PRO on '+systime(0)
	printf,ol,'--'
	if keyword_set(create) then begin
		printf,ol,''
		printf,ol,"SET client_encoding = 'UTF8';"
		printf,ol,"SET standard_conforming_strings = off;"
		printf,ol,"SET check_function_bodies = false;"
		printf,ol,"SET client_min_messages = warning;"
		printf,ol,"SET escape_string_warning = off;"
		printf,ol,''
		printf,ol,"SET search_path = public, pg_catalog;"
		printf,ol,''
		printf,ol,"SET default_tablespace = '';"
		printf,ol,''
		printf,ol,"SET default_with_oids = true;"
		printf,ol,''
		printf,ol,'--'
		printf,ol,'-- Name: '+tabnam+'; Type: TABLE; Schema: public; Owner: '+user+'; TableSpace:'
		printf,ol,'--'
		printf,ol,''
		printf,ol,'CREATE TABLE '+tabnam+' ('
		for i=0,nt-2 do begin
			ty=sql_type(stc[0].(i))
			if strlen(ty) gt 0 then $
				printf,ol,'    '+strlowcase(names[i])+' '+ty+',' $
			else	print,'Bad item: ',names[i]
		endfor
		ty = sql_type(stc[0].(i))
		if strlen(ty) gt 0 then $
			printf,ol,'    '+strlowcase(names[i])+' '+ty $
		else	print,'Bad item: ',names[i]
		printf,ol,');'
		printf,ol,''
		printf,ol,'ALTER TABLE public.'+tabnam+' OWNER TO '+user+';'
	endif
	tb='	'

	tlist=''
	for i=0,nt-2 do $
		tlist = tlist+strlowcase(names[i])+','
	tlist = tlist+strlowcase(names[i])

	printf,ol,"COPY "+tabnam+" ("+tlist+") from stdin NULL as '-';"
	
	if keyword_set(verbose) then print,' '
	for i=0L,ni-1L do begin
		orec= ''
		for j=0,nt-2 do $
			orec=orec + strtrim(strn(stc[i].(j)),2) + tb
		orec=orec + strtrim(strn(stc[i].(j)),2)
		printf,ol,orec
		if keyword_set(verbose) and (i mod 100) eq 0 then $
			print,string(13B),i+1,'/',ni,'  ',string(stc[i].(0)), $
				form='($,a1,i9,a1,i9,a,a)'
	endfor
	if keyword_set(verbose) then $
			print,string(13B),i+1,'/',ni,'  ',string(stc[i-1].(0)), $
				form='($,a1,i9,a1,i9,a,a)'

	printf,ol,'\.'

	free_lun,ol
	if keyword_set(verbose) then print,' '
	return
end
