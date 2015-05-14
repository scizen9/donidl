pro galdb_browse,start=start,sort_ra=sort_ra,sort_mag=sort_mag, $
	sort_name=sort_name,sort_size=sort_size
;
common galdb_info
nmax = n_elements(galdat)-1L
;
i0=0L
if keyword_set(start) then begin
	if size(start,/type) eq 7 and strtrim(start,2) ne '' then begin
		w=gfind(strtrim(start,2),count=nw)
		if nw gt 1 then begin
			print,'Error - ambiguous id: ',start
			return
		endif else if nw lt 1 then begin
			print,'Error - not found: ',start
			return
		endif else i0 = w[0]
	endif else if (size(start,/type) ge 12 and size(start,/type) le 15) or $
		(size(start,/type) ge 1 and size(start,/type) le 3) then $
		i0 = start - 1L
endif
;
s = lindgen(n_elements(galdat))
if keyword_set(sort_ra) then begin
	s=sort(galdat.ra)
endif else if keyword_set(sort_name) then begin
	s=sort(galdat.hlname)
endif else if keyword_set(sort_mag) then begin
	s=sort(galdat.nuv_int_mag)
endif else if keyword_set(sort_size) then begin
	s=sort(galdat.majax)
	s=reverse(s)
endif
;
gdir=!GLGA_ROOT+'data/'
fdir='~/gals/all/fits/'
sdir='~/gals/all/seds/'
;
q=''
j0 = where(s eq i0, nj)
if nj ne 1 then begin
	print,'Error - index not found: ',i0
	return
endif
j0 = j0[0]
for j=j0,nmax do begin
	i = s[j]
	name = strtrim(galdat[i].hlname,2)
	pdir = gdir + glga_degdir(galdat[i].ra) + '/plots/'
;
; basic data
	print,i+1L,name,galdat[i].type,galdat[i].ra,galdat[i].dec,galdat[i].cz,$
		format='(i7,2x,a-25,a-8,2x,2f11.6,2x,f9.1)'
	print,galdat[i].majax,galdat[i].minax,galdat[i].pa,galdat[i].sne, $
		galdat[i].sample,format='(3f7.1,2x,a,2x,a)'
	print,galdat[i].fuv_int_mag,galdat[i].nuv_int_mag,galdat[i].uJ_int_mag,$
		galdat[i].bJ_int_mag,galdat[i].vJ_int_mag,galdat[i].u_int_mag,$
		galdat[i].g_int_mag,galdat[i].r_int_mag,galdat[i].i_int_mag,$
		galdat[i].z_int_mag, format='(10f7.2)'
	print,galdat[i].j_int_mag,galdat[i].h_int_mag,galdat[i].k_int_mag,$
		galdat[i].w1_int_mag,galdat[i].w2_int_mag,galdat[i].w3_int_mag,$
		galdat[i].w4_int_mag,galdat[i].i12m_int_mag, $
		galdat[i].i25m_int_mag,galdat[i].i60m_int_mag, $
		galdat[i].i100m_int_mag, format='(11f7.2)'
; GLGA
	read,'Next <cr>: ',q
	if strupcase(strtrim(q,2)) eq 'Q' then break
endfor
return
end
