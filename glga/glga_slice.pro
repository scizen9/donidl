pro glga_slice,lfile,sdss=sdss,galex=galex,twomass=twomass,csp=csp,wise=wise, $
	silent=silent
;+
; glga_slice - get the processing status of GLGA images
;
; lfile - list of objects with one line per object with these columns:
;       id
;       ra,dec  - degrees
;       majdiam,mindiam - arcmin
;       pa      - degrees
;
; keywords:
;	sdss,galex,twomass,csp,wise - data to analyze
;	silent	- supress output
;
;-
;
; set defaults and check keywords
ddir='uv/'
fpx='_'
dty='galex'
ptl='-nd-int.fit*'
atl='-fd-int.fit*'
jtl='_xd_2color.jpg'
rfl='_NUV_'
verb = (not keyword_set(silent))
if keyword_set(sdss) then begin
	ddir='sdss/'
	fpx='_sdss_'
	dty='sdss'
	ptl='_r.fit*'
	atl='_g.fit*'
	jtl='_x.jpg'
	rfl='_r_'
endif
if keyword_set(galex) then begin
	ddir='uv/'
	fpx='_'
	dty='galex'
	ptl='-nd-int.fit*'
	atl='-fd-int.fit*'
	jtl='_xd_2color.jpg'
	rfl='_NUV_'
endif
if keyword_set(twomass) then begin
	ddir='2mass/'
	fpx='_2mass_'
	dty='2mass'
	ptl='_k.fit*'
	atl='_j.fit*'
	jtl='_x.jpg'
	rfl='_k_'
endif
if keyword_set(csp) then begin
	ddir='csp/'
	fpx='_csp_'
	dty='csp'
	ptl='_V.fit*'
	atl='_B.fit*'
	jtl='_x.jpg'
	rfl='_V_'
endif
if keyword_set(wise) then begin
	ddir='wise/'
	fpx='_wise_'
	dty='wise'
	ptl='_w1.fit*'
	atl='_w2.fit*'
	jtl='_wx.jpg'
	rfl='_w1_'
endif
;
; status request list
stat_req=['needs img', 'needs dss', 'needs jpg', 'needs qa', 'needs mask', $
	  'needs phot', 'needs plots', 'is complete']
nsr = n_elements(stat_req)
;
print,'Choose status slice:'
for i=0,nsr-1 do $
	print,i,stat_req[i],format='(i5,2x,a)'
p=-1
while p lt 0 or p ge nsr do $
	read,'Enter slice number: ',p
;
; check consistency
if p eq 1 and strpos(dty,'galex') lt 0 then begin
	print,dty+' data does not require dss images'
	return
endif
;
stat_name = stat_req[p]
strput,stat_name,'_',strpos(stat_name,' ')
;
; open log file
ofil = 'glga_'+dty+'_'+stat_name+'.glga'
filestamp,ofil,/arch
openw,ol,ofil,/get_lun
printf,ol,'# GLGA_SLICE - '+systime(0)
printf,ol,'# LIST: '+lfile
printf,ol,'# DATA: '+dty+' '+stat_req[p]
printf,ol,'# HOST                     ra          dec           majdiam  mindiam    pa     type'
;
; read in sample data
readcol,lfile, id, ra, dec, majdiam, mindiam, pa, type, $
	        format='a,d,d,f,f,f,a', /silent
;
; define top level directory
deg = string(floor(ra), format='(i3.3)')+'D'
;
; used to generate file names
id = strcompress(id,/rem)

imgbase=!GLGA_ROOT+'data/'+deg+'/'+ddir+'fits/'+id
jpgbase=!GLGA_ROOT+'data/'+deg+'/'+ddir+'jpg/'+id
auxbase=!GLGA_ROOT+'data/'+deg+'/aux/'+id+fpx
mskfile=!GLGA_ROOT+'data/'+deg+'/aux/'+id+fpx+'mask.dat'
ellfile=!GLGA_ROOT+'data/'+deg+'/aux/'+id+'_ellipse.dat'
phtfile=!GLGA_ROOT+'data/'+deg+'/photometry/'+id+rfl+'total.dat'
pltbase=!GLGA_ROOT+'data/'+deg+'/plots/'+id+fpx
dssbase=!GLGA_ROOT+'data/'+deg+'/dss/fits/'+id
;
; starting index
nloop = n_elements(id)
;
; loop over object list
count=0L
for i=0L, nloop-1 do begin
;
; print status
  if verb then $
    print,string(13B),i+1,'/',nloop,id[i],deg[i], $
	format = '($,a1,i6,a1,i6,2x,a-25,a5,a)'
;
; get image status first
  pimgfile = imgbase[i] + ptl
  aimgfile = imgbase[i] + atl
  ilist = file_search(pimgfile, count=nim)
  if nim le 0 then ilist = file_search(aimgfile, count=nim)
  if nim gt 0 then imfo = file_info(ilist[0])
;
; get slice info
  pout = (1 eq 0)	; default no print
  case p of

  ; need img
  0: if nim le 0 then pout = (1 eq 1)

  ;needs dss
  1: if nim gt 0 then begin
	dssfile = dssbase[i] + '_dss*.fit*'
	dlist = file_search(dssfile, count=nf)
	if nf le 0 then pout = (1 eq 1)
     endif

  ;needs jpg
  2: if nim gt 0 then begin
	jpgfile = jpgbase[i] + jtl
	jlist = file_search(jpgfile, count=nf)
	if nf gt 0 then begin
		jinfo=file_info(jlist[0])
		if imfo.mtime gt jinfo.mtime then pout = (1 eq 1)
	endif else pout = (1 eq 1)
     endif else pout = (1 eq 1)

  ;needs qa
  3: if nim gt 0 then begin
	qalogfile = imgbase[i] + '_qa.txt'
	qa=glga_read_qa_stat(qalogfile)
	if qa.ts lt imfo.mtime then pout = (1 eq 1)
     endif else pout = (1 eq 1)

  ;needs mask
  4: if nim gt 0 then begin
	mskfile = auxbase[i] + 'mask.dat'
	pscfile = auxbase[i] + 'pointsrc.dat'
	roifile = auxbase[i] + 'roi.dat'
	mlist = file_search(mskfile, count=nf)
	if nf gt 0 then begin
		minfo=file_info(mlist[0])
		if imfo.mtime gt minfo.mtime then pout = (1 eq 1)
	endif else begin
		if file_exist(pscfile) or file_exist(roifile) then $
			pout = (1 eq 1)
	endelse
     endif else pout = (1 eq 1)

  ;needs phot
  5: if nim gt 0 then begin
	plist = file_search(phtfile[i], count=nf)
	if nf gt 0 then begin				; check currency of phot
		pinfo=file_info(plist[0])
		if imfo.mtime gt pinfo.mtime then $	; new image
			pout = (1 eq 1)
		if file_exist(ellfile[i]) then begin
			einfo=file_info(ellfile[i])
			if einfo.mtime gt pinfo.mtime then $	; new ell file
				pout = (1 eq 1)
		endif
		if file_exist(mskfile[i]) then begin
			minfo=file_info(mskfile[i])
			if minfo.mtime gt pinfo.mtime then $	; new mask file
				pout = (1 eq 1)
		endif
	endif else pout = (1 eq 1)
     endif else pout = (1 eq 1)

  ;needs plots
  6: if nim gt 0 then begin
	pltfile = pltbase[i] + 'profile.jpg'
	plist = file_search(pltfile, count=nf)
	if nf gt 0 then begin
		pinfo=file_info(plist[0])
		if imfo.mtime gt pinfo.mtime then pout = (1 eq 1)
	endif else pout = (1 eq 1)
     endif else pout = (1 eq 1)

  ;complete
  7: if nim gt 0 then begin
	qalogfile = imgbase[i] + '_qa.txt'
	qa=glga_read_qa_stat(qalogfile)
	plist = file_search(phtfile[i], count=nf)
	if nf gt 0 then begin
		pinfo=file_info(plist[0])
		if imfo.mtime lt qa.ts and qa.ts lt pinfo.mtime then $
			pout = (1 eq 1)
	endif
     endif

  else: print,'Error, unknown slice number: ',p
  endcase
;
; print out if pout true
  if pout then begin
	printf,ol,id[i],ra[i],dec[i],majdiam[i],mindiam[i],pa[i],type[i], $
		format='(a-25,2f13.8,3f9.3,2x,a)'
	count = count + 1L
	if verb then print,' '
  endif
;
endfor	; loop over object list
free_lun,ol
;
if verb then begin
;    print,string(13B),'                                                    ', $
;	format='($,a1,a)'
    print,' '
    print,stat_req[p],' - found this many: ',count,form='(a,a,i5)'
endif
;
end
