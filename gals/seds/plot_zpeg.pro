pro plot_zpeg,file,ids,zpegdata=zpegdata,reread=reread,singlefit=singlefit, $
                  yfnur=yfnur, psout=psout, _extra=_extra
;+
;	plot_zspec - plot ZPEG fit file
;
; CALLING SEQUENCE:
;	plot_zspec,galid,/ps,/png,/glgaout,/verbose
;
; INPUTS:
;	galid	- galaxy id as specified in galdat struct
;
; KEYWORDS:
;	ps	- create postscript file of plot
;	png	- convert postscript file to png (implies /ps)
;	pdf	- convert postscript file to pdf (implies /ps)
;	glgaout	- put gif file in appropriate !GLGA_ROOT dir (implies /ps,/png)
;	verbose	- extra output to screen
;
; HISTORY:
; $Id: plot_zpeg.pro,v 1.1 2013/06/10 16:03:50 neill Exp $
;	08-JUN-2013, jdn - updated for GLGA
;-
if n_elements(ids) ne 0 then $
	idsin=ids $
else 	begin
  	print,'PLOT_ZPEG: Usage - plot_zpeg,<fit_file>,<id>'
	return
endelse

  fileres=file
  if n_elements(zpegdata) le 0 then $
  	read_zpegres,fileres,zpegdata,reread=reread,/nosave

  indices=lonarr(n_elements(idsin))
  for k=0l,n_elements(idsin)-1 do indices(k)=(where(zpegdata.id eq idsin(k)))(0)
  good=where(indices ge 0, ngood)
  if ngood gt 0 then $
  	data=zpegdata(indices[good]) $
  else	begin
  	print,'No good matches found, returning'
	return
  endelse

  Ids=data.ID

  iIds_withsol=where(data.nsol ge 1,nIds_withsol)
  if nIds_withsol ne 0 then Ids_withsol=data(iIds_withsol).ID

  nIds=n_elements(Ids)

  for i=0L,nIds-1L do begin
     if n_elements(linesfluxesfile) ne 0 and i eq 0 then begin
        openw,ulines,linesfluxesfile,/get_lun
     endif

     if (data(i).nsol ge 1 or keyword_set(withnosol)) then begin
        print,'object # ',i+1,'/',nIds,' ',Ids[i]

        if (keyword_set(yfnur)) then myyr=yfnur else myyr=[0,0]
        plot_bestfit_zpeg_gals,Ids(i),fileres,/fnu,_extra=_extra, $
		/zpeg_scen,myyr=myyr,/ylog,psout=psout,singlefit=singlefit
     endif

     if not keyword_set(psout) then begin
	     q=''
	     read,'next <cr>: ',q
     endif
  endfor
end

