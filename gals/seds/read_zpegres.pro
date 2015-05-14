pro read_zpegres,file,data,zpeginfo,verbose=verbose,reread=reread, $
	named_struct=named_struct,nosave=nosave
;; data is a structure with 
;;  - ID
;;  - xpixypix
;;  - radec
;;  -

  openr,sav,file+'.sav',/get_lun,error=error
  if (error eq 0) then begin
     close,sav
     free_lun,sav
  endif
  if (keyword_set(verbose)) then begin
     if error ne 0 then begin
        print,'error=',error,strmessage(error)
        print,'if error <>0, the .sav file probably does not exist. No problem, we will create it !'
        print,'if /reread is set, we will create it anyway...'
     endif
  endif

  if (error ne 0 or keyword_set(reread)) then begin
     if (keyword_set(verbose)) then print,'making template'
     make_template_from_zpeg,file,template_zpeg,nfils
     if (keyword_set(verbose)) then print,'reading zpeg info'
     zpeg_read_infos,file,zpeginfo,nlines_header=nlines_header
     template_names=zpeginfo.templates
     filters_names=zpeginfo.filters
     
     
     if (keyword_set(verbose)) then print,'Reading data!'
     tab_zpeg=read_ascii(file,template=template_zpeg,data_start=nlines_header)

     ngals=n_elements(tab_zpeg.ID) 

     if (keyword_set(verbose)) then print,'data read !'

     myarr=dblarr(nfils)
     mmarr=fltarr(3)            ; best min max

     if (zpeginfo.version le 4.0) then begin
        ablock={zpegblock,z:0.0 ,zmin:0.0 ,zmax:0.0, m:0.0, mmin:0.0, mmax:0.0, itemplate:0, age:0.0, norm:0d0, ebv:0.0, absmags:myarr}
     endif
     if (zpeginfo.version gt 4.0 and zpeginfo.version lt 4.3) then begin
        ablock={zpegblock,z:mmarr, mass:mmarr, itemplate:0, age:0.0, norm:0d0, ebv:0.0, specsfr:mmarr, sfr:mmarr, absmags:myarr}
     endif
     if (zpeginfo.version ge 4.3 and zpeginfo.version le 5.02) then begin
        ablock={zpegblock,z:mmarr, mass:mmarr, itemplate:0, age:0.0, norm:0d0, ebv:0.0, specsfr:mmarr, sfr:mmarr, massstellwd:mmarr, intsfr:mmarr, absmags:myarr}
     endif
     if (zpeginfo.version ge 5.03 and zpeginfo.version lt 5.1) then begin
        ablock={zpegblock,z:mmarr, mass:mmarr, itemplate:0, age:0.0, norm:0d0, ebv:0.0, specsfr:mmarr, sfr:mmarr, massstellwd:mmarr, intsfr:mmarr, agestars:mmarr, absmags:myarr}
     endif
     if (zpeginfo.version ge 5.1 and zpeginfo.version lt 5.14 ) then begin
        ablock={zpegblock,z:mmarr, mass:mmarr, itemplate:0, age:0.0, norm:0d0, ebv:0.0, chi2sol:0.0d0, specsfr:mmarr, sfr:mmarr, massstellwd:mmarr, intsfr:mmarr, agestars:mmarr, $
                absmags:myarr, absmags_min:myarr, absmags_max:myarr, modelmags:myarr}
     endif
     if (zpeginfo.version ge 5.14) then begin
        if keyword_set(named_struct) then begin
           ablock={zpegblock,z:mmarr, mass:mmarr, itemplate:0, age:0.0, norm:0d0, ebv:0.0, dlum:0d0, chi2sol:0d0, specsfr:mmarr, sfr:mmarr, massstellwd:mmarr, intsfr:mmarr, agestars:mmarr, $
                   absmags:myarr, absmags_min:myarr, absmags_max:myarr, modelmags:myarr, modelmagerrs:myarr}
        endif else begin
           ablock={z:mmarr, mass:mmarr, itemplate:0, age:0.0, norm:0d0, ebv:0.0, dlum:0d0, chi2sol:0d0, specsfr:mmarr, sfr:mmarr, massstellwd:mmarr, intsfr:mmarr, agestars:mmarr, $
                   absmags:myarr, absmags_min:myarr, absmags_max:myarr, modelmags:myarr, modelmagerrs:myarr}
        endelse
     endif
     tab=replicate(ablock,5)

     myarr=dblarr(2*nfils)
     if keyword_set(named_struct) then begin
        print,'Warning : NAMING STRUCTURE ZPEGRES!'
        adata={zpegres, id:'', xpix:0.0, ypix:0.0, ra:0.0d0, dec:0.0d0,$
               nbands:0, chi2:0.0d0, nsol:0, zmax:0.0, Vmax:0.0, obs:myarr,$
               zphots:tab}
     endif else begin
        print,'Warning : NOT NAMING STRUCTURE (ZPEGRES)!'
        adata={ id:'', xpix:0.0, ypix:0.0, ra:0.0d0, dec:0.0d0, nbands:0, chi2:0.0d0, nsol:0, zmax:0.0, Vmax:0.0, obs:myarr, zphots:tab}
     endelse

     if (keyword_set(verbose)) then help,adata
     if (keyword_set(verbose)) then print,'allocating memory for data'
     data=replicate(adata,ngals)
     if (keyword_set(verbose)) then help,data

     if (keyword_set(verbose)) then print,'memory allocated. Now reformatting data'

     data.id=tab_zpeg.id
     data.xpix=tab_zpeg.xpix
     data.ypix=tab_zpeg.ypix
     data.ra=tab_zpeg.ra
     data.dec=tab_zpeg.dec
     data.chi2=tab_zpeg.chi2
     data.nbands=tab_zpeg.nbands
     data.nsol=tab_zpeg.nsol
     data.zmax=tab_zpeg.zmax
     data.Vmax=tab_zpeg.Vmax

     data.obs=tab_zpeg.obs0

     for l=0,4 do begin
        case l of 
           0:mytab=tab_zpeg.sol1
           1:mytab=tab_zpeg.sol2
           2:mytab=tab_zpeg.sol3
           3:mytab=tab_zpeg.sol4
           4:mytab=tab_zpeg.sol5
        endcase

        if (zpeginfo.version lt 4.1) then begin        
           data.zphots(l).z=reform(mytab(0,*))
           data.zphots(l).zmin=reform(mytab(1,*))
           data.zphots(l).zmax=reform(mytab(2,*))
           data.zphots(l).m=reform(mytab(3,*))
           data.zphots(l).mmin=reform(mytab(4,*))
           data.zphots(l).mmax=reform(mytab(5,*))
        endif else begin
           data.zphots(l).z(0:2)=[reform(mytab(0:2,*))]
           data.zphots(l).mass(0:2)=[reform(mytab(3:5,*))]
        endelse

        if (zpeginfo.version lt 4.) then begin
           data.zphots(l).itemplate=reform(mytab(4,*))
           data.zphots(l).age=reform(mytab(5,*))
           data.zphots(l).norm=reform(mytab(6,*))
;            data.zphots(l).ebv=reform(mytab(7,*))
;            data.zphots(l).absmags=reform(mytab(8:8+nfils-1,*))
           data.zphots(l).ebv=0.
           data.zphots(l).absmags=reform(mytab(7:7+nfils-1,*))
        endif
        if (zpeginfo.version eq 4.0) then begin
           data.zphots(l).itemplate=reform(mytab(6,*))
           data.zphots(l).age=reform(mytab(7,*))
           data.zphots(l).norm=reform(mytab(8,*))
           data.zphots(l).ebv=reform(mytab(9,*))
           data.zphots(l).absmags=reform(mytab(10:10+nfils-1,*))
        endif
        if (zpeginfo.version ge 4.1 and zpeginfo.version lt 4.3) then begin
           data.zphots(l).itemplate=reform(mytab(6,*))
           data.zphots(l).age=reform(mytab(7,*))
           data.zphots(l).norm=reform(mytab(8,*))
           data.zphots(l).ebv=reform(mytab(9,*))
           data.zphots(l).specsfr=reform(mytab(10:12,*))
           data.zphots(l).sfr=reform(mytab(13:15,*))
           data.zphots(l).absmags=reform(mytab(16:16+nfils-1,*))
        endif
        if (zpeginfo.version ge 4.3 and zpeginfo.version le 5.02) then begin
           data.zphots(l).itemplate=reform(mytab(6,*))
           data.zphots(l).age=reform(mytab(7,*))
           data.zphots(l).norm=reform(mytab(8,*))
           data.zphots(l).ebv=reform(mytab(9,*))
           data.zphots(l).specsfr=reform(mytab(10:12,*))
           data.zphots(l).sfr=reform(mytab(13:15,*))
           data.zphots(l).massstellwd=reform(mytab(16:18,*))
           data.zphots(l).intsfr=reform(mytab(19:21,*))
           data.zphots(l).absmags=reform(mytab(22:22+nfils-1,*))
        endif
        if (zpeginfo.version ge 5.03 and zpeginfo.version lt 5.1) then begin
           data.zphots(l).itemplate=reform(mytab(6,*))
           data.zphots(l).age=reform(mytab(7,*))
           data.zphots(l).norm=reform(mytab(8,*))
           data.zphots(l).ebv=reform(mytab(9,*))
           data.zphots(l).specsfr=reform(mytab(10:12,*))
           data.zphots(l).sfr=reform(mytab(13:15,*))
           data.zphots(l).massstellwd=reform(mytab(16:18,*))
           data.zphots(l).intsfr=reform(mytab(19:21,*))
           data.zphots(l).agestars=reform(mytab(22:24,*))
           data.zphots(l).absmags=reform(mytab(25:25+nfils-1,*))
        endif
        if (zpeginfo.version ge 5.1) then begin
           data.zphots(l).itemplate=reform(mytab(6,*))
           data.zphots(l).age=reform(mytab(7,*))
           data.zphots(l).norm=reform(mytab(8,*))
           data.zphots(l).ebv=reform(mytab(9,*))
           data.zphots(l).chi2sol=reform(mytab(10,*))
           data.zphots(l).specsfr=reform(mytab(11:13,*))
           data.zphots(l).sfr=reform(mytab(14:16,*))
           data.zphots(l).massstellwd=reform(mytab(17:19,*))
           data.zphots(l).intsfr=reform(mytab(20:22,*))
           data.zphots(l).agestars=reform(mytab(23:25,*))
           data.zphots(l).absmags=reform(mytab(26:26+nfils-1,*))
           data.zphots(l).absmags_min=reform(mytab(26+nfils:26+nfils+nfils-1,*))
           data.zphots(l).absmags_max=reform(mytab(26+2*nfils:26+2*nfils+nfils-1,*))
           data.zphots(l).modelmags=reform(mytab(26+3*nfils:26+3*nfils+nfils-1,*))
        endif
        if (zpeginfo.version ge 5.14) then begin
           data.zphots(l).itemplate=reform(mytab(6,*))
           data.zphots(l).age=reform(mytab(7,*))
           data.zphots(l).norm=reform(mytab(8,*))
           data.zphots(l).ebv=reform(mytab(9,*))
           data.zphots(l).dlum=reform(mytab(10,*))
           data.zphots(l).chi2sol=reform(mytab(11,*))
           data.zphots(l).specsfr=reform(mytab(12:14,*))
           data.zphots(l).sfr=reform(mytab(15:17,*))
           data.zphots(l).massstellwd=reform(mytab(18:20,*))
           data.zphots(l).intsfr=reform(mytab(21:23,*))
           data.zphots(l).agestars=reform(mytab(24:26,*))
           data.zphots(l).absmags=reform(mytab(27:27+nfils-1,*))
           data.zphots(l).absmags_min=reform(mytab(27+nfils:27+nfils+nfils-1,*))
           data.zphots(l).absmags_max=reform(mytab(27+2*nfils:27+2*nfils+nfils-1,*))
           data.zphots(l).modelmags=reform(mytab(27+3*nfils:27+3*nfils+nfils-1,*))
           data.zphots(l).modelmagerrs=reform(mytab(27+4*nfils:27+4*nfils+nfils-1,*))
        endif
     endfor

     if (keyword_set(verbose)) then print,'data reformated !'

     if not keyword_set(nosave) then begin
     	sav=file+'.sav'
     	save,filename=sav,data,zpeginfo
     endif

  endif else begin
     sav=file+'.sav'
     restore,filename=sav
  endelse

end
