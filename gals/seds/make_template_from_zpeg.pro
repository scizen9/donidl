pro make_template_from_zpeg,file,template,nfils
;+
; NAME: make_template_from_zpeg
;
; PURPOSE: define a "template" structure, to be used by read_ascii
;
; CALLING SEQUENCE: make_template_from_zpeg,file,template,nfils
;
; INPUTS: 
;   - file (string): filename of a ZPEG output catalog
;
; OUTPUTS: 
;   - template: structure that can be used by read_ascii
;   - nfils: number of filters in the output catalog
;
; MODIFICATION HISTORY:
;  V1.0: initial
;-

openr,uin,file,/get_lun

;;get number of filters
found=0
a='#'
version=0.
while(not(found) and strmid(a,0,1) eq '#') do begin
    a=''
    readf,uin,a
    if (stregex(a,'number of filters[:=]',/boolean)) then begin
        found=1
        nfils=strtrim((stregex(a,'number of filters[:=](.*)',/extract,/subexpr))(1),2)
    endif
    if (stregex(a,'Version[:=]',/boolean)) then begin
        version=1.*strtrim((stregex(a,'Version[:=](.*)',/extract,/subexpr))(1),2)
    endif    
endwhile

close,uin
free_lun,uin

;;compute nfields: total number of fields
;print,'version=',version
if (version lt 4.) then begin
    nfperg=7+nfils ; z zmin zmax m itemplate age norm  + mags abs
;    nfperg=8+nfils ; z zmin zmax m itemplate age norm ebv + mags abs
    nfperg_befnorm=6
endif
if (version eq 4.0) then begin
    nfperg=10+nfils ; z zmin zmax m mmin mmax itemplate age norm ebv + mags abs
    nfperg_befnorm=8
endif
if (version ge 4.1 and version lt 4.3) then begin
    nfperg=16+nfils ; z zmin zmax m mmin mmax itemplate age norm ebv + mags abs, sfrspec(*3),sfr(*3)
    nfperg_befnorm=8
endif
if (version ge 4.3 and version le 5.02) then begin
    nfperg=22+nfils ; z zmin zmax m mmin mmax itemplate age norm ebv + mags abs, sfrspec(*3),sfr(*3),massstellwd(*3),intsfr(*3)
    nfperg_befnorm=8
endif
if (version ge 5.03 and version lt 5.1) then begin
    nfperg=25+nfils ; z zmin zmax m mmin mmax itemplate age norm ebv + mags abs, sfrspec(*3),sfr(*3),massstellwd(*3),intsfr(*3), agestars(*3)
    nfperg_befnorm=8
endif
if (version ge 5.1) then begin
    nfperg=26+4*nfils ; z zmin zmax m mmin mmax itemplate age norm ebv chi2 + mags abs, sfrspec(*3),sfr(*3),massstellwd(*3),intsfr(*3), agestars(*3) , absmags, absmags_inf, absmags_sup, modelmags
    nfperg_befnorm=8
endif
if (version ge 5.14) then begin
    nfperg=27+5*nfils ; z zmin zmax m mmin mmax itemplate age norm lumdist ebv chi2, sfrspec(*3),sfr(*3),massstellwd(*3),intsfr(*3), agestars(*3) , absmags, absmags_inf, absmags_sup, modelmags, modelmags_err
    nfperg_befnorm=8
endif
    
;print,'nfperg=',nfperg
i0=1+2+2+1+$
   5+1+$
   2*nfils+1
nfields=i0+5*(nfperg+1)

template={VERSION:1.0, DATASTART:0L, DELIMITER:32B, MISSINGVALUE:!values.(1), $
               COMMENTSYMBOL:'', FIELDCOUNT:nfields, FIELDTYPES:lonarr(nfields), $
               FIELDNAMES:strarr(nfields), FIELDLOCATIONS:lonarr(nfields), FIELDGROUPS:lonarr(nfields)}

;----------------------------------------------------------------------
; give field names

; first block
tab=['id','xpix','ypix','ra','dec','FIELD06','chi2','nbands','nsol','zmax','Vmax']

; observed photometry
tab=[tab,'OBSPHOT']
for i=0,nfils-1 do begin
    num=strtrim(string(format='(i4)',i),2)
    tab=[tab,'obs'+num,'obs_err'+num]
endfor

; each block
tab=[tab,'DEBUT']
for i=0,4 do begin
    num=strtrim(string(format='(i4)',i+1),2)
    sttab='FIELD'+strtrim(string(format='(i4)',1+indgen(nfperg)+i0+(nfperg+1)*i),2)
    tab=[tab,'sol'+num,sttab]
endfor
;tab=[tab,'LAST']


template.fieldnames=tab


;--------------------------------------------------
;define types
tabfields=[7,5,5,5,5,0,5,2,2,5,5,0,$
                     replicate(5,nfils*2)] ;; input photometry
;for i=0,4 do tabfields=[tabfields,0,replicate(5,nfperg)]
for i=0,4 do tabfields=[tabfields,0,replicate(4,nfperg_befnorm),$
                        5,$ ; Norm is DOUBLE
                        4,$ ; EBV is float
                        5,$ ; lumdist is DOUBLE
                        5,$ ; Chi2 is double
                        replicate(4,nfperg-nfperg_befnorm-4)] ; the rest is float
tabfields=[tabfields,0]


template.fieldtypes=tabfields

tab=[  0,1,2,3,4,5,6,7,8,9,10,11,$
                        replicate(12,nfils*2)]
for i=0,4 do begin
    ii0=(i0)+i*(nfperg+1)
    tab=[tab,ii0,replicate(ii0+1,nfperg)]
endfor

tab=[tab,nfields-1]
template.fieldgroups=tab


end
