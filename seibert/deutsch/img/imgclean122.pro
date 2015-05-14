pro IMGClean122,img,h,cr, SKY_VAL_SAMP_SZ=SKY_VAL_SAMP_SZ, FINE_CLEAN=FINE_CLEAN, $
  HI_SCAN_HEIGHT=HI_SCAN_HEIGHT, LO_SCAN_HEIGHT=LO_SCAN_HEIGHT, $
  STAR_PSF_SENS=STAR_PSF_SENS, SC_inner_width=SC_inner_width, $
  SC_outer_width=SC_outer_width, SC_upper_lim=SC_upper_lim, $
  PHASE1_ITER=PHASE1_ITER,HELP=extHelp
;+
; NAME:
;   IMGclean
; PURPOSE:
;     This procedure is designed to remove cosmic ray hits (CRs) from a image,
;   specifically WFPC images.  The procedure was originally written with one
;   image in mind and was made to work well with that image.  Its effectiveness
;   for other images varies.  Often, it does an excellent job, but sometimes
;   the operational parameters must be modified (via IDL2 kerwords) to tweak
;   the performance of the software.
;     Still in a somewhat primitive state, this procedure will be improved
;   beginning June 1992.  Suggestions welcome to SCIVAX::DEUTSCH.
; CALLING SEQEUNCE:
;   IMGclean,IMAGE,HEADER,[CR_ARRAY],[optional keywords]'
;   IMGclean,/help
; INPUT/OUTPUT PARAMETERS:
;   IMAGE     This parameter supplies the image in which the CRs are to be
;               removed.  This image is MODIFIED!  The output cleaned image
;               is returned via this variable.
;   HEADER    This parameter is the FITS header.  The header will be modified
;               with added HISTORY detailing the procedure.
; OPTIONAL OUTPUT:
;   CR_ARRAY  This array is a BYTE array of the same size as the input image
;               containing some information of which pixels the software
;               considered, and what descisions it made as far as CR or STAR.
; OPTIONAL INPUT:
;   keywords  The purpose of these ketwords is described by typing
;               IMGclean,/help
; HISTORY:
;   02-SEP-90 Version 1.0 written by Eric W. Deutsch (EWD)
;   27-JUN-91 Version 1.1 released with several minor modifications including
;               changing the operational parameters to keywords. (EWD)
;   27-JAN-92 Version 1.2 released with the additions of changing the
;               operational parameters to keywords and adding better on-line
;               help. (EWD)
;   07-MAY-92 Version 1.21 released: proper header added and /HELP keyword
;               added.  Added MOUSSE check.
;   10-MAY-92 Version 1.22: Fixed "Donutting" problem and changed default
;               STAR_PSF_SENS value to .50 instead of .66 since it was removing
;               too many actual stars.
;-

  COMMON STARCHCK122,SC_inner_width1,SC_outer_width1,SC_upper_lim1

  if (n_elements(extHelp) eq 1) then goto,PRNT_HELP
  if (n_params(0) lt 1) then begin
    print,'Call> IMGCLEAN,image_array,header,[returned_CR_array],[several keywords]'
    print,'e.g.> IMGCLEAN,img1,h1,cr1'
    print,"Type> IMGCLEAN,/HELP for description of keywords"
    return
    endif
  s=size(img)
  if (s(0) ne 2) then goto,PRNT_HELP

  if (n_elements(SKY_VAL_SAMP_SZ) eq 0) then SKY_VAL_SAMP_SZ=10.
  if (n_elements(HI_SCAN_HEIGHT) eq 0) then HI_SCAN_HEIGHT=6.
  if (n_elements(LO_SCAN_HEIGHT) eq 0) then LO_SCAN_HEIGHT=1.5
  if (n_elements(STAR_PSF_SENS) eq 0) then STAR_PSF_SENS=.50
  if (n_elements(SC_inner_width) eq 0) then SC_inner_width=8
  if (n_elements(SC_outer_width) eq 0) then SC_outer_width=20
  if (n_elements(SC_upper_lim) eq 0) then SC_upper_lim=4.5
  if (n_elements(FINE_CLEAN) eq 0) then FINE_CLEAN=1
  if (n_elements(PHASE1_ITER) eq 0) then PHASE1_ITER=2

  SC_inner_width1=SC_inner_width & SC_outer_width1=SC_outer_width
  SC_upper_lim1=SC_upper_lim

  print,'[IMGclean 1.22] MESSAGE: Released 10-MAY-1992.  Please report problems or'
  print,'  bugs to SCIVAX::DEUTSCH.  Suggestions for improvement are welcome.'
  print,'  Include example images (or portions thereof) if possible.'

  s=size(img)
  NAXIS1=s(1) & NAXIS2=s(2)
  cr=bytarr(NAXIS1,NAXIS2)

  flag=2
;  yboxes=100 & xboxes=100 & flag=0                    ; manual override
  while (flag ge 2) do begin
    xboxes=NAXIS1/SKY_VAL_SAMP_SZ & yboxes=NAXIS1/SKY_VAL_SAMP_SZ
    if (xboxes eq fix(xboxes)) and (yboxes eq fix(yboxes)) then flag=0
    if (flag ge 14) then flag=1
    if (flag ge 2) then begin
      if (SKY_VAL_SAMP_SZ eq 16) and ((flag and 4) eq 0) then begin
        SKY_VAL_SAMP_SZ=10. & flag=flag+4 & endif
      if (SKY_VAL_SAMP_SZ eq 10) and ((flag and 8) eq 0) then begin $
        SKY_VAL_SAMP_SZ=16. & flag=flag+8 & endif
      if (SKY_VAL_SAMP_SZ ne 10) and (SKY_VAL_SAMP_SZ ne 16) then $
        SKY_VAL_SAMP_SZ=10.
      endif
    endwhile
  if (flag eq 1) then begin
    print,'[IMGClean] WARNING: Axes are not multiples of either 10 or 16.'
    print,'                    Using boxsize of ',strn(fix(SKY_VAL_SAMP_SZ)), $
      '.  Edges of image may not be cleaned.'
    xboxes=NAXIS1/SKY_VAL_SAMP_SZ & yboxes=NAXIS1/SKY_VAL_SAMP_SZ
    endif

  xboxes=fix(xboxes) & yboxes=fix(yboxes)
  xboxsz=NAXIS1/xboxes & yboxsz=NAXIS2/yboxes
  hipix=0L & lopix=0L & phase=0

PH1ITER:
  print,'PHASE 1(Iteration ',strn(phase+1),'): Scanning for High Pixels (Stars and CRs)....'
  print,'                 High Scan height: ',strn(HI_SCAN_HEIGHT),'*RMS'
  print,'                 Low Scan height: ',strn(LO_SCAN_HEIGHT),'*RMS'

  for y=0,yboxes-1-phase do begin
    for x=0,xboxes-1-phase do begin
      tmp=1 & crtmp=1
      tmp=extrac(img,(x+phase/2.)*xboxsz,(y+phase/2.)*yboxsz,xboxsz,yboxsz)
      skyline,tmp,skyv,rms
      lo=where(tmp gt skyv+rms*LO_SCAN_HEIGHT)
      chk=size(lo)
      if (chk(0) ne 0) then begin
        crtmp=cr((x+phase/2.)*xboxsz:(x+phase/2.+1)*xboxsz-1, $
          (y+phase/2.)*yboxsz:(y+phase/2.+1)*yboxsz-1)
        crtmp(lo)=70
        cr((x+phase/2.)*xboxsz:(x+phase/2.+1)*xboxsz-1, $
          (y+phase/2.)*yboxsz:(y+phase/2.+1)*yboxsz-1)=crtmp
        lopix=lopix+n_elements(lo)
        endif
      high=where(tmp gt skyv+rms*HI_SCAN_HEIGHT)
      chk=size(high)
      if (chk(0) ne 0) then begin
        crtmp=cr((x+phase/2.)*xboxsz:(x+phase/2.+1)*xboxsz-1, $
          (y+phase/2.)*yboxsz:(y+phase/2.+1)*yboxsz-1)
        crtmp(high)=120
        cr((x+phase/2.)*xboxsz:(x+phase/2.+1)*xboxsz-1, $
          (y+phase/2.)*yboxsz:(y+phase/2.+1)*yboxsz-1)=crtmp
        hipix=hipix+n_elements(high)
        endif
      endfor
      if (y/5 eq y/5.) then print,strn(fix((y+1)*yboxsz*100./NAXIS2),length=3),'% complete:', $
        strn(hipix,length=5),' high pixels found',lopix,' low-level pixels found'
    endfor
  lopix=0L & hipix=0L
  if (PHASE1_ITER eq 2) and (phase eq 0) then begin
    phase=1 & goto,PH1ITER & endif

  no_crpix=0 & no_cr=0 & no_starpix=0 & no_star=0 & t2=0
  no_adj_CR=0 & no_adj_CR_pix=0

  print,'PHASE 2: Identifying High Pixels (Stars or CRs)....'
  for y=0,NAXIS2-1 do begin
    band=where(cr(*,y) eq 120)
    chk=size(band)
    if (chk(0) ne 0) then begin
      for i=0,n_elements(band)-1 do begin
        current=lonarr(20) & t1=0 & flag=0
        current(t1)=band(i) & t1=t1+1 & starti=i
        while (flag eq 0) do begin
          if (i eq n_elements(band)-1) or (t1 eq 19) then flag=2
          if (flag eq 0) then begin
            if (band(i) eq band(i+1)-1) then begin
              i=i+1 & current(t1)=band(i) & t1=t1+1
            endif else begin
              flag=1
              endelse
            endif
          endwhile
        if (flag ne 0) then begin
          cent=starti+t1/2
          starchck122,img,band(cent),y,skyv,rms,avg1,below
;          print,skyv,rms,avg1,(avg1(0)-avg1(1))/rms
          if (avg1(0)-rms*STAR_PSF_SENS gt avg1(1)) then begin
            is_a_CR=0
            no_starpix=no_starpix+t1 & no_star=no_star+1
          endif else begin
            is_a_CR=1
            no_crpix=no_crpix+t1 & no_cr=no_cr+1
            tmp=extrac(img,band(cent)-7,y-7,15,15)
            skyline,tmp,skyv,rms
            endelse
          seed=skyv
          for t2=starti,i do begin
            if (is_a_CR eq 1) then begin
              cr(band(t2),y)=255
              n_rnd=n_elements(band(t2)) & rnd=fltarr(n_rnd)
              for rndi=0,n_rnd-1 do rnd(rndi)=randomu(seed)*rms*1.5-rms*.5
              img(band(t2),y)=skyv+rnd
              endif
            endfor
          if (is_a_CR eq 1) and (band(starti) gt 0) and (band(i) lt NAXIS1-1) $
            and (y gt 0) and (y lt NAXIS2-1) then begin
            subcr=extrac(cr,band(starti)-1,y-1,band(i)-band(starti)+3,3)
            lowcr=where(subcr eq 70)
            chk=size(lowcr)
            if (chk(0) ne 0) then begin
              subimg=extrac(img,band(starti)-1,y-1,band(i)-band(starti)+3,3)
              n_rnd=n_elements(lowcr) & rnd=fltarr(n_rnd)
              for rndi=0,n_rnd-1 do rnd(rndi)=randomu(seed)*rms*1.5-rms*.5
              subimg(lowcr)=skyv+rnd
              subcr(lowcr)=200
              img(band(starti)-1:band(i)+1,y-1:y+1)=subimg
              cr(band(starti)-1:band(i)+1,y-1:y+1)=subcr
              no_adj_CR=no_adj_CR+1
              no_adj_CR_pix=no_adj_CR_pix+n_elements(lowcr)
              endif
            endif
          endif
        if (skyv gt 200) then print,'      ',skyv,rms,skyv
        endfor
      endif
    if (y/25. eq y/25) then print,strn(fix((y+1)*100./NAXIS2),length=2),'% complete', $
      ': (Ln-Obj,Pix): CRs=',vect([no_cr,no_crpix]), $
      ' AdjCRs=',vect([no_adj_CR,no_adj_CR_pix]), $
      ' Stars=',vect([no_star,no_starpix])
    endfor
    print,'*** Final:  ', $
      ': (Ln-Obj,Pix): CRs=',vect([no_cr,no_crpix]), $
      ' AdjCRs=',vect([no_adj_CR,no_adj_CR_pix]), $
      ' Stars=',vect([no_star,no_starpix])

  s=size(h)
  if (n_elements(s) lt 3) then return
  if (s(2) ne 7) then return

  on_error,2			; return if no Astronomy or MOUSSE library

  sxaddhist,'[IMGclean 1.22] '+!stime+': IC_ keywords updated',h
  sxaddpar,h,'IC_CRSRM',no_crpix,' Number of CR pixels removed'
  sxaddpar,h,'IC_ADJRM',no_adj_CR_pix,' Number of pixels adjacent to CRs removed'
  sxaddpar,h,'IC_STRPX',no_starpix,' Pixels part of a star (not removed)'
  sxaddpar,h,'IC_SMPSZ',SKY_VAL_SAMP_SZ,' Size of Box for local Sky Value'
  sxaddpar,h,'IC_HISCN',HI_SCAN_HEIGHT,' RMSs above sky for HI classif.'
  sxaddpar,h,'IC_LOSCN',LO_SCAN_HEIGHT,' RMSs above sky for LO classif.'
  sxaddpar,h,'IC_PSFSN',STAR_PSF_SENS,' Sensitivity of Star/CR Discrimination'
  sxaddpar,h,'IC_INRWD',SC_inner_width,' Inner Star Discrimination Anulus'
  sxaddpar,h,'IC_OTRWD',SC_outer_width,' Outer Star Discrimination Anulus'
  sxaddpar,h,'IC_UPRLM',SC_upper_lim,' Star Discrimination Upper Limit'
  sxaddpar,h,'IC_FINCL',FINE_CLEAN,' Adjacent Pixels removed? (1=Yes)'
  sxaddpar,h,'IC_PH1IT',PHASE1_ITER,' Iterations of Phase 1 (Find)'

  return

PRNT_HELP:

  print,'Call> IMGCLEAN,image_array,header,[returned_CR_array],[several keywords]
  print,'Keywords:
  print,'  SKY_VAL_SAMP_SZ   Size of the boxes used when calculating the local
  print,'                      background.  The X,Y image size must be a multiple of the
  print,'                      box size.  Use smaller boxes for steeper images.
  print,'                      Default values are 10 or 16.
  print,'  HI_SCAN_HEIGHT    Pixel value above the sky value in sky RMS units that is
  print,'                      flagged as a suspected CR.  Default=6.  Typical=5,6,7
  print,'  LO_SCAN_HEIGHT    Pixel value above the sky value in sky RMS units that is
  print,'                      removed if it is adjacent to a positively identified CR,
  print,'                      provided FINE_CLEAN=1.  Default=1.5.  Typical=2,2.5,3
  print,'  STAR_PSF_SENS     Sensitivity of the STAR checking (in sky value RMS units.)
  print,'                      The lower the value, the more CRs will be left in the
  print,'                      image because they are suspected as being stars.  The
  print,'                      higher the value, the more faint stars will be removed.
  print,'                      Default=.50  Typical=.4, .50, .6, 1.
  print,'  SC_inner_width    Width of the inner star checking box.  Best if it is the
  print,'                      average approximate size of the star and halo.
  print,'                      Default=8.  Typical=6,8,10,12,15.
  print,'  SC_outer_width    Width of the outer star checking box.  Default=20.
  print,'  SC_upper_lim      Pixel value above which is ignored by STARCHCK122 in sky
  print,'                      RMS units.  Default=4.5.  Typical=4,5,6
  print,'**** Press any key for more ****' & key=get_kbrd(1)
  print,'  FINE_CLEAN        Set (=1) this keyword (Flag) if pixels above LO_SCAN_HEIGHT
  print,'                      neighboring pixels identified as CRs are to be removed.
  print,'  PHASE1_ITER       Number of iterations for Phase 1.  Should always be 2
  print,'                      because 1 iteration tends to leave more CRs behind.
  return
end
