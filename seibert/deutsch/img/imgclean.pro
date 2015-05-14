pro IMGClean,img,h,cr,SKY_VAL_SAMP_SZ=SKY_VAL_SAMP_SZ, FINE_CLEAN=FINE_CLEAN, $
  HI_SCAN_HEIGHT=HI_SCAN_HEIGHT, LO_SCAN_HEIGHT=LO_SCAN_HEIGHT, $
  STAR_PSF_SENS=STAR_PSF_SENS, $
  PHASE1_ITER=PHASE1_ITER,HELP=extHelp,test=testrun,qphot=qphot,noclean=noclean
;+
; NAME:
;   IMGclean
; PURPOSE:
;     This procedure is designed to remove cosmic ray hits (CRs) from an image,
;   especially WFPC images.  The procedure's effectiveness varies from image
;   to image.  Often, it does an excellent job, but sometimes
;   the operational parameters must be modified (via keywords) to tweak
;   the performance of the software.
;     It should not be assumed that this procedure will automatically do a good
;   job!  Keep a close eye on before and after images.  Occaisionally, a faint
;   star gets obliterated, especially if there is a cosmic ray around 3 pixels
;   from it.  Naturally, this procedure is only recommended when a CR split is
;   not possible, as that will often yield better results.
;     Run IMGclean twice on the same image to acheive best cleaning results
;   especially on images with very dense CR's.
;     Still in a somewhat primitive state, this procedure still need some
;   improvement to the algorithm.  Suggestions welcome to deutsch@stsci.edu.
;     Use the CLEANEXAMINE main program to examine the results of the IMGclean
;   run.
;     Cuyrrently, IMGclean is tweaked for PC images.  I haven't worked with
;   WF images in a while, so you may to diddle with the parameters for best
;   effect.
; CALLING SEQEUNCE:
;   IMGclean,IMAGE,HEADER,[CR_ARRAY],[optional keywords]'
;   IMGclean,/help
; INPUT/OUTPUT PARAMETERS:
;   IMAGE     This parameter supplies the image in which the CRs are to be
;               removed.  This image is MODIFIED!  The output cleaned image
;               is returned via this variable.
;   HEADER    This parameter is the FITS header.  The header will be modified
;               with added HISTORY notes and keywords detailing the settings
;               used on the last IMGclean run.
; OPTIONAL OUTPUT:
;   CR_ARRAY  This array is a BYTE array of the same size as the input image
;               containing some information of which pixels the software
;               considered, and what descisions it made as far as CR or STAR.
;                   DN    Meaning
;                   70    Low scan pixel not removed
;                  120    Hi scan pixel not removed (i.e. suspected star)
;                  255    Hi scan pixel removed (i.e. cosmic ray)
;                  200    Neighboring Low scan pixel removed
; OPTIONAL INPUT:
;   HELP             Displays quick keyword defaults values
;   TESTRUN          If set, dumps a file with stats on each object considered
;   STAR_PSF_SENS    Sensitivity of the STAR checking.
;                     The higher the value, the more CRs will be left in the
;                     image because they are suspected as being stars.  The
;                     lower the value, the more faint stars will be removed.
;                     Default=.35  Typical=.35, .25, .30, .40.
;                      -Value: .35 is probably the optimum value for doing a
;                       good job with CR's but leaving virtually faint
;                       scrungy stars for PC frames.
;                      -Value: .25 is probably the optimum value for doing
;                       the best possible job of cleaning CR's at the
;                       expense of removing a few more faint scrungy stars
;                       than is proper.  If faint, scrungy stars are not
;                       important, this may be the best setting (again, PC).
;   SKY_VAL_SAMP_SZ  Size of the boxes used when calculating the local
;                     background.  The X,Y image size must be a multiple of the
;                     box size.  Use smaller boxes for steeper images.
;                     Default values are 10 or 16.
;   HI_SCAN_HEIGHT   Pixel value above the sky value in sky RMS units that is
;                     flagged as a suspected CR.  Default=6.  Typical=5,6,7
;   LO_SCAN_HEIGHT   Pixel value above the sky value in sky RMS units that is
;                     removed if it is adjacent to a positively identified CR,
;                     provided FINE_CLEAN=1.  Default=1.5.  Typical=2,2.5,3
;   FINE_CLEAN       Set (=1) this keyword (Flag) if pixels above LO_SCAN_HEIGHT
;                     neighboring pixels identified as CRs are to be removed.
;   PHASE1_ITER      Number of iterations for Phase 1.  Should always be 2
;                     because 1 iteration tends to leave more CRs behind.
;   QPHOT            If set, quick photometry is run on stars using qphot.pro
;   NOCLEAN          If set, all objects are assumed to be stars and nothing
;                     is checked or removed.  Used in conjunction with QPHOT
;                     on an already cleaned image.
; HISTORY:
;   02-SEP-90 Version 1.0 written by Eric W. Deutsch (EWD)
;   27-JUN-91 Version 1.1 released with several minor modifications including
;               changing the operational parameters to keywords. (EWD)
;   27-JAN-92 Version 1.2 released with the additions of changing the
;               operational parameters to keywords and adding better on-line
;               help. (EWD)
;   07-MAY-92 Version 1.21 released: proper header added and /HELP keyword
;               added.  EWD.
;   10-MAY-92 Version 1.22: Fixed "Donutting" problem and changed default
;               STAR_PSF_SENS value to .50 instead of .66 since it was removing
;               too many actual stars.  EWD.
;   24-JUL-92 Version 1.3: Better Star/CR determination algorithm added.
;               This algorithm is less Kludgy, but sometimes gives poorer
;               results than the v1.22 release.  Needs Work!  EWD.
;   08-DEC-92 Version 1.31: Tidied things up a bit, but nothing major.  EWD.
;   09-DEC-92 Version 1.32: Fixed a rather major bug in the focus pixel
;               identifier and fixed the logic for negative ratios.  This
;               now shows a major improvement!  The major task now is to add
;               a special identifier for streak CR hits.  EWD
;   04-JAN-93 Version 1.40: Changed the default sensitivity to 0.35 which
;               make IMGclean more lenient as far as stars go, but added
;               another simple-minded neighboring pixels check which catches
;               A LOT of CR's.  Basically, I say that if more than 2 of the
;               8 neighboring pixels aren't above the low scan hight, then
;               it MUST be a cosmic ray.  This now shows another big im-
;               provement in number of CR's removed as well as star removed.
;               This also does a better job at removing long streaks, but
;               an algorithm specifically for longs streaks is needful and
;               will be implemented soon.  Header updated.  Eric W. Deutsch
;   05-JAN-93 Version 1.41: Little Faster.  Fixed FITS header updating.  EWD
;   06-JAN-93   Added a few more comments and text to program header.  EWD
;   28-FEB-93 Version 1.42: Added /QPHOT and /NOCLEAN options and code so that
;               IMGclean can do some rudimentary quick aperture photometry. EWD
;-

  if (n_elements(extHelp) eq 1) then goto,PRNT_HELP
  if (n_params(0) lt 1) then begin
    print,'Call> IMGCLEAN,image_array,header,[returned_CR_array],[several keywords]'
    print,'e.g.> IMGCLEAN,img1,h1,cr1'
    print,"Type> IMGCLEAN,/HELP for parameter listing/default values"
    return
    endif
  s=size(img)
  if (s(0) ne 2) then goto,PRNT_HELP

  if (n_elements(SKY_VAL_SAMP_SZ) eq 0) then SKY_VAL_SAMP_SZ=10.
  if (n_elements(HI_SCAN_HEIGHT) eq 0) then HI_SCAN_HEIGHT=6.
  if (n_elements(LO_SCAN_HEIGHT) eq 0) then LO_SCAN_HEIGHT=1.5
  if (n_elements(STAR_PSF_SENS) eq 0) then STAR_PSF_SENS=.35
  if (n_elements(FINE_CLEAN) eq 0) then FINE_CLEAN=1
  if (n_elements(PHASE1_ITER) eq 0) then PHASE1_ITER=2
  if (n_elements(testrun) eq 0) then testrun=0
  if (n_elements(qphot) eq 0) then qphot=0
  if (n_elements(noclean) eq 0) then noclean=0

  if (testrun eq 1) then openw,5,'imgclean.dmp'
  if (qphot ge 1) then begin
    openw,8,'qphot.dmp'
    qphot,img,-999,-999
    endif

  ICversion='1.41'
  print,'[IMGclean '+ICversion+'] MESSAGE: Released 05-JAN-1993.  Please report problems or'
  print,'  bugs to deutsch@stsci.edu.  Suggestions for improvement are welcome.'
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
          starchck,img,band(cent),y,ratio,radius,skyv,rms,test=testrun, $
            crim=cr,sens=STAR_PSF_SENS,qphot=qphot,noclean=noclean

          if (ratio lt STAR_PSF_SENS) then begin
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
        if (skyv gt 200) then print,'   Large Sky!:      ',skyv,rms
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

  if (testrun eq 1) then close,5
  if (qphot eq 1) then close,8

  s=size(h)
  if (n_elements(s) lt 3) then return
  if (s(2) ne 7) then return

  on_error,2			; return if no SX routines available

  tmp=sxpar(h,'IC_CRSRM') & iter='_'
  if (!ERR ge 0) then begin
    i=2 & next=9
    while (i lt 8) do begin
      tmp=sxpar(h,'IC'+strn(i)+'CRSRM')
      if (!ERR lt 0) then begin & next=i & i=9 & endif
      i=i+1
      endwhile
    iter=strn(next)
    endif

  sxaddhist,'[IMGclean '+ICversion+'] '+!stime+': IC'+iter+' keywords added',h
  sxaddpar,h,'IC'+iter+'CRSRM',no_crpix,' Number of CR pixels removed'
  sxaddpar,h,'IC'+iter+'ADJRM',no_adj_CR_pix,' Number of pixels adjacent to CRs removed'
  sxaddpar,h,'IC'+iter+'STRPX',no_starpix,' Pixels part of a star (not removed)'
  sxaddpar,h,'IC'+iter+'SMPSZ',SKY_VAL_SAMP_SZ,' Size of Box for local Sky Value'
  sxaddpar,h,'IC'+iter+'HISCN',HI_SCAN_HEIGHT,' RMSs above sky for HI classif.'
  sxaddpar,h,'IC'+iter+'LOSCN',LO_SCAN_HEIGHT,' RMSs above sky for LO classif.'
  sxaddpar,h,'IC'+iter+'PSFSN',STAR_PSF_SENS,' Sensitivity of Star/CR Discrimination'
  sxaddpar,h,'IC'+iter+'FINCL',FINE_CLEAN,' Adjacent Pixels removed? (1=Yes)'
  sxaddpar,h,'IC'+iter+'PH1IT',PHASE1_ITER,' Iterations of Phase 1 (Find)'

  return

PRNT_HELP:

  print,'Call> IMGCLEAN,image_array,header,[returned_CR_array],[optional keywords]
  print,'Keywords:
  print,'  SKY_VAL_SAMP_SZ   Default: 10.0 pixels squared'
  print,'  HI_SCAN_HEIGHT    Default: 6.0 RMS units above local sky'
  print,'  LO_SCAN_HEIGHT    Default: 1.5 RMS units above local sky'
  print,'  STAR_PSF_SENS     Default: 0.35 height to volume ratio'
  print,'  FINE_CLEAN        Default: 1 (TRUE) (Remove highish adjacent pixels'
  print,'  PHASE1_ITER       Default: 2 iterations (each iteration slightly different).'
  return
end
