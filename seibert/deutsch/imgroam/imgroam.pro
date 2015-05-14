pro imgroam,image,hdrdummy,img1
;+
; NAME:
;   IMGROAM
; PURPOSE:
;   IMGroam is a program which is designed to simplify routine
;   examination of images.  IMGroam provides facilities to easily move around
;   an image, zooming, and displaying cursor coordinates both X,Y and RA,DEC
;   if a plate solution is provided.  The stretch can be interacticely adjusted,
;   facilities for creating and tranfering plate solutions are available.
; CALLING SEQEUNCE:
;   IMGroam,image,header,[frame]
; INPUT:
;   IMAGE     The 2D image array to be examined.
;   HEADER    The corresponding string array GEIS header.  In fact, this
;               parameter does not NEED to be supplied, and if it is not, then
;               one can just examine the array passed with IMAGE.  If the
;               array is an astronomical image, it is usually wise to supply
;               the header.  But, for example, if one reads in a .GIF file,
;               or other 2D data array, the array may be examined with
;               IMGroam without supplying any header.
; OPTIONAL OUTPUT:
;   FRAME     This parameter can return the currently displayed frame when
;               IMGroam is exited.
; EXAMPLE:
;   >IMGread,img,h                  ; Read in an image (Pickfile available)
;   >IMGroam,img,h                  ; Examine the image
;      or
;   >tmp=indgen(512,512)            ; Create some array
;   >IMGroam,tmp                    ; Use IMGroam to examine array
; HISTORY:
;   25-MAY-90 Original Alpha Test Version 1.0 'completed' by Eric W. Deutsch
;     (countless undocumented additions, revisions, etc.)
;   02-DEC-92 DIST_COORD_CONV support added for UIT images.  EWD
;   06-DEC-92 Version 3.1 completed.  EWD
;   03-APR-93 Version 3.2 released.  EWD
;-

  arg=n_params(0)
  if (arg lt 1) then begin
    print,'Call: IDL> IMGROAM,image_array,[GEIS_header],[Returned_frame]'
    print,'e.g.: IDL> IMGROAM,img1,h,frm' & return & endif

  if (!d.name ne 'X') then begin & print,'You must be running Xwindows to use IMGRoam.' & return & endif
  if ((!d.flags and 65536) eq 0) then begin & print,'IMGRoam now uses the IDL Widget interface.  EXIT and type $WIDL at DCL prompt.' & return & endif

  s=size(image)
  if (s(0) ne 2) then begin & print,'First parameter must be 2D image array.' & return & endif

  !QUIET=1
  defansi

  if (arg lt 2) then hdrdummy=1
  s=size(hdrdummy) & if (s(2) ne 7) then mkhdr,hdrdummy,image

  COMMON fparm,NAXIS1,NAXIS2,xsize,ysize,xcent,ycent,xll,yll,zoom,frtyp
  COMMON fparm2,xso,yso,xmin,ymin
  COMMON ANSI,CR,LF,esc,UP,CLRSCRN,BELL,DOWN,RIGHT,LEFT,NORMAL,BOLD,REVERSE,BLINKING
  COMMON IR_ENVIR,stat,itype
  COMMON cmpwin,COMPWIN,COMPZOOM,COMPIMG,COMPIMG1
  COMMON IR_ASTROM,astrom_type,hdr,astr,gsa
  COMMON frpc,scmin,scmax,rdtyp,satlim
  COMMON Windows,FrameWIN,CmpressWIN
  COMMON Widgets,w

  hdr=hdrdummy                          ; put header in COMMON
  NAXIS1=sxpar(hdr,'NAXIS1')
  NAXIS2=sxpar(hdr,'NAXIS2')
  BITPIX=sxpar(hdr,'BITPIX') & if (BITPIX eq 0) then BITPIX=16
  OBJECT=sxpar(hdr,'OBJECT') & if (!ERR lt 0) then OBJECT='none'

  stat={ASTR:0, XMEN:1, AUTOD:1, AUTOB:0, WINS:0, NUMSF:0, $
    CBAUTOD:1, CBAUTOB:1, ZTYPE:1, CBBCOL:255, NEARR:1, $
    CBBTHK:1, BCOL:255, BTHK:1, AUTOPROC:0, CBLLX:6, CBLLY:5, CBX:500, CBY:10}

  xll=0 & yll=0 & zoom=1.
  xso=512 & yso=xso

  if (NAXIS1 lt 512) and (NAXIS2 lt 512) then zoom=fix(512/NAXIS1)>fix(512/NAXIS2)+1.
  xsize=yso & ysize=yso
  xcent=NAXIS1/2 & ycent=NAXIS2/2

  frtyp='NONE' & COMPWIN=0
  s=size(image) & itype=s(3)

  print,cr,cr,cr,'Automatic Frame Display: ON'

; === Determine what sort of astrometry, if any, is present in the header ================
  astrom_type='NONE'					; reset default to NONE

  if (strn(sxpar(hdr,'CTYPE1')) eq 'PIXEL') then delast,hdr

  extast,hdr,astr,success_flag				; try to extract astrom from hdr
  if (success_flag gt 0) then begin
    if (strn(astr.ctype(0)) ne 'PIXELS') then begin
      astrom_type='STD' & stat.ASTR=1			; type to STD and enable disp.
      print,' Standard Astrometry found in header'
      endif
    endif

  if (astrom_type eq 'NONE') then begin			; if not regular astrom, try GSSS
    tmp=sxpar(hdr,'AMDX1')				; test for GSSS-type astrometry
    if (!ERR ge 0) then begin
      astrom_type='GSSS' & stat.ASTR=1			; type to GSSS and enable disp.
      gsssextast,hdr,gsa				; extract astrometric info
      print,' GSSS Astrometry Found in header'
      endif
    endif

  if (stat.ASTR eq 1) then print,'Automatic Astrometry Display: ON'

; === Determine what stretch to start out with ===========================================
  scmin=0 & scmax=1000 & rdtyp='NONE' & satlim=0

  tmp=sxpar(hdr,'IR_SCMIN') & FrameWIN=0
  if (!ERR ge 0) then begin
    print,' IMGRoam Frame Processing keywords found in header
    print,'Automatic Frame Processing: ON'
    scmin=sxpar(hdr,'IR_SCMIN') & scmax=sxpar(hdr,'IR_SCMAX')
    rdtyp=strn(sxpar(hdr,'IR_RDTYP')) & stat.AUTOPROC=1
    satlim=sxpar(hdr,'IR_SATLM')
  endif else IR_Roam,999,mx,my,313,image

  win_init,1 & win_alloc,FrameWIN
  IR_GetWinPos,'FrameWIN',x,y
  win_open,FrameWIN,512,512,x,y,'IMGroam Version 3.3   Frame Window'
  irdisp,image,img1,3
  print,cr,'Compressed Window Display: ON'
  COMPWIN=3 & CmpressWIN=-1 & pwin=-1 & awin=-1 & cmpwinsho,image

  IR_Widgets,w,'SETUP'
  IR_Widgets,w,'IR_SETUP' & Refresh=0
  IR_Widgets2,w,'DS_SETUP'

  IR_flag=0 & AperDump=0

  IR_Disp,OBJECT,image(xcent,ycent)
  IR_Widgets2,w,'ButtonLab',Ret_Val,'SaveDef'
  IR_Widgets2,w,'ButtonLab',Ret_Val,'OffWindow'

  while (IR_flag eq 0) do begin
LOOPq:
    Ret_Val=1 & win_mseread,awin,mx,my,button,Wid_Chk=Ret_Val,KeyHit=KeyHit


; ***************************************************** Frame Window stuff ****
    if (awin eq FrameWIN) or (awin eq CmpressWIN) then begin
      if (pwin ne FrameWIN) and (pwin ne CmpressWIN) then begin
        IR_Widgets2,w,'ButtonLab',Ret_Val,'OnWindow'
        pwin=FrameWIN
        endif
      IR_Roam,awin,mx,my,button,image,img1

; ********************** Keyboard Input *************************************
      if (KeyHit eq '') then KeyHit=get_kbrd(0)
      if (KeyHit ne '') then begin
        IR_KbdHndlr,image,img1,mx,my,KeyHit,hdr

        while (KeyHit ne '') do KeyHit=get_kbrd(0)	; Flush buffer
        endif
; ***************************************************************************

      if (button eq 1) or (button eq 4) then Refresh=1
      if (button eq 2) and (awin eq FrameWIN) and (mx ne -1) then begin
        if (mx lt 0) or (my lt 0) or (mx ge NAXIS1) or (my ge NAXIS2) then $
          DN=0. else DN=image(mx,my)
        IR_astdisp,mx,my,ra,dec,DN
        IR_Widgets2,w,'ButtonLab',Ret_Val,'Left','Done'
        IR_Widgets2,w,'ButtonLab',Ret_Val,'Middle','Not Defined'
        IR_Widgets2,w,'ButtonLab',Ret_Val,'Right','Menu'
        x=mx & y=my & wait,.6 & cursor,mx,my,1,/device & pwin=-1
        if (!ERR eq 4) then begin
          ir_widgets2,w,'PostCentroid',Ret_val,image,x,y
          endif
        wait,1
        endif
      endif
; **************************************************** Option Window stuff ****
    IR_Widgets,w,'IR_CHECK',Ret_Val
    if (Ret_Val eq -1) then goto,M0
    if (Ret_Val eq 7) or (Ret_Val eq 107) then begin
      IR_flag=Ret_Val & goto,M0 & endif
    option=Ret_Val
; ********************** DEFROI PROCEDURE **********************************
    if (option eq 0) then begin
      print,'Define new HII Region'
      win_set,FrameWIN
      reslt=DefROI(512,512,xverts,yverts,/noregion)
      xverts=xverts*1./zoom+xmin-0.5
      yverts=yverts*1./zoom+ymin-0.5
      if (n_elements(ROInum) eq 0) then ROInum=0
      fname='ROI'+strn(ROInum,length=3,padchar='0')+'.dat'
      openw,funit,fname,/GET_LUN
      printf,funit,'; Region of Interest (ROI) Dump File'
      printf,funit,'; Polygon Information:
      tmp=polyfillv(xverts+1.0,yverts+1.0,NAXIS1,NAXIS2)
      printf,funit,'NVERTS  = ',strn(n_elements(xverts))
      printf,funit,'NPIXELS = ',strn(n_elements(tmp))
      printf,funit,'COUNTS  = ',strn(total(image(tmp)))
      print,'COUNTS,NPIXELS = ',total(image(tmp)),n_elements(tmp)
      printf,funit,'; List of Vertices follows:'
      for qi=0,n_elements(xverts)-1 do printf,funit,xverts(qi),yverts(qi)
      close,funit & free_lun,funit & ROInum=ROInum+1
      print,'  '+fname+' written..' & wait,1
      endif
; ********************** COLOR TABLE PROCEDURES ****************************
    if (option eq 1) then begin
      IR_Widgets2,w,'ColorTables'
      endif
; ********************** ASTROMETRY PROCEDURES ********************************
    if (option eq 2) then begin
      GS_MAIN,ss,image,img1,hdr
      IR_Widgets,w,'IR_CLEAR'
      win_set,FrameWIN & Refresh=1
      endif
; ********************** PROGRAM SETTINGS *************************************
    if (option eq 3) then begin
      IR_Widgets,w,'IR_Settings',Ret_Val,stat
      IR_Widgets,w,'IR_CLEAR'
      win_set,FrameWIN & Refresh=1
      if (Ret_Val eq 1) and (stat.AUTOPROC eq 1) then begin
        irdisp,image,img1,3
        COMPWIN=2 & cmpwinsho,image
        endif
      endif
; ****************** RUN ANNOTATION PROGRAM  *********************************
    if (option eq 4) then begin
      wset,0
      ann
      endif

; ****************** Button Functions ****************************************
    if (option gt 9) and (option lt 16) then begin
      IR_Roam,awin,xcent,ycent,200+option,image,img1
      tmp=[10,11,12,14]
      if (min(abs(tmp-option)) eq 0) then Refresh=1
      endif


M0: if (Refresh eq 1) then begin
      IR_Disp,OBJECT,image(xcent,ycent)
      print,'Move Mouse pointer to a Window or Menu',cr,cr
      pwin=-1 & Refresh=0 & endif
    endwhile

; ********************************************************* End of Program ****
  IR_Widgets,w,'IR_DESTROY'
  print,cr,cr,cr,cr,cr
  hdrdummy=hdr
  wset,0
  if (IR_flag eq 107) then begin
    win_dele,FrameWIN
    win_dele,CmpressWIN
    endif
  return

end
