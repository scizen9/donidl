pro GSC_Read,file,ls,Trgt_coord,silent=silent,Duplicates=ReadDup, $
  Verbose=Verbose,MaxStars=MAXSTARS,RArange=RArange,DECrange=DECrange
;+
; NAME:
;   GSC_READ
; DESCRIPTION:
;   This procedure reads a ST Guide Star format file and returns the stars in
;   an array of structures with fields ID,StrID,RA,DEC,MAG.
; CALLING SEQUENCE:
;   GSC_Read,FILE,LS,[Trgt_Coord],[/silent,/Duplicates,/Verbose,MaxStars=]
; INPUT:
;   FILE      The Name of the File to Load
; OUTPUT:
;   LS        The array of Stuctures containing the star information.  The
;               fields are given above.  Address the String ID of the sixth
;               star with ls(5).StrID
;             Note that ls(0).ID will equal -1 if the file could not be loaded.
; OPTIONAL OUTPUT:
;   TRGT_COORD  The [RA,DEC] of the Target as listed at the top of the GSC file.
; OPTIONAL FLAGS/PARAMETERS:
;   SILENT     If set, all messages including error messages are inhibited.
;                This keyword overrides the VERBOSE keyword.
;   DUPLICATES If set, duplicate stars will be KEPT.  The default is to keep
;                only the first of duplicate stars, which appear frequently
;                in the STGSC (when a star appears on more than one plate.)
;   VERBOSE    If set, informational messages (Targets Coords, Equinox, Stars
;                read, duplicates skipped) WILL be printed.
;   MAXSTARS   The stars information is initally loaded into a huge array and
;                then chopped down to the appropriate size when all stars are
;                loaded.  The default value is 2000.  Increase for huge files.
;   RArange    Allows the selection of Stars within this VECTOR [MIN,MAX] RA.
;   DECrange   Allows the selection of Stars within this VECTOR [MIN,MAX] DEC.
; HISTORY:
;   24-AUG-91 Updated, fixed, and made more user-friendly procedure STGSFREAD
;             (EWD) and renamed to GSC_Read   (Eric W. Deutsch)
;   29-JAN-92 Changed trgt_RA,trgt_DEC to one parameter Trgt_Coord    (EWD)
;   29-JAN-92 Added RArange and DECrange keywords and selection code  (EWD)
;-

  if (n_params(0) lt 2) then begin
    print,'Call> GSC_Read,file_name,Stars_struc,[Trgt_Coord],[/silent,/Duplicates],'
    print,'        [/Verbose,MaxStars=,RArange=[,],DECrange=[,]]
    print,"e.g.> GSC_Read,'M81.GSC',ls,Target,/Verbose"
    return
    endif
  if (n_elements(silent) eq 0) then silent=0
  if (n_elements(Verbose) eq 0) then Verbose=0
  if (n_elements(ReadDup) eq 0) then ReadDup=0
  if (n_elements(MAXSTARS) eq 0) then MAXSTARS=2000
  if (n_elements(RArange) lt 2) then RArange=[0.,400.]
  if (n_elements(DECrange) lt 2) then DECrange=[-90.,+90.]
  if silent then Verbose=0

  star=-2
  tmp=findfile(file,count=exist)
  if (exist gt 0) then begin
    openr,infil,file,/get_lun,error=err
    if (err ne 0) then begin
      if not silent then print,'[GSC_Read] ',strmessage(err),file & goto,OUT
      endif
  endif else begin
    if not silent then print,'File: ',file,' not Found.' & goto,OUT
    endelse

  lin=strarr(1) & star=-1
  idt0=1 & idt=1 & rah=1 & ram=1 & ras=1.0 & decd=1 & decm=1 & decs=1.0
  magt=1. & equin=1.0 & decsgn='=' & prev='' & nDup=0
  ls=replicate({GSC_star,ID:0,StrID:'',RA:0.0D,DEC:0.0D,MAG:0.0},maxstars)
  ls(0).ID=-1 & FileType=0

  while (not EOF(infil)) and (FileType eq 0) do begin
    readf,infil,lin
    if (strmid(lin(0),0,12) eq ' Object ID :') then FileType=1
    if (strmid(lin(0),0,11) eq ' GSC number') then FileType=2
    if (strmid(lin(0),0,5) eq ';----') then begin
      FileType=1 & goto,SKIPHDR
      endif
    endwhile
  if (FileType eq 0) then goto,OUT

  if (FileType eq 1) then begin
    while(equin ne 2000.0) do begin
      readf,infil,lin
      stringad,strmid(lin(0),14,12)+strmid(lin(0),32,13),trgt_RA,trgt_DEC
      Trgt_Coord=[trgt_RA,trgt_DEC]
      equin=float(strmid(lin(0),52,6))
      endwhile
    if Verbose then begin
      print,'File is in GSSS DataBase Format:'
      print,'Target CENTER: (RA,DEC)=',vect(Trgt_Coord),'   (degrees)'
      print,'EQUINOX:',equin
      endif
    while (not EOF(infil)) and (strmid(lin(0),0,13) ne '  Object I.D.') do $
      readf,infil,lin
    if (EOF(infil) ne 0) then goto,OUT
    format='(i9,i8,2i3,f7.3,10x,a1,i2,i3,f6.2,9x,f5.2)'
    endif

  if (FileType eq 2) then begin
    equin=float(strmid(lin(0),18,6))
    readf,infil,lin
    if Verbose then begin
      print,'File is in CD-ROM format:'
      print,'EQUINOX:',equin
      endif
    format='(i5,1x,i5,2x,2i3,f7.3,3x,a1,i2,i3,f6.2,3x,f5.2)'
    endif


SKIPHDR:
  star=0
  while not EOF(infil) do begin
      readf,infil,form=format,idt0,idt,rah,ram,ras,decsgn,decd,decm,decs,magt
    ID='GS-'+strn(idt0)+'-'+strn(idt)
    if (ID eq prev) and (ReadDup eq 0) then begin
      nDup=nDup+1 & goto,SKIPSTAR & endif
    RA=(rah+ram/60.+ras/3600.)/24.*360.
    DEC=decd+decm/60.+decs/3600.
    if (RA lt RArange(0)) or (RA gt RArange(1)) then goto,SKIPSTAR
    if (DEC lt DECrange(0)) or (DEC gt DECrange(1)) then goto,SKIPSTAR
    ls(star).strID=ID & prev=ID
    ls(star).ID=idt & ls(star).MAG=magt
    ls(star).RA=RA & ls(star).DEC=DEC
    if (decsgn eq '-') then ls(star).DEC=-ls(star).DEC
    star=star+1
SKIPSTAR:
    if (star eq maxstars) then goto,OUT
    endwhile

OUT:
  if (n_elements(infil) ne 0) then close,infil
  if (star eq -1) and not silent then print,'File is not in ST Guide Star format.  Unable to load.'
  if (star eq maxstars) and not silent then print,'More than ',maxstars,' stars in file.  Ignoring rest of list.'
  if Verbose and (star gt 0) then begin
    print,strn(star),' stars read from file: ',file
    if not ReadDup then print,strn(nDup),' duplicate star entries skipped.'
    endif
  ls=ls(0:(star-1)>1)
  if (star lt 1) then ls(0).ID=-1
  if (FileType eq 2) then $
    Trgt_Coord=[avg([min(ls.RA),max(ls.RA)]),avg([min(ls.DEC),max(ls.DEC)])]
  return
end
