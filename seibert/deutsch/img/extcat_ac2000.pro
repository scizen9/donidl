pro extcat_ac2000,outfile,ra,dec,boxwidth
;+
; NAME:
;	EXTCAT_AC2000
;
; PURPOSE:
;	Extract a region out of the AC 2000 CD-ROM.
;
; CATEGORY:
;	Catalog software
;
; CALLING SEQUENCE:
;	extcat_ac2000,outfile,ra,dec,boxwidth
;
; INPUTS:
;	outfile	  Filename of output file
;	ra	  Central RA in degrees of box
;	dec	  Central DEC in degrees of box
;	boxwidth  Width of extraction box in degrees 
;
; OUTPUTS:
;	The file
;
; PROCEDURE:
;
; EXAMPLE:
;
; MODIFICATION HISTORY:
;	01/27/98 Written by E. Deutsch
;
;-


; -- Not enough parameters?  Show the call sequence -------------------
  if (n_params(0) lt 4) then begin
    print,'Call> extcat_ac2000,outfile,ra,dec,boxwidth'
    print,"e.g.> stringad,'18 16  1.439  -14 02 11.92',ra,dec"
    print,"e.g.> extcat_ac2000,'NPSer.ac2000',ra,dec,1.0"
    return
    endif


  if (not exist('/hsfs/data/ac_n00.dat')) then begin
    print,'ERROR: Data files not found.  Is the CD-ROM mounted?'
    print,'  mount with:  cdmount -m /hsfs'
    print,'  dismount with:  cdumount -m /hsfs'
    return
    endif


  if (ra lt 0) or (ra ge 360) or (dec lt -90) or (dec gt 90) $
    or (boxwidth le 0) or (boxwidth gt 10.0) then begin
    print,'ERROR: RA, DEC, or boxwidth is out of bounds.  bummer.'
    return
    endif

  dec1=dec-boxwidth/2.0 & dec2=dec+boxwidth/2.0
  ra1=ra-boxwidth/2.0 & ra2=ra+boxwidth/2.0

  zone1=fix(dec1/10)*10
  signstr1='p'
  if (dec1 lt 0) then begin
    signstr1='n' & zone1=abs(zone1)
    endif
  if (zone1 gt 60) then zone1=60

  zone2=fix(dec2/10)*10
  signstr2='p'
  if (dec2 lt 0) then begin
    signstr2='n' & zone2=abs(zone2)
    endif
  if (zone2 gt 60) then zone2=60


  lin=''
  zonefile='/hsfs/data/ac_'+signstr1+strn(zone1)+'.dat'
  if (not exist(zonefile)) then begin
    print,'ERROR: Data file '+zonefile+' not found.  Bummer.'
    return
    endif
  openr,1,zonefile
  fptr=long(ra1/360d*20000000d)
  fstep=1000000L
  point_lun,1,fptr
  readf,1,lin

  rah=0 & ram=0 & ras=0.0 & decpm='' & decd=0 & decm=0 & decs=0.0
  nsel=0L & nsrch=0L
  flag=0 & lastdir=1
  print,'Searching '+zonefile+'....'
  while (flag eq 0) do begin
    readf,1,lin
    reads,lin,rah,ram,ras,decpm,decd,decm,decs,format='(i2,i3,f7,a1,i2,i3,f6)'
    ra9=(rah+ram/60d +ras/3600d)*15
    dec9=decd+decm/60d +decs/3600d
    if (decpm eq '-') then dec9=-dec9
    if (ra9 lt ra1) and (fstep gt 10000L) then begin
      if (lastdir eq -1) then fstep=fstep/2L
;      print,strn(fptr),' + ',strn(fstep)+' = '+strn(fptr+fstep)
      fptr=fptr+fstep
      point_lun,1,fptr
      readf,1,lin
      lastdir=1
      endif $
    else begin
      if (lastdir eq 1) then fstep=fstep/2L
;      print,strn(fptr),' - ',strn(fstep)+' = '+strn(fptr+fstep)
      fptr=fptr-fstep
      point_lun,1,fptr
      readf,1,lin
      lastdir=-1
      endelse
    if (ra9 lt ra1) and (fstep le 10000L) then begin
      openw,2,outfile
      while (flag eq 0) do begin
        readf,1,lin & nsrch=nsrch+1L
        reads,lin,rah,ram,ras,decpm,decd,decm,decs, $
          format='(i2,i3,f8,a1,i2,i3,f6)'
        ra9=(rah+ram/60d +ras/3600d)*15
        dec9=decd+decm/60d +decs/3600d
        if (decpm eq '-') then dec9=-dec9
        if (ra9 gt ra2) then flag=1
        if (ra9 ge ra1) and (ra9 le ra2) and (dec9 ge dec1) and $
          (dec9 le dec2) then begin
          print,strmid(lin,0,75)
          printf,2,lin & nsel=nsel+1L
          endif
       endwhile
     endif
    endwhile

  print,'Searched through '+strn(nsrch)+' objects'
  print,'Selected '+strn(nsel)+' objects'

  close,1,2
  return

end



