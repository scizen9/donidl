pro dishdrfix,searchspec
;+
; No formal header yet.  See the documentation in
;   http//www.astro.washington.edu/deutsch/apoinfo.html
;-

  if (n_params(0) ne 1) then begin
    print,"Call> dishdrfix,search_spec"
    print,"e.g.> dishdrfix,'*.hhh'"
    return
    endif


  files=findfile(searchspec)
  nfiles=n_elements(files)
  objname=''
  imtype=''
  filtstat=''
  print,' ' & print,' '

  
  for i=0, nfiles-1 do begin
  
    print,'******* ',files(i),' ***********'
    imgread,img,h,files(i)
  
    tmp=''
    read,'Enter Object name: ('+objname+') ',tmp
    if (tmp ne '') then objname=tmp
    sxaddpar,h,'OBJECT',strn(objname),' Name of Target'
  
    tmp=''
    read,'Enter Image type: ('+imtype+') ',tmp
    if (tmp ne '') then imtype=tmp
    sxaddpar,h,'IMAGETYP',strn(imtype),' Image Type(zero,flat,dark,object)'
  
    if (1 eq 1) then begin
      mask=sxpar(h,'MASK')
      if (mask eq 1) or (mask eq 4) then filtstat='OUT'
      if (mask eq 3) then filtstat='B'
      if (mask eq 4) then filtstat='R'
      if (mask eq 5) then filtstat='I'
      endif

    tmp=''
    read,'Are filters IN or OUT: ('+filtstat+') ',tmp
    if (tmp ne '') then filtstat=tmp
    if (strupcase(filtstat) eq 'IN') then filtstat='IN'
    if (strupcase(filtstat) eq 'OUT') then filtstat='OUT'
    filtstat=strupcase(filtstat)

    filter=strmid(files(i),strlen(files(i))-5,1)
    descrip='?'
    if (filter eq 'b') and (filtstat eq 'IN') then begin
      filter='g' & descrip=' Gunn g filter' & endif
    if (filter eq 'r') and (filtstat eq 'IN') then begin
      filter='r' & descrip=' Gunn r filter' & endif

    if (filter eq 'b') and (filtstat eq 'OUT') then begin
      filter='S' & descrip=' Unfiltered blue (Short wl) chip' & endif
    if (filter eq 'r') and (filtstat eq 'OUT') then begin
      filter='L' & descrip=' Unfiltered red (Long wl) chip' & endif

    if (filter eq 'b') and (filtstat eq 'I') then begin
      filter='X' & descrip=' No light' & endif
    if (filter eq 'r') and (filtstat eq 'I') then begin
      filter='I' & descrip=' Harris I' & endif

    if (filter eq 'b') and (filtstat eq 'B') then begin
      filter='B' & descrip=' Harris B' & endif
    if (filter eq 'r') and (filtstat eq 'B') then begin
      filter='X' & descrip=' No Light' & endif

    if (filter eq 'b') and (filtstat eq 'R') then begin
      filter='X' & descrip=' No light' & endif
    if (filter eq 'r') and (filtstat eq 'R') then begin
      filter='R' & descrip=' Harris R' & endif


    sxaddpar,h,'FILTERS',filter,descrip
  
    print,'  Added to header:'
    print,'    OBJECT   = '+strn(objname)
    print,'    IMAGETYP = ',strn(imtype)
    print,'    FILTERS  = ',filter,'       /',descrip

    if (strn(sxpar(h,'TURRET')) eq 'GRATINGS1') then begin
      sxaddpar,h,'DISPAXIS',2,' Dispersion Axis'
      print,'    DISPAXIS = ',2,'        /',descrip
      endif


    sxhwrite,files(i),h

    print,' '
    print,' '
  
    endfor

end
