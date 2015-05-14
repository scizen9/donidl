pro IMGtext,xoff,y,str,fontname,clearunder=clearunder,spacing=spacing
;+
; NAME:
;   IMGtext
; PURPOSE:
;   This procedure writes text onto the current graphics window.  Its
;   purpose is to provide large, nice looking text with which to annotate
;   images suitable for publication, since the Hershey Vector fonts often
;   fall short of good publication quality at large sizes.  The drawback
;   is that, since they are bitmapped fonts, only certain fonts and sizes
;   are available.  However, by using the FONTCNVT utility, more can be
;   added.
;   One column of space is automatically provided on both sides of each
;   character.  Additional space can be add with the spacing keyword.
; CALLING SEQEUNCE:
;   IMGtext,xoffset,ybaseline,prtstring,fontname,[clearunder=]
; INPUT:
;   XOFFSET   This is the X position where the first character is to begin.
;               The actual character starts at X+1, but the space at the
;               edge of the character begins at X.  This value is changed
;               to reflect the new position of the 'cursor' as characters
;               are written.
;   YBASELINE This is the Y position of the baseline of the font.  Note that
;               some characters of some fonts drop below the baseline.
;   PRTSTRING This is the string of characters to write (e.g. 'Hello')
;   FONTNAME  This is the string of the root filename of the font
;               (e.g. 'bold24')
; OUTPUT:
;   XOFFSET   The returned new position of the cursor.
; OPTIONAL KEYWORDS:
;   CLEARUNDER  This keyword governs how the letter if written to the window
;                  0  Text is written to the screen leaving the background
;                  1  The space under the text is blanked out
;                  2  One pixel all around each letter is blanked out
;                       (default value)
;   SPACING     This keyword specifies how many pixels of spacing between
;                 letter is provided.  The default=2.
; EXAMPLE:
;   x=100
;   IMGtext,x,200,'Scale=30''''','bold16'
; HISTORY:
;   03-NOV-1992  Version 1 Written  (E. Deutsch)
;-

  if (n_params(0) lt 3) then begin
    print,'Call> IMGtext,xoffset,ybaseline,prtstring,fontname,[clearunder=]'
    print,'e.g.> IMGtext,200,100,''Hello World'',''arial16'''
    return
    endif

  if (n_params(0) eq 3) then fontname='arial16'

  if (n_elements(clearunder) eq 0) then clearunder=2
  if (n_elements(spacing) eq 0) then spacing=1

  path=getenv('IDL_DIR')+'/deutsch/img/fonts'
  GetDefVal,path,'IMGFONTS'

  if not exist(path+fontname+'.fnh') then begin
    print,'Unable to load font ',fontname
    return
    endif

  IMGread,img,h,path+fontname+'.fnh',/silent
  height=n_elements(img(0,*))
  Nomhght=sxpar(h,'NOMHGHT')
  Drop=sxpar(h,'MAXDROP')

  for chars=0,strlen(str)-1 do begin
    char=fix(byte(strmid(str,chars,1)))
    offset=sxpar(h,'CHROF'+strn(char))
    width=sxpar(h,'CHRWD'+strn(char))

    if (width eq 0) then begin
      print,'Character "',strmid(str,chars,1),'" not found!'
      goto,SKIP1
      endif

    letter=bytarr(width+4,height+2)
    letter(2:width+1,1:height)=img(offset:offset+width-1,*)*255

    if (clearunder ne 1) then begin
      bkg=tvrd(xoff,y-Drop,width+4,height+2)
      tmp=letter
      if (clearunder eq 2) then tmp=smooth(letter,3)
      if (clearunder eq 3) then begin
        tmp=tmp*0b & tmp(*,Drop-1:Drop+NomHght+3<height+1)=1
        endif
      letter(where(tmp eq 0))=bkg(where(tmp eq 0))
      endif

    tv,letter,xoff,y-Drop
    xoff=xoff+width+2

    if (spacing gt 0) then begin
      if (clearunder eq 1) then tv,img(0:spacing-1,*),xoff,y-Drop
      xoff=xoff+spacing
      endif

SKIP1:
    endfor

  return
end
