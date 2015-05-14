; *****************************************************************************
; ******* PROCEDURE: IMGMRG
; *****************************************************************************
; *** DESCRIPTION:
; ***     This procedure will combine two images (mainly designed for GSSS use)
; ***       that individually do not produce a nice centered image.  In other
; ***       words, if as object is near a plate edge, the other plate where the
; ***       object is near the plate edge can also be extracted and the two can
; ***       be combined so that the object will appear in the center if the
; ***       new image and the edge defects can be eliminated.
; *** INPUT:
; ***    Lfile	  The file name (with Header extender) in which the center of
; ***               the object is less than 1024 (Low.) (from ODlist)
; ***    Hfile    The file name (with Header extender) in which the center of
; ***               the object is greater that 12975 (High.) (from ODlist)
; ***    OUTfile  The file name (with Header extender) to which the combined
; ***               image should be written
; ***    LoCen    The height of the Center line of the object (from ODlist) in
; ***               the Lfile.
; ***    HiCen    The height of the Center (corresponding) line of the object
; ***               in the Hfile. [2048-(14000-ODlist#)]
; ***    LoBkg    Background to be subtracted from the Lfile image.
; ***    HiBkg    Background to be subtracted from the Hfile image.
; *** OUTPUT:
; ***    The output file (both .HHD and .HHH)
; ***    all passed variables remain unchanged
; *** NOTES:
; ***    LoCen and HiCen must correspond to the same line in each image for
; ***      the two images to fit properly.  They can be found as calculated
; ***      above from the ODlist, or just picked arbitrarily by examining both
; ***      images and choosing a line.
; ***    The two backround variables are mainly intended to make the two
; ***      backgrounds roughly similar.  These variables were not intended to
; ***      completely subtract out the backgrounds.
; *** HISTORY:
; ***    19-JUN-90 Version 1 written
; *********** IDL 2.0 ************ Eric W. Deutsch ********* 19-JUN-90 ********

	pro imgmrg,Lfile,Hfile,OUTfile,LoCen,HiCen,CUT,LoBkg,HiBkg

	arg=n_params(0)
	if (arg lt 1) then begin
		print,'Call: IDL> IMGMRG,Lfile,Hfile,OUTfile,LoCen,HiCen,CUT,LoBkg,HiBkg'
		print,"e.g.: IDL> IMGMRG,'n74.hhh','n74top.hhh',n74comb.hhh',710,1498,520,700,0
		goto,BRK
		endif

	image=intarr(2048,2048)

	file=strmid(Hfile,0,strlen(Hfile)-1)+'d'
	print,'Loading ',file,'...'
	sxhread,Hfile,h1
	PLATEID1=sxpar(h1,'PLATEID')
	openr,1,file
	tmp=assoc(1,intarr(2048,1))
	if (CUT le LoCen) then begin
		for i=0,1023-LoCen+CUT do image(*,i)=tmp(HiCen-1023+i)-HiBkg
		endif
	if (CUT ge HiCen) then begin
		for i=0,1023+(CUT-HiCen) do image(*,i)=tmp(HiCen-1023+i)-HiBkg
		CRPIX2=sxpar(h1,'CRPIX2')
		h=h1
		sxaddpar,h,'CRPIX2',CRPIX2+(B-1024),' Adjusted to equivalent'
		endif
	close,1

	file=strmid(Lfile,0,strlen(Lfile)-1)+'d'
	print,'Loading ',file,'...'
	sxhread,Lfile,h1
	PLATEID2=sxpar(h1,'PLATEID')
	openr,1,file
	tmp=assoc(1,intarr(2048,1))
	if (CUT le LoCen) then begin
		for i=0,(LoCen-CUT)+1023 do image(*,1024-LoCen+CUT+i)= $
			tmp(CUT+i)-LoBkg
		CRPIX2=sxpar(h1,'CRPIX2')
		h=h1
		sxaddpar,h,'CRPIX2',LoCen-1024,' Adjusted to equivalent'
		PLATEID=strtrim(PLATEID2)+'+'+strtrim(PLATEID1)+'         '
		endif
	if (CUT ge HiCen) then begin
		for i=0,1023-(CUT-HiCen) do image(*,1024+(CUT-HiCen)+i)= $
			tmp(LoCen+(CUT-HiCen)+i)-LoBkg
		PLATEID=strtrim(PLATEID2)+'+'+strtrim(PLATEID1)+'         '
		endif
	close,1

	file=strmid(OUTfile,0,strlen(OUTfile)-1)+'d'
	print,'Saving ',file,'...'
	openw,1,file,4096,/fixed,/none
	tmp=assoc(1,intarr(2048,2048))
	tmp(0)=image
	close,1

	sxaddpar,h,'PLATEID',PLATEID,' First ID is Main portion of image'
	sxaddhist,"Image combined from the two above Plate ID's: "+systime(0),h
	sxhwrite,OUTfile,h

BRK:	return
	end
