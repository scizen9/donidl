; ***************************************************************************
; ***   This procedure is designed to allow you to edit the variable inputs of
; *** the IMGMRG procedure more easily.  The the IMGMRG program itself for a
; *** more detailed description of the variables.
; ***************************************************************************

	pro combine,dummy

	LoCen=976		; Height of OBJECT center where the
				;   OBJECT is closer to 0
	HiCen=1621		; Height of OBJECT center where the
				;   OBJECT is closer 2048
	CUT=597 		; Height of separation line
				;   ( 0>M>T ) OR ( B>M>2048)
	LoBkg=0 		; Background to subtract from the image
				;   with the OBJECT closer to 0
	HiBkg=1050		; Background to subtract from the image
				;   with the OBJECT closer to 2048
	Lfile='sa68-1l.hhh'	; File which corresponds to LoCen
	Hfile='sa68-1h.hhh'	; File which corresponds to HiCen
	OUTfile='sa68-1c.hhh'	; combined image output file

	imgmrg,Lfile,Hfile,OUTfile,LoCen,HiCen,CUT,LoBkg,HiBkg

	return
	end
