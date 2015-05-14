;
; Auto Save File For ./discalc.pro
;
;  Mon Nov  7 19:38:54 PST 1994
;


;+
; No formal header yet.  See the documentation in
;   http//www.astro.washington.edu/deutsch/apoinfo.html
;-


; CODE MODIFICATIONS MADE ABOVE THIS COMMENT WILL BE LOST.
; DO NOT REMOVE THIS COMMENT: BEGIN HEADER


; $Id: lib_template.pro,v 1.2 1994/04/29 16:50:28 dan Exp $
;
; Copyright (c) 1993, Research Systems, Inc.  All rights reserved.
;	Unauthorized reproduction prohibited.
; (Of course, if you don't work for RSI, remove these lines or
;  modify to suit.)

;
; NAME:
;	ROUTINE_NAME
;
; PURPOSE:
;	Tell what your routine does here.  I like to start with the words:
;	"This function (or procedure) ..."
;	Try to use the active, present tense.
;
; CATEGORY:
;	Put a category (or categories) here.  For example:
;	Widgets.
;
; CALLING SEQUENCE:
;	Write the calling sequence here. Include only positional parameters
;	(i.e., NO KEYWORDS). For procedures, use the form:
;
;	ROUTINE_NAME, Parameter1, Parameter2, Foobar
;
;	Note that the routine name is ALL CAPS and arguments have Initial
;	Caps.  For functions, use the form:
; 
;	Result = FUNCTION_NAME(Parameter1, Parameter2, Foobar)
;
;	Always use the "Result = " part to begin. This makes it super-obvious
;	to the user that this routine is a function!
;
; INPUTS:
;	Parm1:	Describe the positional input parameters here. Note again
;		that positional parameters are shown with Initial Caps.
;
; OPTIONAL INPUTS:
;	Parm2:	Describe optional inputs here. If you don't have any, just
;		delete this section.
;	
; KEYWORD PARAMETERS:
;	KEY1:	Document keyword parameters like this. Note that the keyword
;		is shown in ALL CAPS!
;
;	KEY2:	Yet another keyword. Try to use the active, present tense
;		when describing your keywords.  For example, if this keyword
;		is just a set or unset flag, say something like:
;		"Set this keyword to use foobar subfloatation. The default
;		 is foobar superfloatation."
;
; OUTPUTS:
;	Describe any outputs here.  For example, "This function returns the
;	foobar superflimpt version of the input array."  This is where you
;	should also document the return value for functions.
;
; OPTIONAL OUTPUTS:
;	Describe optional outputs here.  If the routine doesn't have any, 
;	just delete this section.
;
; COMMON BLOCKS:
;	BLOCK1:	Describe any common blocks here. If there are no COMMON
;		blocks, just delete this entry.
;
; SIDE EFFECTS:
;	Describe "side effects" here.  There aren't any?  Well, just delete
;	this entry.
;
; RESTRICTIONS:
;	Describe any "restrictions" here.  Delete this section if there are
;	no important restrictions.
;
; PROCEDURE:
;	You can describe the foobar superfloatation method being used here.
;	You might not need this section for your routine.
;
; EXAMPLE:
;	Please provide a simple example here. An example from the PICKFILE
;	documentation is shown below.
;
;	Create a PICKFILE widget that lets users select only files with 
;	the extensions 'pro' and 'dat'.  Use the 'Select File to Read' title 
;	and store the name of the selected file in the variable F.  Enter:
;
;		F = PICKFILE(/READ, FILTER = ['pro', 'dat'])
;
; MODIFICATION HISTORY:
; 	Written by:	Your name here, Date.
;	July, 1994	Any additional mods get described here.  Remember to
;			change the stuff above if you add a new keyword or
;			something!
;



; DO NOT REMOVE THIS COMMENT: END HEADER
; CODE MODIFICATIONS MADE BELOW THIS COMMENT WILL BE LOST.


; CODE MODIFICATIONS MADE ABOVE THIS COMMENT WILL BE LOST.
; DO NOT REMOVE THIS COMMENT: BEGIN MAIN13




PRO MAIN13_Event, Event


  WIDGET_CONTROL,Event.Id,GET_UVALUE=Ev

  CASE Ev OF 

  'FIELD6': BEGIN
      Print, 'Event for X:'
      END
  'FIELD7': BEGIN
      Print, 'Event for Y:'
      END
  'FIELD16': BEGIN
      Print, 'Event for X:'
      END
  'FIELD17': BEGIN
      Print, 'Event for Y:'
      END
  'BGROUP41': BEGIN
      CASE Event.Value OF
      0: Print,'Button Red Chip Pressed'
      1: Print,'Button Blue Chip Pressed'
      ELSE: Message,'Unknown button pressed'
      ENDCASE
      END
  'FIELD42': BEGIN
      Print, 'Event for Pixelsize'
      END
  'FIELD32': BEGIN
      Print, 'Event for RA:'
      END
  'FIELD33': BEGIN
      Print, 'Event for DEC:'
      END
  'BUTTON39': BEGIN
      Print, 'Event for       DONE       '
      widget_control,event.top,/destroy
      END
  ENDCASE
END


; DO NOT REMOVE THIS COMMENT: END MAIN13
; CODE MODIFICATIONS MADE BELOW THIS COMMENT WILL BE LOST.


pro dodiscalc,dummy

  COMMON DISCALC_COMM,starpos,imsize,filt,pixsize,slitpos,offset,FIELD32,FIELD33

  slitpos=(slitpos-starpos)*pixsize*[1,-1]
  widget_control,FIELD32,set_value=slitpos(0)
  widget_control,FIELD33,set_value=slitpos(1)

  return

end

;-------------------------------------------------------------------------------


PRO discalc,img,x,y, GROUP=Group


  IF N_ELEMENTS(Group) EQ 0 THEN GROUP=0

  junk   = { CW_PDMENU_S, flags:0, name:'' }

  COMMON DISCALC_COMM,starpos,imsize,filt,pixsize,slitpos,offset,FIELD32,FIELD33

  starpos=[x,y] & imsize=(size(img))([1,2]) & filt='x' & pixsize=0.0
  slitpos=[180,116.34] & offset=[0,0]
  if (imsize(0) eq 681) or (imsize(0) eq 870) then begin
    filt='R' & pixsize=0.6098 & endif
  if (imsize(0) eq 374) or (imsize(0) eq 586) then begin
    filt='B' & pixsize=1.088 & endif
  if (filt eq 'x') then print,'Unable to determine imagetype'
  openr,1,'/host/dione/u5/deutsch/apo/dis/dis_nomslitpos.txt'
  lin=''
  while (lin ne filt) and not EOF(1) do readf,1,lin
  if not EOF(1) then begin
    readf,1,lin & slitpos(0)=float(lin) & print,slitpos(0)
    readf,1,lin & slitpos(1)=float(lin) & print,slitpos(1)
    endif
  close,1

  MAIN13 = WIDGET_BASE(GROUP_LEADER=Group, $
      ROW=5, $
      MAP=1, $
      TITLE='DIS Offset Calculator', $
      UVALUE='MAIN13')

  BASE3 = WIDGET_BASE(MAIN13, $
      ROW=1, $
      MAP=1, $
      UVALUE='BASE3')

  LABEL4 = WIDGET_LABEL( BASE3, $
      UVALUE='LABEL4', $
      VALUE='Object Coords:')

  FieldVal396 = [ strn(starpos(0)) ]
  FIELD6 = CW_FIELD( BASE3,VALUE=FieldVal396, $
      ROW=1, $
      FLOAT=1, $
      RETURN_EVENTS=1, $
      TITLE='X:', $
      UVALUE='FIELD6')

  FieldVal461 = [ strn(starpos(1)) ]
  FIELD7 = CW_FIELD( BASE3,VALUE=FieldVal461, $
      ROW=1, $
      FLOAT=1, $
      RETURN_EVENTS=1, $
      TITLE='Y:', $
      UVALUE='FIELD7')


  BASE18 = WIDGET_BASE(MAIN13, $
      ROW=1, $
      MAP=1, $
      UVALUE='BASE18')

  LABEL15 = WIDGET_LABEL( BASE18, $
      UVALUE='LABEL15', $
      VALUE='Desired Slit Position')

  FieldVal591 = [ strn(slitpos(0)) ]
  FIELD16 = CW_FIELD( BASE18,VALUE=FieldVal591, $
      ROW=1, $
      FLOAT=1, $
      RETURN_EVENTS=1, $
      TITLE='X:', $
      UVALUE='FIELD16')

  FieldVal593 = [ strn(slitpos(1)) ]
  FIELD17 = CW_FIELD( BASE18,VALUE=FieldVal593, $
      ROW=1, $
      STRING=1, $
      RETURN_EVENTS=1, $
      TITLE='Y:', $
      UVALUE='FIELD17')


  BASE43 = WIDGET_BASE(MAIN13, $
      ROW=1, $
      MAP=1, $
      UVALUE='BASE43')

  Btns1908 = [ $
    'Red Chip', $
    'Blue Chip' ]
  BGROUP41 = CW_BGROUP( BASE43, Btns1908, $
      COLUMN=2, $
      EXCLUSIVE=1, $
      UVALUE='BGROUP41')

  FieldVal1910 = [ strn(pixsize) ]
  FIELD42 = CW_FIELD( BASE43,VALUE=FieldVal1910, $
      ROW=1, $
      FLOAT=1, $
      RETURN_EVENTS=1, $
      TITLE='Pixelsize', $
      UVALUE='FIELD42')


  BASE34 = WIDGET_BASE(MAIN13, $
      ROW=1, $
      MAP=1, $
      UVALUE='BASE34')

  LABEL31 = WIDGET_LABEL( BASE34, $
      UVALUE='LABEL31', $
      VALUE='Offset:')

  FieldVal1384 = [ $
    '88.8' ]
  FIELD32 = CW_FIELD( BASE34,VALUE=FieldVal1384, $
      ROW=1, $
      FLOAT=1, $
      TITLE='RA:', $
      UVALUE='FIELD32')

  FieldVal1386 = [ $
    '99.9' ]
  FIELD33 = CW_FIELD( BASE34,VALUE=FieldVal1386, $
      ROW=1, $
      STRING=1, $
      TITLE='DEC:', $
      UVALUE='FIELD33')


  BASE38 = WIDGET_BASE(MAIN13, $
      ROW=1, $
      MAP=1, $
      UVALUE='BASE38')

  BUTTON39 = WIDGET_BUTTON( BASE38, $
      UVALUE='BUTTON39', $
      VALUE='      DONE       ')


  WIDGET_CONTROL, MAIN13, /REALIZE

  if (filt eq 'R') then WIDGET_CONTROL, BGROUP41,set_value=0
  if (filt eq 'B') then WIDGET_CONTROL, BGROUP41,set_value=1
  dodiscalc

  XMANAGER, 'MAIN13', MAIN13
END
