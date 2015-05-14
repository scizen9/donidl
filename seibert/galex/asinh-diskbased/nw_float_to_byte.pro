;+
;NAME:
;  nw_float_to_byte
;PURPOSE:
;  Converts floats of an array to bytes
;INPUTS:
;  image       - image array
;OPTIONAL INPUTS:
;  none
;KEYWORDS:
;  none
;OUTPUTS:
;  The float-value image
;REVISION HISTORY:
;  10/03/03 written - wherry
;-
pro nw_float_to_byte,colors
 restore,colors
 image = byte((floor(colors * 256.0) > 0) < 255)
 ;byte_image = byte((floor(colors * 256.0) > 0) < 255)
;RETURN, byte_image

save,filename='tempimage.sav',image
return

END
