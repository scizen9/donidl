pro lcur_dao_img
;
COMMON dao_data		; dao_cid, ...
COMMON dao_imdata	; dao_im, dao_imhdr, dao_imgno
;
if dao_imgno lt 0 then $
	dao_imgno = 1 $
else	dao_imgno = -1
;
lcur_dao_next,dao_cid
;
return
end	; pro lcur_dao_img
