pro lcur
lcur_init
;
;
q = ''	; input action
lcur_prt_help
repeat begin
    q = get_kbrd(1)	; get action letter
    case q of
	'?':	lcur_prt_help

	'%':	lcur_dao_img

	'+':	lcur_dao_next
	'=':	lcur_dao_next

	'-':	lcur_dao_next,/prev
	'_':	lcur_dao_next,/prev

	'*':	lcur_plt_phot

;	'a':	lcur_dao_add
	'a':	lcur_dao_finder

	'b':	lcur_set_bin

	'c':	lcur_plt_phase

	'C':	begin
		done=(1 eq 0)
		print,'User cursor to select stars, lower left corner to quit.'
		while not done do lcur_cursor,done=done
		print,'Exit cursor mode.'
		end

    	'd':	lcur_data_get

	'e':	lcur_set_merr

	'f':	lcur_set_fran

	'g':	lcur_dao_get

	'i':	lcur_prt_pars

	'j':	lcur_plt_phase,/fit

	'k':	lcur_set_sub

	'l':	lcur_set_per

;	'm':	lcur_plt_mags
	'm':	lcur_plt_phot

	'n':	lcur_phase_prt

	'o':	begin
		reply = ''
		read,'Next id: ',reply
		lcur_dao_next,long(reply)
		end

	'p':	lcur_plt_phase,/mags

	'r':	lcur_set_pran

	's':	lcur_scar_run

	't':	lcur_set_twin

	'v':	lcur_set_varth

	'w':	lcur_scar_plt

	'x':	lcur_set_rnei

	'z':	lcur_dao_zoom

	'>':	lcur_inc_per,/up

	'<':	lcur_inc_per,/down

	'.':	lcur_inc_per,/larger

	',':	lcur_inc_per,/smaller

	'2':	lcur_inc_per,/double

	'1':	lcur_inc_per,/half

    else:
    endcase
endrep until q eq 'q'

return
end	; pro lcur
