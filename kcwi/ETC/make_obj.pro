	pro make_obj,flux,grat_wave,f_lam_index, w, p_A
        
	w = findgen(6000)*1.+3000.
	p_A = flux/(2.d-8/w)*(w/grat_wave)^f_lam_index
	return
	end

