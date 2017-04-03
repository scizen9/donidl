	function inst_throughput, wave, grat, ang=ang
	eff_bl = [0.1825,0.38,0.40,0.46,0.47,0.44,0.0,0.0,0.0,0.0,0.0]
	eff_bm = [0.1575, 0.33, 0.36, 0.42, 0.48, 0.45,0.0,0.0,0.0,0.0,0.0]
	eff_bh1 = [0., 0.0, 0.0, 0.0, 0.0, 0.,0.0,0.0,0.0,0.0,0.0]
	eff_bh2 = [0.,  0.18, 0.3, 0.4, 0.28, 0.,0.0,0.0,0.0,0.0,0.0]
	eff_bh3 = [0., 0., 0., 0.2, 0.29, 0.31,0.0,0.0,0.0,0.0,0.0]
        eff_medrez = [0.0, 0.2, 0.2, 0.2, 0.2, 0.2, 0.0, 0.0, 0.0, 0.0, 0.0]
        eff_blue = [0.0, 0.0, 0.0, 0.2, 0.2, 0.2, 0.0, 0.0, 0.0, 0.0, 0.0]
        eff_yell = [0.0, 0.0, 0.0, 0.0, 0.0, 0.2, 0.2, 0.2, 0.0, 0.0, 0.0]
        eff_red = [0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.2, 0.2, 0.2, 0.0]

	wave_0 = [355.,380.,405.,450.,486.,530., 600.0, 650.0, 700., 800.0, 900.]*10.

	wave_bl = [355., 530.]*10.
	wave_bm = [355., 530.]*10.
	wave_bh1 = [350., 450.]*10.
	wave_bh2 = [405., 486.]*10.
	wave_bh3 = [405., 530.]*10.
        wave_medrez = [380.0, 550.0]*10.
        wave_blue = [ 460.0, 550.0]*10.
        wave_yell = [540.0, 650.0]*10.0
        wave_red = [640., 770.]*10.0

	trans_atmtel = [0.54, 0.55, 0.56, 0.56, 0.56, 0.55, 0.55, 0.55, 0.55, 0.55, 0.55]

	case grat of 
	'BL': begin
	eff = eff_bl*trans_atmtel
	wave_range = wave_bl
	end
	'BM': begin
	eff = eff_bm*trans_atmtel
	wave_range = wave_bm
	end
	'BH1': begin
	eff = eff_bh1*trans_atmtel
	wave_range = wave_bh1
	end
	'BH2': begin
	eff = eff_bh2*trans_atmtel
	wave_range = wave_bh2
	end
	'BH3': begin
	eff = eff_bh3*trans_atmtel
	wave_range = wave_bh3
	end
        'MEDREZ': begin
           eff = eff_medrez*trans_atmtel
           wave_range=wave_medrez
        end
        'BLUE' : begin
           eff = eff_blue*trans_atmtel
           wave_range=wave_blue
        end 
        'YELLOW' : begin
           eff = eff_yell*trans_atmtel
           wave_range=wave_yell
        end 
        'RED' : begin
           eff = eff_red*trans_atmtel
           wave_range=wave_red
        end 
	else: begin
		print,'ERROR INVALID GRATING'
		return,0.*wave
	end
	endcase
	wave1 = wave
	eff_int = interpol(eff,wave_0,wave1)
	i = where(wave1 lt wave_range[0] or wave1 gt wave_range[1])
	if i[0] ge 0 then eff_int[i] = 0.
	return, eff_int
	end

;		355	380	405	450	486	530
;BL		0.1825	0.38	0.4	0.46	0.47	0.44
;BM		0.1575	0.33	0.36	0.42	0.48	0.45
;BH2			0.18	0.3	0.4	0.28	
;BH3				0.2	0.29	0.31
;ATM+TEL	0.54	0.55	0.56	0.56	0.56	0.55

