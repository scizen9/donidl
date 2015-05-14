pro perfunc, x, a, f, pder

f  = a(0) + a(1) * cos( a(2) * x + a(3) ) + a(4) * sin( a(5) * x + a(6) )

if n_params() ge 4 then $
	pder = [[ replicate( 1.0, n_elements(x) ) ], $		; df/da(0)
		[ cos( a(2) * x + a(3) ) ], $			; df/da(1)
		[ -a(1) * x * sin( a(2) * x + a(3) ) ], $	; df/da(2)
		[ -a(1) * sin( a(2) * x + a(3) ) ], $		; df/da(3)
		[ sin( a(5) * x + a(6) ) ], $			; df/da(4)
		[  a(4) * x * cos( a(5) * x + a(6) ) ], $	; df/da(5)
		[  a(4) * cos( a(5) * x + a(6) ) ]]		; df/da(6)

end
