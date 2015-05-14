pro lcur_set_sub
;
;

print,'CURRENT:'
print,'Do Count Sub : ',strn( lcur_com_get('do_sub') )
q = ''
read, 'Do Subtraction? (y/n): ',q
if q eq 'y' then $
	ret = lcur_com_put('do_sub', (1 eq 1) ) $
else	ret = lcur_com_put('do_sub', (1 eq 0) )
;
return
end
