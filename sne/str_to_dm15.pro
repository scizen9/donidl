function str_to_dm15, str
;
return,1.00 - 1.63 * (str-1.) + 2.03*(str-1.)^2 - 1.82*(str-1.)^3
end
