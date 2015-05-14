pro user_input,prompt,default,value
;
str = prompt + ' <cr> - (' + strn(default) + ') : '
ival = ' '
read,ival,prompt=str
if ival eq '' then $
	value = default $
else	value = getopt(ival)
;
return
end
	
