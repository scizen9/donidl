function wmean, vec, errs
;
return,total(vec/errs^2) / total(1./errs^2)
end
