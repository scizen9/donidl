function wstdev, vec, errs
;
return,sqrt( 1. / total(1./errs^2) )
end
