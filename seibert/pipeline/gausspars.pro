;*******************************************************************************
pro gausspars, gcoef, gcoef_err, struct = struct
;*******************************************************************************

center = gcoef[1]
center_err = gcoef_err[1]

fwhm = gcoef[2] * 2 * sqrt(2 * alog(2))
fwhm_err = gcoef_err[2] * 2 * sqrt(2 * alog(2))

ampl = gcoef[0]
ampl_err = gcoef_err[0]

flux = sqrt(2 * !PI) * gcoef[0]* gcoef[2]
flux_err = sqrt(2 * !PI * (gcoef[2]^2 * gcoef_err[0]^2 + $
                           gcoef[0]^2 * gcoef_err[2]^2))

struct = {center: center, center_err: center_err, fwhm: fwhm, $
          fwhm_err: fwhm_err, flux: flux, flux_err: flux_err}

end


