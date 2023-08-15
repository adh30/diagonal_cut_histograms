// Equivalent Stata function for dhist
capture program drop dhist
program define dhist, rclass
    version 15.1
    syntax varname(a numeric) [nbins(int 1) rx(numeric numeric) eps(numeric 0.15) xlab(string "x") plot(bool) lab_spikes(bool)]
    marksample touse
    qui count if `touse'
    if r(N) < 1 {
        di "No valid observations for the variable."
        exit
    }

    qui su `varname' if `touse', detail
    local N = r(N)
    local Q1 = r(p25)
    local Q3 = r(p75)
    local IQR = `Q3' - `Q1'

    if `a' == 0 {
        local a = (`r(max)' - `r(min)') / 100000000
    }

    if `a' != 0 & `a' != . {
        local h = (`rx'[2] + `a' - `rx'[1]) / `nbins'
        local ybr = `rx'[1] + `h' * (0..`nbins')
        qui sum `varname' if `touse', meanonly
        local n = r(sum)
        local yupper = `varname' + (`a' * (_n / `n'))
        local ylower = `yupper' - `a' / `n'

        local cmtx = J(`N', 4, 1)
        qui forvalues i = 1 / `N' {
            local binno = floor((`yupper'[`i'] - `ybr'[1]) / `h') + 1
            if `binno' < 1 local binno = 1
            if `binno' > `nbins' local binno = `nbins'
            local theta = ((`yupper'[`i'] - `ybr'[`binno']) * `n') / `a'
            matrix C = J(1, 4, 0)
            matrix C[1, 1] = `binno'
            matrix C[1, 2] = `binno'
            matrix C[1, 3] = `binno' - 1
            matrix C[1, 4] = `binno' - 1
            matrix C[1, `theta' == .] = 1
            matrix C[1, `i' == `N'] = `nbins'
            matrix rownames C = `i'
            matrow `i' = C
        }
        matrix list `e(C)'
        matrix checksum = mod(rowsum(`e(C)'), 4)
        matrix straddlers = rownums(checksum :== 2)
        qui tab `e(C)' if _n <= `N', matcell(counts)
        if rows(`straddlers') > 0 {
            matrix counts = counts - 1
            qui forvalues i = 1 / `N' {
                if `checksum'[`i'] == 2 {
                    local binno = `e(C)'[`i', 1']
                    local theta = ((`yupper'[`i'] - `ybr'[`binno']) * `n') / `a'
                    matrix counts[`binno' - 1] = counts[`binno' - 1] + (1 - `theta')
                    matrix counts[`binno'] = counts[`binno'] + `theta'
                }
            }
        }
        local xbr = `ybr'
        qui forvalues i = 2 / `nbins' {
            local xbr[`i'] = `ybr'[`i'] - (`a' * sum(counts[1..`i'-1])) / `n'
        }
        local spike = `eps' * (`rx'[2] - `rx'[1]) / `nbins'
        local flagvec = (`xbr'[2..] - `xbr'[1..`nbins']) < `spike'
        if sum(abs(`xbr'[2..] - `xbr'[1..`nbins'])) <= `spike' {
            local xbr_new = `xbr'
            matrix counts_new = counts
            local amt_spike = `xbr'[`nbins'] - `xbr'[1']
            qui forvalues i = `nbins' - 1 / 2 {
                local diff_xbr = abs(`xbr'[`i'+1] - `xbr'[`i'])
                if `diff_xbr' <= `spike' & !missing(`diff_xbr') {
                    local prev_diff_xbr = abs(`xbr'[`i'] - `xbr'[`i'-1'])
                    local prev_diff_xbr_present = !missing(`prev_diff_xbr')
                    if `prev_diff_xbr_present' {
                        local amt_spike = `amt_spike' + `diff_xbr'
                        matrix counts_new[`i'] = counts_new[`i'] + counts_new[`i'+1]
                        local xbr_new[`i'+1] = .  // set to missing
                        matrix counts_new[`i'+1] = . // set to missing
                        local flagvec[`i'] = 1
                    }
                }
                else {
                    if `prev_diff_xbr_present' {
                        local amt_spike = `diff_xbr'
                    }
                }
            }
            local flagvec = flagvec[!missing(`xbr_new')]
            local flagvec = flagvec[_n..]
            matrix counts = counts_new[!missing(counts_new)]
            local xbr = xbr_new[!missing(xbr_new)]
        }
        else {
            local flagvec = flagvec[_n..]
        }
        local widths = abs(diff(`xbr'))
        local heights = counts / widths
    }
    else {
        qui sum `varname' if `touse', meanonly
        local n = r(sum)
        local a = `a' == . ? 1 : `a'  // handling missing value
        local yupper = `varname' + (`a' * (_n / `n'))
        local ylower = `yupper' - `a' / `n'
        qui histogram `varname' if `touse' , start(`rx'[1]) width(`rx'[2] - `rx'[1]) `nbins'
        local heights = `r(N)' * _f(binwidth) * _f(bin_heights)
        local xbr = `rx'[1] + (_n - 1) * _f(binwidth)
    }

    local bin_size = `N' / `nbins'
    local cut_pt = min(`varname') -
