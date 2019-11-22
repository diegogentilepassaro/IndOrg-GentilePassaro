clear all

cd "/Users/dgentil1/Desktop/Diego/Brown/Industrial Organization/Industrial Organization/assignment_5/code"

program main
    use "../raw_data/Replication_File_JPE/stata/comprehensive.dta", clear
    clean_data 

	* (a)
	levelsof country_year, local(c_y)
    local forlab: value label country_year
    
	eststo clear
	foreach i of local c_y {
	    local label_`i': label `forlab' `i'
	    eststo: reg y x if country_year == `i'
		test _b[x] = 1
		estadd scalar p_val = r(p)
	}
	esttab * using "../output/table_1.tex", se compress ///
	    stats(p_val r2 N , fmt(%9.3f %9.0g) labels("p-value test beta_hat = 1" "R-squared" "Observations")) ///
	    mtitles("`label_1'" "`label_2'" "`label_3'" "`label_4'" "`label_5'") replace

   * (b)
   gen beta_i = X/Y
   
end

program clean_data
	keep if industry == 1
	
	gen new_country = regexr(country, "[0-9]+", "")
	replace country = new_country
	drop new_country 

	rename (c274a1y c274b1y c274e1y c281a1y) (total_sales raw_mat_cost energy_cost tot_assets)
	gen Y = total_sales - raw_mat_cost - energy_cost
	gen y = ln(Y)
	gen X = tot_assets
	gen x = ln(X)

	drop if (missing(Y) | missing(X))
	
	bysort country year: egen nbr_obs = count(idstd)
	keep if nbr_obs >= 50
	drop nbr_obs
	
	egen country_year = group(country year), label
	
	keep idstd country year country_year Y X y x
end

* EXECUTE
main


