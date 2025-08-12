** Emergency Stop if executed by accident **
balalalablub
* Start of Do-File
cd "/Users/simonsperl/Library/CloudStorage/ProtonDrive-simon.sperl@proton.me/LSE/Studies/Dissertation/Raw Data"
use BES_teaching_long_v29.1.dta, clear
cd "/Users/simonsperl/Library/CloudStorage/ProtonDrive-simon.sperl@proton.me/LSE/Studies/Dissertation"
*------------------------------------------------------------------------------
* Packages
*------------------------------------------------------------------------------
	ssc install ftools, replace
	ssc install reghdfe, replace
	ssc install coefplot, replace
	ssc install estout, replace
	ssc install table1, replace
	ssc install asdoc, replace
	ssc install shp2dta, replace
	ssc install spmap, replace
	ssc install outreg2, replace
*------------------------------------------------------------------------------
// Initial Cleaning Steps and Variable Recoding
*------------------------------------------------------------------------------
* Irrelevant variables
drop starttime likeLDLeader likeSNPLeader likeLD likeSNP likePC likeBrexitParty redistLD redistSNP redistPC immigCon immigLab immigLD immigSNP immigPC scotReferendumVote britishness scottishness welshness englishness europeanness headHouseholdPast ns_sec_parent ns_sec_analytic_parent edlevelParent subjClass speakWelsh endtime turnoutUKGeneral likePCLeader likeUKIPLeader likeBrexitLeader changeNHS welshReferendumIntention efficacyNotUnderstand approveUKGovt approveScotGovt approveWelshGovt cvEconSelf dutyToVote2 euRefVote govtHandleVaccine govtHandlelockdown EUIntegrationSelf EUIntegrationCon EUIntegrationLab EUIntegrationLD EUIntegrationSNP EUIntegrationPC welshgovtHandleVaccine welshgovtHandlelockdown scotgovtHandleVaccine scotgovtHandlelockdown
sort id wave

	* Remove wave 21 because of duplicates (seen in code example from e-mail 4 July 2025, may be fixed in BES now but dropping to be safe)
	drop if wave == 21

	* Remove observations without weights
	drop if missing(weight)
	
	* Count unique respondents per pcon
	bysort pcon id: gen first_id = (_n == 1)
	bysort pcon: egen unique_ids_per_pcon = total(first_id)
	drop first_id	
	* Check for pcons with few respondents -> enough everywhere
	summarize unique_ids_per_pcon, meanonly
	local minval = r(min)
	list pcon unique_ids_per_pcon if unique_ids_per_pcon == `minval'
	
	* pcon in BES is labeled variable, converting to string for inequality data matching
	decode pcon, gen(pcon_str)
	rename pcon pcon_num
	rename pcon_str pcon
	
	* Drop 1 observation "NOT in a 2010 Parliamentary Constituency"
	drop if pcon == "NOT in a 2010 Parliamentary Constituency"

	* Dropping empty pcon
	drop if missing(pcon)
	// Empty Pcon investigation, super useless but i thought some might be convertible to movers
	// 	list id wave if missing(pcon)
	// 	egen ids_with_missing_pcon = tag(id) if missing(pcon)
	// 	count if ids_with_missing_pcon == 1
	// 	* reveals 44 unique ids with 109 missing pcon values
	// 	* continue to identify precisely those that could be one time movers
	// 		gen byte pcon_missing = missing(pcon)
	// 		egen id_missing_pcon = tag(id) if pcon_missing
	// 		gen byte has_missing_pcon = id_missing_pcon == 1
	// 		* unique pcon values per id (including missing)
	// 		gen byte tag_pcon = 1
	// 		bysort id pcon (wave): replace tag_pcon = 0 if _n > 1 & pcon == pcon[_n-1]
	// 		egen num_unique_pcon = total(tag_pcon), by(id)
	// 	* count those with = 3 unique pcons, two normal, one empty
	// 	egen tag_id = tag(id)
	// 	count if tag_id == 1 & has_missing_pcon == 1 & num_unique_pcon == 3
	// 	gen mover_ambig = 1 if tag_id == 1 & has_missing_pcon == 1 & num_unique_pcon == 3
	// 	* only 7 potential movers, manual inspection shows none are valuable
	// 	// Decision to drop all those with missing pcon since they're neither one time movers nor clearly identified as none-movers
	// 	drop id_missing_pcon
	// 	egen id_missing_pcon = max(pcon_missing), by(id)
	// 	drop if id_missing_pcon == 1
	// 	drop ids_with_missing_pcon tag_pcon num_unique_pcon tag_id pcon_missing id_missing_pcon has_missing_pcon mover_ambig

	* Generate variable for PFR vote
	gen vote_pfr = 0
	replace vote_pfr = 1 if inlist(generalElectionVote, 6, 8, 12) // UKIP, BNP, Brexit/Reform
	* Same thing for Green Vote
	gen vote_green = 0
	replace vote_green = 1 if generalElectionVote == 7 // Green
	* Anti-System Vote in General
	gen vote_pop = 0
	replace vote_pop = 1 if vote_pfr == 1 | vote_green == 1
	* Leftist Vote in General
	gen vote_left = 0
	replace vote_left = 1 if inlist(generalElectionVote, 2, 3, 4, 5, 7)
	* Independent Vote
	gen vote_ind = 0
	replace vote_ind = 1 if inlist(generalElectionVote, 13, 9)
	
	* Sanity Check whether people actually change their votes
	bysort id: egen vote_changes = sd(vote_pfr) if !missing(vote_pfr)
	sum vote_changes, detail // yep plenty of vote changes
	drop vote_changes

	* Generate conservative/labour/libdem vote for robustness/follow-ups
	gen con_vote = 1 if generalElectionVote == 1
	replace con_vote = 0 if generalElectionVote != 1
	gen lab_vote = 1 if generalElectionVote == 2
	replace lab_vote = 0 if generalElectionVote != 2
	gen ld_vote = 1 if generalElectionVote == 3
	replace ld_vote = 0 if generalElectionVote != 3
	
	* Create reversed redistribution variables for better intuition
	gen redSelf_rev = 10 - redistSelf
	label var redSelf_rev "Support for redistribution (0=low, 10=high)"
	replace redSelf_rev = . if redistSelf == 9999
	gen redCon_rev = 10 - redistCon
	label var redCon_rev "Conservative redistribution (0=low, 10=high)"
	replace redCon_rev = . if redistCon == 9999
	gen redLab_rev = 10 - redistLab
	label var redLab_rev "Labour redistribution (0=low, 10=high)"
	replace redLab_rev = . if redistLab == 9999
	
	* Waves taken
	egen waves_taken = count(id), by(id)
	
	* Country
	gen country = ""
	replace country = "England" if inlist(gor, 1, 2, 3, 4, 5, 6, 7, 8, 9)
	replace country = "Wales"   if gor == 10
	replace country = "Scotland" if gor == 11
	count if missing(country) // 30 missing, eye-balling data reveals few in Scotland/Wales, rest england
		replace gor = 11 if missing(country) & inlist(pcon, "Dundee West", "Aberdeen South")
		replace gor = 10 if missing(country) & pcon == "Dwyfor Meirionnydd"
		replace country = "Scotland" if missing(country) & gor == 11
		replace country = "Wales" if missing(country) & gor == 10
		replace country = "England" if missing(country) // Assign remaining missing country as England
	count if missing(country)
	
	* Ethnicity Group 
	gen ethnicity_group = ""
	replace ethnicity_group = "White" if inlist(p_ethnicity, 1, 2)
	replace ethnicity_group = "Black" if inlist(p_ethnicity, 11, 12, 13)
	replace ethnicity_group = "Asian" if inlist(p_ethnicity, 7, 8, 9, 10, 14)
	replace ethnicity_group = "Mixed" if inlist(p_ethnicity, 3, 4, 5, 6)
	replace ethnicity_group = "Other" if p_ethnicity == 15
	replace ethnicity_group = "NA" if p_ethnicity == 9998 | missing(p_ethnicity)
		* Dummies
// 		gen eth_white = (ethnicity_group == "White")
// 		gen eth_black = (ethnicity_group == "Black")
// 		gen eth_asian = (ethnicity_group == "Asian")
// 		gen eth_mixed = (ethnicity_group == "Mixed")
// 		gen eth_other = (ethnicity_group == "Other")
		gen non_white = (ethnicity_group != "White") if p_ethnicity != 9998 & !missing(p_ethnicity) // only this is useful actually
		
	* Income Groups, splitting BES categories into three
	gen income_group = .
		replace income_group = 1 if inrange(p_gross_household, 1, 6)        // Low
		replace income_group = 2 if inrange(p_gross_household, 7, 11)       // Medium
		replace income_group = 3 if inrange(p_gross_household, 12, 15)      // High
		* "Prefer not to answer" (9998) and "Don't know" (9999)
		replace income_group = . if inlist(p_gross_household, 9998, 9999)
		* Label
		label define income_grp_lbl 1 "Low" 2 "Medium" 3 "High"
		label values income_group income_grp_lbl
		* Dummies
			gen inc_low = (income_group == 1)
			gen inc_med = (income_group == 2)
			gen inc_high = (income_group == 3)
	
	* Party Affiliation for Summary Statistics
	gen party_clean = ""
		replace party_clean = "Conservative" if partyId == 1
		replace party_clean = "Labour" if partyId == 2
		replace party_clean = "LibDem" if partyId == 3
		replace party_clean = "Green" if partyId == 7
		replace party_clean = "PFR" if inlist(partyId, 6, 8, 12)
		replace party_clean = "Other" if missing(party_clean)
	
	* Female & Unemployed
	gen unemployed = p_work_stat == 6
	gen female = gender == 2
	
	* Recoding Religion
	gen rel_group = ""
	replace rel_group = "None" if p_religion == 1
	replace rel_group = "Christian" if inlist(p_religion, 2, 3, 4, 5, 6, 7, 8, 9, 17, 18, 19)
	replace rel_group = "Muslim" if p_religion == 12
	replace rel_group = "Other" if inlist(p_religion, 10, 11, 13, 14, 15) // Jewish, Hindu, Sikh, Buddhist, Other
	replace rel_group = "Missing" if p_religion == 9998 | missing(p_religion) // Prefer not to say & Missing
	
	* Recoding Housing
	gen rent_own = ""
	replace rent_own = "Own" if inlist(p_housing, 1, 2, 3)
	replace rent_own = "Rent" if inlist(p_housing, 4, 5, 6)
	replace rent_own = "Other" if inlist(p_housing, 7, 8, 9)
	
	* Replacing 9999 don't know value for immigration and trustMps to not mess up aggregates
	replace immigEcon = . if immigEcon == 9999
	replace trustMPs = . if trustMPs == 9999
	replace econPersonalRetro = . if econPersonalRetro == 9999
	
*------------------------------------------------------------------------------
* Merge with lineq data
*------------------------------------------------------------------------------

	* Up to Wave 3 is 2014, Wave 20 and following are past 2019, so no lineq (landreg + zoopla) data
	gen interval = ""
		replace interval = "10-14" if wave <= 3
		replace interval = "15-19" if wave >= 4 & wave <= 19

	merge m:1 pcon interval using lineq_pcon_clean.dta
	* drop all observations with time interval before 2014 // no longer necessary with cleaned lineq file
	* drop if _merge == 2
	order pcon interval, first
	sort id wave

	* Sanity Check: Gini there for each pcon for 15-19 interval (keep waves before/after for pre/post trends)
		list id wave pcon if _merge == 1 & interval != "" & interval != "10-14" // should not exist
		list id wave pcon if _merge == 2 & substr(pcon_code, 1, 1) != "N" // should not exist
		list id pcon wave if _merge == 3 & missing(gini) // should not exist
		list pcon wave id if missing(gini) & interval == "15-19" // should not exist
		list pcon id wave if !missing(gini) & pcon_lreg == 0 & interval == "10-14" // should not exist
		// 0 observations each, merge successful

	* Drop pcons for which there is no BES data (only codes from Northern Ireland)
	drop if _merge == 2
	drop _merge
	
	* Merge Local Authority Code data for possible robustness checks later on -> this is not LSOA
	// merge m:1 id wave using oslaua_long.dta
	// drop if _merge == 2
	// drop _merge
	// drop oslaua

*------------------------------------------------------------------------------
* Generate Mover Variables
*------------------------------------------------------------------------------

	* Ensure Data is sorted
	sort id wave

	* Identify pcon changes
	by id (wave), sort: gen pcon_change = pcon != pcon[_n-1] if id == id[_n-1]
	replace pcon_change = 0 if missing(pcon_change)
	sort id wave

	* Count total moves per person
	gen pcon_change_total = .
	bysort id (wave): replace pcon_change_total = sum(pcon_change)
	bysort id (wave): replace pcon_change_total = pcon_change_total[_N]
	order pcon pcon_change pcon_change_total, first

	* First mover count
	egen idtag = tag(id)
	count if pcon_change_total > 1 & idtag == 1 // 3,234
	count if pcon_change_total == 1 & idtag == 1 // 10,890
	count if pcon_change_total == 0 & idtag == 1 // 102,468

	* Drop multiple movers 
	drop if pcon_change_total > 1 
	 
	* Identify move wave and single movers
	gen move_wave = wave if pcon_change == 1
	bysort id: egen first_move_wave = min(move_wave)
	* Identify single movers (people who move only once)
	gen byte mover = (pcon_change_total == 1)
	order move_wave first_move_wave
	 
	* Flag ids who ever live in zoopla data only pcons
	gen byte zoopla_flag = (pcon_lreg == 0)
	replace zoopla_flag = 0 if missing(pcon_lreg)
	bysort id (wave): egen zoopla_id = max(zoopla_flag)

	* Flag for post-2019 movers who can't be analyzed
	gen byte late_mover = (first_move_wave >= 19) if mover == 1
	replace mover = 0 if late_mover == 1  // Treat as non-movers, 4,728
		* Drop all their waves from late move onwards
		drop if late_mover == 1 & wave >= first_move_wave // keeping all "un-treated" waves
	count if late_mover == 1 & idtag == 1 // still 4728
		
	* New mover count
	count if mover == 1 & first_move_wave == wave // 6,162

	* identify origin and destination constituencies
		* Origin constituency
		bysort id (wave): gen pcon_pre = pcon if wave < first_move_wave & mover == 1
		bysort id (wave): egen pcon_pre_clean = mode(pcon_pre), maxmode
		replace pcon_pre = pcon_pre_clean if mover == 1
		drop pcon_pre_clean

		* Destination constituency  
		bysort id (wave): gen pcon_post = pcon if wave >= first_move_wave & mover == 1
		bysort id (wave): egen pcon_post_clean = mode(pcon_post), maxmode
		replace pcon_post = pcon_post_clean if mover == 1
		drop pcon_post_clean
		
	* Create event time (relative to move)
	gen event_time = wave - first_move_wave if mover == 1
	replace event_time = 0 if mover == 0
		
	* Get inequality values
		* Origin: last observed gini before move
		sort id wave
		gen temp_gini_pre = gini if wave < first_move_wave & mover == 1
		bysort id (wave): replace temp_gini_pre = temp_gini_pre[_n-1] if missing(temp_gini_pre) & mover == 1

		* Get the last non-missing value
		bysort id (wave): gen last_gini_pre = temp_gini_pre if !missing(temp_gini_pre) & mover == 1
		bysort id: gen last_wave_pre = wave if !missing(last_gini_pre)
		bysort id: egen max_wave_pre = max(last_wave_pre)
		gen gini_orig = last_gini_pre if wave == max_wave_pre & mover == 1
		bysort id: egen gini_orig_spread = mean(gini_orig)
		replace gini_orig = gini_orig_spread if mover == 1
		drop temp_gini_pre last_gini_pre last_wave_pre max_wave_pre gini_orig_spread

		* Destination: first observed gini at/after move (not min!)
		gen temp_gini_post = gini if wave >= first_move_wave & mover == 1
		bysort id (wave): gen first_nonmiss_post = temp_gini_post if !missing(temp_gini_post) & mover == 1
		bysort id: gen first_wave_post = wave if !missing(first_nonmiss_post) 
		bysort id: egen min_wave_post = min(first_wave_post)
		gen gini_dest = first_nonmiss_post if wave == min_wave_post & mover == 1
		bysort id: egen gini_dest_spread = mean(gini_dest)
		replace gini_dest = gini_dest_spread if mover == 1
		drop temp_gini_post first_nonmiss_post first_wave_post min_wave_post gini_dest_spread
		
			* Same for average housing values for pcon comparison
				sort id wave
				gen temp_avg_pre = pcon_avg if wave < first_move_wave & mover == 1
				bysort id (wave): replace temp_avg_pre = temp_avg_pre[_n-1] if missing(temp_avg_pre) & mover == 1
				* Get the LAST non-missing value
				bysort id (wave): gen last_avg_pre = temp_avg_pre if !missing(temp_avg_pre) & mover == 1
				bysort id: gen last_wave_pre_avg = wave if !missing(last_avg_pre)
				bysort id: egen max_wave_pre_avg = max(last_wave_pre_avg)
				gen pcon_avg_orig = last_avg_pre if wave == max_wave_pre_avg & mover == 1
				bysort id: egen pcon_avg_orig_spread = mean(pcon_avg_orig)
				replace pcon_avg_orig = pcon_avg_orig_spread if mover == 1
				drop temp_avg_pre last_avg_pre last_wave_pre_avg max_wave_pre_avg pcon_avg_orig_spread
				* Destination: first observed pcon_avg at/after move (not min!)
				gen temp_avg_post = pcon_avg if wave >= first_move_wave & mover == 1
				bysort id (wave): gen first_nonmiss_post_avg = temp_avg_post if !missing(temp_avg_post) & mover == 1
				bysort id: gen first_wave_post_avg = wave if !missing(first_nonmiss_post_avg)
				bysort id: egen min_wave_post_avg = min(first_wave_post_avg)
				gen pcon_avg_dest = first_nonmiss_post_avg if wave == min_wave_post_avg & mover == 1
				bysort id: egen pcon_avg_dest_spread = mean(pcon_avg_dest)
				replace pcon_avg_dest = pcon_avg_dest_spread if mover == 1
				drop temp_avg_post first_nonmiss_post_avg first_wave_post_avg min_wave_post_avg pcon_avg_dest_spread
	
	* Calculate delta gini
	gen delta_gini = gini_dest - gini_orig if mover == 1
	replace delta_gini = 0 if mover == 0

	* Post-move indicator
	gen byte post_move = (wave >= first_move_wave) if mover == 1
	replace post_move = 0 if mover == 0

	* Balanced panel indicator for 2+ waves before/after
	bysort id: gen pre_count = sum(wave < first_move_wave) if mover == 1
	bysort id: gen post_count = sum(wave >= first_move_wave) if mover == 1
	bysort id: egen min_pre = max(pre_count)
	bysort id: egen min_post = max(post_count)
	gen balanced = (min_pre >= 2 & min_post >= 2) if mover == 1
	replace balanced = 0 if mover == 0

	* Sanity Checks
	tab mover
	sum delta_gini if mover == 1, detail
	tab balanced if mover == 1
	count if mover == 1 & missing(gini_orig)
	count if mover == 1 & missing(gini_dest)
	* Dropping more problematic movers
		drop if mover == 1 & (missing(gini_orig) | missing(gini_dest)) // those are 266 movers (Scotland/Northern Ireland)
			// count if first_move_wave == wave & missing(delta_gini) & mover == 1 // 266
			// count if first_move_wave == wave & missing(gini_orig) & mover == 1 // 264
			// count if first_move_wave == wave & missing(gini_dest) & mover == 1 // 24
			// count if first_move_wave == wave & !missing(gini_dest) & mover == 1 & missing(gini_orig) // 2
	* Check for those with large gaps between moves that make event timing impossible to identify
		bysort id (wave): gen last_pre_wave = wave if wave < first_move_wave & mover == 1
		bysort id: egen last_pre = max(last_pre_wave)
		gen move_gap = first_move_wave - last_pre
	//	
	// 	foreach gap in 2 3 4 5 {
	//     egen tag_gap`gap' = tag(id) if mover == 1 & move_gap > `gap'
	//     count if tag_gap`gap' == 1
	//     display "Movers with gap > `gap': " r(N)
	//     drop tag_gap`gap'
	// } // Get numbers before dropping missing gini movers: Counting Movers with gap > 2: 1556, Movers with gap > 3: 1095 (-30 overlap with missing gini), Movers with gap > 4: 796, Movers with gap > 5: 568
		drop if mover == 1 & move_gap > 3 // 3 waves are about a year, so dropping additional 1065 movers here for max sample size 
		
	// * Check the full distribution of event times for movers
	// tab event_time if mover == 1
	// * Get summary statistics
	// sum event_time if mover == 1, detail
	// * See specifically how many are beyond ±10
	// count if mover == 1 & event_time < -10
	// count if mover == 1 & event_time > 10
	// * Look at the extremes
	// tab event_time if mover == 1 & (event_time < -10 | event_time > 10) // not clean enough, using balanced spec for accuracy
	// tab event_time if mover == 1 & balanced == 1 & move_gap == 1 // at least 1000 observations would be good for each event time, -6 to 8 offer this so binning -7 and +9

	* Handle event times from -6 to 8 by binning, richest observations for balanced mover panel
	gen event_time_binned = event_time
	replace event_time_binned = -7 if event_time < -7 & !missing(event_time) & mover == 1
	replace event_time_binned = 9 if event_time > 9 & !missing(event_time) & mover == 1
	* Alternative binning with -10 to +10 for robustness checks
	gen bin_alt = event_time
	replace bin_alt = -10 if event_time < -10 & !missing(event_time) & mover == 1
	replace bin_alt = 10 if event_time > 10 & !missing(event_time) & mover == 1

	* Create event time dummies from binned version
	tab event_time_binned if !missing(event_time_binned), gen(et_)
	tab bin_alt if !missing(bin_alt), gen(alt_et_)

	* Drop reference period et_7 = -1 & alt_et_10 = -1
	drop et_7 alt_et_10

	* Create interactions for both 
	foreach var of varlist et_* {
		gen `var'_X_delta = `var' * delta_gini
	}
	foreach var of varlist alt_et_* {
		gen `var'_X_delta = `var' * delta_gini
	}


*------------------------------------------------------------------------------
* Summary Statistics *
*------------------------------------------------------------------------------

	* Enable Mover/Non-mover/late-mover comparison
	gen mover_cat = .
	replace mover_cat = 0 if mover == 0
	replace mover_cat = 1 if mover == 1 // non-movers without late movers
	replace mover_cat = 2 if late_mover == 1 // late movers
	label define mover_cat 0 "Non-mover" 1 "Mover (treated)" 2 "No late-mover"
	label values mover_cat mover_cat

	* Mover coun final: 
	count if mover == 1 & idtag == 1 // 4,831
	count if mover == 0 & idtag == 1 // 107,196
	count if mover == 0 & idtag == 1 & late_mover == 1 // 4,728
	count if mover == 1 & idtag == 1 & balanced == 1 & move_gap == 1 // 2,292

	* Delta Gini numbers
	gen abs_delta_gini = abs(delta_gini)
	sum abs_delta_gini if mover == 1, detail // Median is .0324458
	count if mover == 1 & first_move_wave == wave & abs_delta_gini > 0.0324458 // 2,461
	count if mover == 1 & first_move_wave == wave & delta_gini > 0 // 2,422

	* Descriptive Summary for non_movers
	cd "/Users/simonsperl/Library/CloudStorage/ProtonDrive-simon.sperl@proton.me/LSE/Studies/Dissertation/Descriptives_01"
		// numeric variables
		estpost tabstat age female unemployed eth_white eth_black eth_asian eth_mixed eth_other non_white ///
			inc_low inc_med inc_high trustMPs immigEcon econPersonalRetro waves_taken, by(mover) statistics(mean sd count) columns(statistics)
		esttab using "summary_numeric.rtf", replace label title("Summary Statistics by Mover Status") ///
			cells("mean(fmt(3)) sd(fmt(2)) count") nonumber
		// categorical variables
		* Summary stats (percentages) by mover for categorical variables
		asdoc tab party_clean mover, col replace title(Party by mover)
		asdoc tab rel_group mover, col append title(Religion by mover)
		asdoc tab country mover, col append title(Country by mover)
		asdoc tab p_edlevel mover, col append title(Education level by mover)
		asdoc tab rent_own mover, col append title(House Ownership level by mover)

	* Descriptive Summary for Appendix including late movers
	cd "/Users/simonsperl/Library/CloudStorage/ProtonDrive-simon.sperl@proton.me/LSE/Studies/Dissertation/Descriptives_02"
		// numeric variables
		estpost tabstat age female unemployed eth_white eth_black eth_asian eth_mixed eth_other non_white ///
			inc_low inc_med inc_high trustMPs immigEcon econPersonalRetro waves_taken, by(mover_cat) statistics(mean sd count) columns(statistics)
		esttab using "summary_numeric2.rtf", replace label title("Summary Statistics by Mover Category") ///
			cells("mean(fmt(3)) sd(fmt(2)) count") nonumber
		// categorical variables	
		* Summary stats (percentages) by mover category for categorical variables
		asdoc tab party_clean mover_cat, col replace title(Party by mover)
		asdoc tab rel_group mover_cat, col append title(Religion by mover)
		asdoc tab country mover_cat, col append title(Country by mover)
		asdoc tab p_edlevel mover_cat, col append title(Education level by mover)
		asdoc tab rent_own mover_cat, col append title(House Ownership level by mover)

	* Change Back Directory!
	cd "/Users/simonsperl/Library/CloudStorage/ProtonDrive-simon.sperl@proton.me/LSE/Studies/Dissertation"

*------------------------------------------------------------------------------
* Compare Origin Pcon and Destination Pcon *
*------------------------------------------------------------------------------

	* gini Comparison t-test
		ttest gini_dest == gini_orig if mover == 1 & event_time == 0 // significant difference
		sum delta_gini if mover == 1 & idtag == 1 // mean gini difference negative, same as t-test
		sum gini_orig if mover == 1 & event_time == 0
		sum gini_dest if mover == 1 & event_time == 0

	* housing values comparison
		ttest pcon_avg_dest == pcon_avg_orig if mover == 1 & event_time == 0
		sum pcon_avg_orig if mover == 1 & event_time == 0
		sum pcon_avg_dest if mover == 1 & event_time == 0

	* Histograms
		histogram delta_gini if mover == 1 & idtag == 1, freq ytitle("Frequency", size(medium)) xtitle("∆ Gini", size(medium)) ///
			ylabel(, angle(horizontal) nogrid) ///
			xlabel(, angle(horizontal))
			
		histogram abs_delta_gini if mover == 1 & idtag == 1, freq ytitle("Frequency", size(medium)) xtitle("∆ Gini Absolute", size(medium)) ///
			ylabel(, angle(horizontal) nogrid) ///
			xlabel(, angle(horizontal))
	
	* Get Urban-Rural Classification: 
	cd "/Users/simonsperl/Library/CloudStorage/ProtonDrive-simon.sperl@proton.me/LSE/Studies/Dissertation/Pcon Comparison"
	merge m:1 pcon using RuralUrban.dta // ONS Data
	drop if _merge == 2
	drop _merge
	sort id wave
		// 	What was done to that data:
		// 	gen urban_rural = .
		// 	* Urban: A1, B1, C1, C2 (England/Wales), 1, 2 (Scotland) // According to documentation of ONSPD
		// 	replace urban_rural = 1 if inlist(ru11ind, "A1", "B1", "C1", "C2")   // England/Wales Urban
		// 	replace urban_rural = 1 if inlist(ru11ind, "1", "2")                // Scotland Urban
		// 	* Rural: D1–F2 (England/Wales), 3–8 (Scotland)
		// 	replace urban_rural = 0 if inlist(ru11ind, "D1", "D2", "E1", "E2", "F1", "F2") // E/W Rural
		// 	replace urban_rural = 0 if inlist(ru11ind, "3", "4", "5", "6", "7", "8")      // Scotland Rural
		// 	label define urb 0 "Rural" 1 "Urban"
		// 	label values urban_rural urb
		// 	gen is_urban = (urban_rural == 1)
		// 	egen urban_share = mean(is_urban), by(pcon)
		// 	* Assign dominant type
		// 	replace urban_rural = 1 if urban_share >= 0.5
		// 	replace urban_rural = 0 if urban_share < 0.5
		// 	rename pcon pcon_code_check
		// 	merge m:1 pcon_code_check using pcon_names.dta
		// 	drop if merge != 3
		// 	keep pcon urban_rural
		// 	duplicates drop pcon, force

	* Save before Pcon Calculations
	save "/Users/simonsperl/Library/CloudStorage/ProtonDrive-simon.sperl@proton.me/LSE/Studies/Dissertation/CleanData03.dta", replace
		cd "/Users/simonsperl/Library/CloudStorage/ProtonDrive-simon.sperl@proton.me/LSE/Studies/Dissertation/Pcon Comparison"
		* Get unique origin constituencies
			* First block: get outflows
			preserve
				keep if mover == 1
				bysort id: keep if _n == 1
				collapse (count) outflow = id, by(pcon_pre)
				rename pcon_pre pcon
				save pcon_outflows, replace
			restore
			* Second block: get inflows
			preserve
				keep if mover == 1
				bysort id: keep if _n == 1
				collapse (count) inflow = id, by(pcon_post)
				rename pcon_post pcon
				save pcon_inflows, replace
			restore
		* Combine flows and calculate net moving
			use pcon_outflows, clear
			merge 1:1 pcon using pcon_inflows
			replace outflow = 0 if missing(outflow)
			replace inflow = 0 if missing(inflow)
			* Calculate net flow
			gen net_flow = inflow - outflow
			gen flow_ratio = inflow / outflow if outflow > 0
			gen pcon_type = "MoveIn" if net_flow > 0
			replace pcon_type = "MoveOut" if net_flow < 0
			replace pcon_type = "Both" if net_flow == 0
		* Summary
		tab pcon_type
		sum net_flow, detail
		drop _merge
		* Save for merging
		save pcon_net_flows, replace
		
	* Create constituency profiles using all observations
	cd "/Users/simonsperl/Library/CloudStorage/ProtonDrive-simon.sperl@proton.me/LSE/Studies/Dissertation"
		use CleanData03.dta, clear
		cd "/Users/simonsperl/Library/CloudStorage/ProtonDrive-simon.sperl@proton.me/LSE/Studies/Dissertation/Pcon Comparison"
	
	preserve
    * Only untreated observations/ those in the move period pre 2019, closest to the state of pcon that movers arrive in
    keep if post_move == 0 & wave <= 19
	* Calculate new variables for averages across observations, to take into account change over time
		* Exclude invalid redistribution
		gen red_clean = redSelf_rev
		replace red_clean = . if redSelf_rev == -9989

		* Take ID-level averages
		gen rel_none = (rel_group == "None")
		gen own_home = (rent_own == "Own")
		egen nonbelief = mean(rel_none), by(id)
		egen mean_age = mean(age), by(id)
		egen mean_red = mean(red_clean), by(id)
		egen mean_pfr = mean(vote_pfr), by(id)
		egen mean_inc_low = mean(inc_low), by(id)
		egen mean_inc_high = mean(inc_high), by(id)
		egen mean_own = mean(own_home), by(id)
		egen mean_house = mean(pcon_avg), by(id)
		egen mean_immig = mean(immigEcon), by(id)
		egen mean_trust = mean(trustMPs), by(id)
		egen mean_econ = mean(econPersonalRetro), by(id)
		egen max_ed = max(p_edlevel), by(id)
		gen high_ed = 0
		replace high_ed = 1 if max_ed >= 4
		egen mean_unemploy = mean(unemployed), by (id)

    * One observation per person per pcon
    bysort id pcon (wave): keep if _n == 1
    * Collapse to pcon profiles
		
	collapse (mean) ///
		x_avg_gini = gini ///
		x_avg_gini_orig = gini_orig ///
		x_avg_house = mean_house ///
		x_avg_house_orig = pcon_avg_orig ///
		x_pct_pfr = mean_pfr ///
		x_avg_age = mean_age ///
		x_pct_female = female ///
		x_pct_atheist = nonbelief ///
		x_pct_unemploy = mean_unemploy ///
		x_pct_own = mean_own ///
		x_pct_low_inc = mean_inc_low ///
		x_pct_high_inc = mean_inc_high ///
		x_pct_nonwhite = non_white ///
		x_pct_uni = high_ed ///
		x_avg_redist = mean_red ///
		x_avg_immig = mean_immig ///
		x_avg_trust = mean_trust ///
		x_avg_econ = mean_econ ///
		x_pct_urban = urban_rural ///
		(count) n_respondents = id ///
		(first) country, by(pcon)

    save pcon_profiles, replace
	restore
	
	tempfile gini_dest // because if using one observation per mover, they can't be in two pcons at once
		preserve
		keep if mover==1 & event_time==0
		collapse (mean) gini_dest, by(pcon_post)
		rename pcon_post pcon
		rename gini_dest gini_dest_pcon
		save `gini_dest'
		restore
		
		use pcon_profiles.dta, clear
		merge 1:1 pcon using `gini_dest'
		rename _merge merge1
		save pcon_profiles, replace
		merge m:1 pcon using pcon_net_flows.dta
		drop if _merge != 3 // only one pcon
		drop _merge merge1
		rename gini_dest_pcon x_avg_gini_dest
		save pcon_profiles, replace
	
	cd "/Users/simonsperl/Library/CloudStorage/ProtonDrive-simon.sperl@proton.me/LSE/Studies/Dissertation"
	use CleanData03.dta, clear
	cd "/Users/simonsperl/Library/CloudStorage/ProtonDrive-simon.sperl@proton.me/LSE/Studies/Dissertation/Pcon Comparison"

	tempfile avg_dest // because if using one observation per mover, they can't be in two pcons at once
		preserve
			keep if mover == 1 & event_time == 0
			collapse (mean) pcon_avg_dest, by(pcon_post)
			rename pcon_post pcon
			rename pcon_avg_dest pcon_avg_dest_pcon
			save `avg_dest'
		restore

		use pcon_profiles.dta, clear
		merge 1:1 pcon using `avg_dest'
		rename _merge merge2
		save pcon_profiles, replace

		merge m:1 pcon using pcon_net_flows.dta
		drop if _merge != 3
		drop _merge merge2
		rename pcon_avg_dest_pcon x_avg_house_dest
		save pcon_profiles, replace

	* Export Summary Statistics for Origin/Destination Pcons
		* Create tempfile
		tempfile dest
		* Save destination summary (weighted by inflow)
		preserve
			gen weight = inflow
			collapse (mean) ///
				x_avg_gini x_avg_gini_orig x_avg_gini_dest x_avg_house x_avg_house_orig x_avg_house_dest x_avg_age ///
				x_pct_pfr x_pct_female x_pct_atheist x_pct_nonwhite ///
				x_pct_unemploy x_pct_own x_pct_low_inc x_pct_high_inc ///
				x_pct_uni x_avg_redist x_avg_immig x_avg_trust x_avg_econ x_pct_urban ///
				[aw=weight]
			gen role = "Destination"
			save `dest'
		restore
		* Now create origin summary (weighted by outflow)
			gen weight = outflow
			collapse (mean) ///
				x_avg_gini x_avg_gini_orig x_avg_gini_dest x_avg_house x_avg_house_orig x_avg_house_dest x_avg_age ///
				x_pct_pfr x_pct_female x_pct_atheist x_pct_nonwhite ///
				x_pct_unemploy x_pct_own x_pct_low_inc x_pct_high_inc ///
				x_pct_uni x_avg_redist x_avg_immig x_avg_trust x_avg_econ x_pct_urban ///
				[aw=weight]
			gen role = "Origin"
		* Append destination
		append using `dest'
		* Export combined table
		order role
		export excel mover_profiles.xlsx, firstrow(variables) replace
	use pcon_profiles.dta, clear

*------------------------------------------------------------------------------
* Map Creation *
*------------------------------------------------------------------------------

cd "/Users/simonsperl/Library/CloudStorage/ProtonDrive-simon.sperl@proton.me/LSE/Studies/Dissertation/Pcon Comparison"
	* Get pcon codes for map
	merge m:1 pcon using pcon_names.dta
	drop if _merge != 3
	drop _merge no_lineq
	rename pcon_code_check pcon_code
	save "pcon_profiles.dta", replace
	
	clear 
	* This has to be done only once
// 	shp2dta using "/Users/simonsperl/Library/CloudStorage/ProtonDrive-simon.sperl@proton.me/LSE/Studies/Dissertation/Map Data/WPC_Dec_2015_SGCB_GB.shp", ///
// 		database(map_data) coordinates(map_coords) genid(id)
	use map_data, clear // check variable names
	use pcon_profiles.dta, clear
	rename pcon_code pcon15cd
	save "map_profiles.dta", replace
	use map_data, clear
	merge 1:1 pcon15cd using map_profiles.dta
	
	* Adjust Maps based on need
		* Inflow vs Outflow Maps
		spmap inflow using map_coords.dta, id(id) fcolor(Blues) ///
			ocolor(black ..) legtitle("Total Mover Inflow") ///
			legend(position(10))
		spmap outflow using map_coords.dta, id(id) fcolor(Oranges) ///
			ocolor(black ..) legtitle("Total Mover Outflow") ///
			legend(position(10))
		* Net flow map
		spmap net_flow using map_coords.dta, id(id) ///
			clmethod(custom) clbreaks(-21 -10 -5 -2 0 2 5 10 14) ///
			fcolor(orange_red dkorange erose white white ltblue eltblue ebblue) ///
			ocolor(black ..) ///
			legend(position(10)) ///
			legtitle("Net Migration Flow")
	
// 		* Urban vs Rural Map
// 		spmap x_pct_urban using map_coords.dta, id(id) ///
// 			clmethod(unique) fcolor(gs12 emerald) ocolor(black ..) ///
// 			legtitle("Urban (1) vs Rural (0)")
// 		spmap outflow using map_coords.dta, id(id) fcolor(Blues) ///
// 			ocolor(none ..) legtitle("Total Mover Outflow")
	
** Additional Checks to double-check pcon profile data

	cd "/Users/simonsperl/Library/CloudStorage/ProtonDrive-simon.sperl@proton.me/LSE/Studies/Dissertation"
	use CleanData03.dta, clear
	
	* Sanity Check: Compare Urban-Rural for Pre/Post Pcon
	tab urban_rural if mover == 1 & pcon == pcon_pre & idtag == 1 // same number 0.8487
	tab urban_rural if mover == 1 & event_time == 0 // same number 0.7996

	* Manually get country comparison for origin/destination pcons = constant across observations for pcon
	tab country if mover == 1 & pcon == pcon_pre & idtag == 1 // origin
	tab country if mover == 1 & event_time == 0 // destination
		
*------------------------------------------------------------------------------
* MAIN ANALYSES : POLITICAL BEHAVIOR (PFR VOTING) *
*------------------------------------------------------------------------------
* -----------------------------------------------------------------
* A Main Populist Far Right Vote Mover Event Study
* -----------------------------------------------------------------
	* Main Model Full mover panel
	reghdfe vote_pfr et_* [aweight=weight], /// 
	absorb(id wave pcon) cluster(id pcon)
		* Export
		outreg2 using eventstudy_et.doc, ///
			keep(et_*) ///
			word replace se
	* Coefplot doesn't work because of missing reference period, would need to add to model when using coefplot, tried a full day to make it work
	* Working solution: twoway 
{
    * Matrix for interaction coefficients
    matrix results = J(11, 4, .)  // periods from -5 to +5
    
    * Extract coefficients for et_3 through et_13 (which correspond to -5 to +5)
    * Mapping: et_1 = -7, et_2 = -6, ..., et_7 = -1 (omitted), et_8 = 0, ..., et_17 = 9
    local row = 1
    forvalues i = 3/13 {
        if `i' == 7 {  // Reference period (event_time = -1, et_7 is omitted)
            matrix results[`row', 1] = -1   // Event time = -1
            matrix results[`row', 2] = 0    // Coefficient = 0
            matrix results[`row', 3] = 0    // Lower CI = 0
            matrix results[`row', 4] = 0    // Upper CI = 0
        }
        else {
            * Map et number to event time
            * et_1 = -7, et_2 = -6, ..., et_7 = -1, et_8 = 0, ..., et_17 = 9
            * So event_time = `i' - 8
            local event_time = `i' - 8
            
            matrix results[`row', 1] = `event_time'
            matrix results[`row', 2] = _b[et_`i'_X_delta]
            matrix results[`row', 3] = _b[et_`i'_X_delta] - 1.96*_se[et_`i'_X_delta]
            matrix results[`row', 4] = _b[et_`i'_X_delta] + 1.96*_se[et_`i'_X_delta]
        }
        local row = `row' + 1
    }
    
    preserve
        clear
        svmat results
        rename results1 event_time
        rename results2 coef
        rename results3 ci_lower
        rename results4 ci_upper
        
        * Clean version without title
        twoway (connected coef event_time, lcolor(navy) mcolor(navy) msymbol(O)) ///
               (rcap ci_lower ci_upper event_time, lcolor(navy) lpattern(dash)), ///
               xline(-0.5, lcolor(red) lpattern(dash)) ///
               yline(0, lcolor(black)) ///
               xlabel(-5(1)5, labsize(small)) ///
               ylabel(, angle(0) labsize(small)) ///
               xtitle("Waves Relative to Move", size(small)) ///
               ytitle("Effect on PFR Voting", size(small)) ///
               legend(off) ///
               graphregion(color(white)) ///
               plotregion(margin(medium)) ///
               scheme(s2mono)
        
        graph export "event_study_main_notitle.png", replace width(2400) height(1600)
    restore
}
	
	* Main Model Balanced panel (no move gap, min observations)
	reghdfe vote_pfr et_* if mover == 1 & move_gap == 1 & balanced == 1 | mover == 0 [aweight=weight], ///
	absorb(id wave pcon) cluster(id pcon)
	{
    * Matrix for interaction coefficients
    matrix results = J(11, 4, .)  // periods from -5 to +5
    
    * Extract coefficients for et_3 through et_13 (which correspond to -5 to +5)
    * Mapping: et_1 = -7, et_2 = -6, ..., et_7 = -1 (omitted), et_8 = 0, ..., et_17 = 9
    local row = 1
    forvalues i = 3/13 {
        if `i' == 7 {  // Reference period (event_time = -1, et_7 is omitted)
            matrix results[`row', 1] = -1   // Event time = -1
            matrix results[`row', 2] = 0    // Coefficient = 0
            matrix results[`row', 3] = 0    // Lower CI = 0
            matrix results[`row', 4] = 0    // Upper CI = 0
        }
        else {
            * Map et number to event time
            * et_1 = -7, et_2 = -6, ..., et_7 = -1, et_8 = 0, ..., et_17 = 9
            * So event_time = `i' - 8
            local event_time = `i' - 8
            
            matrix results[`row', 1] = `event_time'
            matrix results[`row', 2] = _b[et_`i'_X_delta]
            matrix results[`row', 3] = _b[et_`i'_X_delta] - 1.96*_se[et_`i'_X_delta]
            matrix results[`row', 4] = _b[et_`i'_X_delta] + 1.96*_se[et_`i'_X_delta]
        }
        local row = `row' + 1
    }
    
    preserve
        clear
        svmat results
        rename results1 event_time
        rename results2 coef
        rename results3 ci_lower
        rename results4 ci_upper
        
        * Clean version without title
        twoway (connected coef event_time, lcolor(navy) mcolor(navy) msymbol(O)) ///
               (rcap ci_lower ci_upper event_time, lcolor(navy) lpattern(dash)), ///
               xline(-0.5, lcolor(red) lpattern(dash)) ///
               yline(0, lcolor(black)) ///
               xlabel(-5(1)5, labsize(small)) ///
               ylabel(, angle(0) labsize(small)) ///
               xtitle("Waves Relative to Move", size(small)) ///
               ytitle("Effect on PFR Voting", size(small)) ///
               legend(off) ///
               graphregion(color(white)) ///
               plotregion(margin(medium)) ///
               scheme(s2mono)
        
        graph export "event_study_balanced_notitle.png", replace width(2400) height(1600)
    restore
}
		outreg2 using eventstudy_et.doc, keep(et_*) word append se	
* -----------------------------------------------------------------
* B PFR Event Study No Weights
* -----------------------------------------------------------------
	* B No Weights Full mover panel
	reghdfe vote_pfr et_*, absorb(id wave pcon) cluster(id pcon)
		outreg2 using eventstudy_et.doc, keep(et_*) word append se
	* B No Weights Balanced panel
	reghdfe vote_pfr et_* if mover == 1 & move_gap == 1 & balanced == 1 | mover == 0, absorb(id wave pcon) cluster(id pcon)
		outreg2 using eventstudy_et.doc, keep(et_*) word append se
* -----------------------------------------------------------------
* C PFR Event Study ID Cluster
* -----------------------------------------------------------------
	* C Cluster ID Only Full mover panel
	reghdfe vote_pfr et_* [aweight=weight], /// 
	absorb(id wave pcon) cluster(id)
		outreg2 using eventstudy_et.doc, keep(et_*) word append se
	* C Cluster ID Only Balanced panel
	reghdfe vote_pfr et_* if mover == 1 & move_gap == 1 & balanced == 1 | mover == 0 [aweight=weight], ///
	absorb(id wave pcon) cluster(id)
		outreg2 using eventstudy_et.doc, keep(et_*) word append se
* -----------------------------------------------------------------
* D PFR Event Study Sample Restrictions
* -----------------------------------------------------------------	
* 1 Land Registry Data Only
	* Full mover panel
	reghdfe vote_pfr et_* if zoopla_id == 0 [aweight=weight], /// 
	absorb(id wave pcon) cluster(id pcon)
		* Export
		outreg2 using eventstudy_restrict.doc, ///
			keep(et_*) ///
			word replace se
	* Balanced panel
	reghdfe vote_pfr et_* if zoopla_id == 0 & (mover == 1 & move_gap == 1 & balanced == 1 | mover == 0) [aweight=weight], ///
	absorb(id wave pcon) cluster(id pcon)
		outreg2 using eventstudy_restrict.doc, keep(et_*) word append se
		
* 2 Subset of only higher inequality
	* Full Panel
	reghdfe vote_pfr et_* if delta_gini > 0 | mover == 0 [aweight=weight], ///
		absorb(id wave pcon) cluster(id pcon)
		outreg2 using eventstudy_restrict.doc, keep(et_*) word append se
	* Balanced Panel
	reghdfe vote_pfr et_* if delta_gini > 0 & move_gap == 1 & balanced == 1 | mover == 0 [aweight=weight], ///
		absorb(id wave pcon) cluster(id pcon)
		outreg2 using eventstudy_restrict.doc, keep(et_*) word append se
	
* 3 Subset of only lower inequality
	* Full Panel
	reghdfe vote_pfr et_* if delta_gini < 0 | mover == 0 [aweight=weight], ///
		absorb(id wave pcon) cluster(id pcon)
		outreg2 using eventstudy_restrict.doc, keep(et_*) word append se
	* Balanced Panel
	reghdfe vote_pfr et_* if delta_gini < 0 & move_gap == 1 & balanced == 1 | mover == 0 [aweight=weight], ///
		absorb(id wave pcon) cluster(id pcon)
		outreg2 using eventstudy_restrict.doc, keep(et_*) word append se

* 4 Subset of only movers with meaningful absolute inequality changes
// 	sum abs_delta_gini if mover == 1 & idtag == 1, detail
// 	sum delta_gini if mover == 1 & idtag == 1, detail
	* Subset of only above median absolute inequality changes
		* Full Panel
		reghdfe vote_pfr et_* if abs_delta_gini > 0.0334603 | mover == 0 [aweight=weight], ///
			absorb(id wave pcon) cluster(id pcon)
			outreg2 using eventstudy_restrict.doc, keep(et_*) word append se
		* Balanced Panel
		reghdfe vote_pfr et_* if abs_delta_gini > 0.0334603 & move_gap == 1 & balanced == 1 | mover == 0 [aweight=weight], ///
			absorb(id wave pcon) cluster(id pcon)
			outreg2 using eventstudy_restrict.doc, keep(et_*) word append se
	* Subset of only 75% quartile 
		* Full Panel
		reghdfe vote_pfr et_* if abs_delta_gini > 0.0599993 | mover == 0 [aweight=weight], ///
			absorb(id wave pcon) cluster(id pcon)
			outreg2 using eventstudy_restrict.doc, keep(et_*) word append se
		* Balanced Panel
		reghdfe vote_pfr et_* if abs_delta_gini > 0.0599993 & move_gap == 1 & balanced == 1 | mover == 0 [aweight=weight], ///
			absorb(id wave pcon) cluster(id pcon)
			outreg2 using eventstudy_restrict.doc, keep(et_*) word append se
// 	* Excluding top 5% (Extra) -> results stay super significant for balanced, but full panel nah
// 		* Full Panel
// 		reghdfe vote_pfr et_* if abs_delta_gini < 0.1120375 | mover == 0 [aweight=weight], ///
// 			absorb(id wave pcon) cluster(id pcon)
// 			outreg2 using eventstudy_restrictX.doc, keep(et_*) word replace se
// 		* Balanced Panel
// 		reghdfe vote_pfr et_* if abs_delta_gini < 0.1120375 & move_gap == 1 & balanced == 1 | mover == 0 [aweight=weight], ///
// 			absorb(id wave pcon) cluster(id pcon)
// 			outreg2 using eventstudy_restrictX.doc, keep(et_*) word append se		
* -----------------------------------------------------------------
* E Event Study Other Variables
* -----------------------------------------------------------------
* 1 Green Party
	* Full panel
	reghdfe vote_green et_* [aweight=weight], /// 
	absorb(id wave pcon) cluster(id pcon)
		outreg2 using robust_vars.doc, ///
			keep(et_*) ///
			word replace se
	* Balanced panel
	reghdfe vote_green et_* if mover == 1 & move_gap == 1 & balanced == 1 | mover == 0 [aweight=weight], ///
	absorb(id wave pcon) cluster(id pcon)
		outreg2 using robust_vars.doc, keep(et_*) word append se
* 2 Leftist Parties (Labour, LibDem, SNP, Plaid, Green) // Re-Do
	* Full panel
	reghdfe vote_left et_* [aweight=weight], /// 
	absorb(id wave pcon) cluster(id pcon)
		outreg2 using robust_vars.doc, keep(et_*) word append se
	* Balanced panel
	reghdfe vote_left et_* if mover == 1 & move_gap == 1 & balanced == 1 | mover == 0 [aweight=weight], ///
	absorb(id wave pcon) cluster(id pcon)
		outreg2 using robust_vars.doc, keep(et_*) word append se
	* Balanced with labour  only 
	reghdfe lab_vote et_* if mover == 1 & move_gap == 1 & balanced == 1 | mover == 0 [aweight=weight], ///
	absorb(id wave pcon) cluster(id pcon)
		outreg2 using followup_het.doc, ///
			keep(et_*) ///
			word replace se
	* Balanced with LibDem ? // does any left party even benefit?, nothing here
	reghdfe ld_vote et_* if mover == 1 & move_gap == 1 & balanced == 1 | mover == 0 [aweight=weight], ///
	absorb(id wave pcon) cluster(id pcon)
		outreg2 using followup_het.doc, ///
			keep(et_*) ///
			word append se
	* Others/Independent? -> significance here
	reghdfe vote_ind et_* if mover == 1 & move_gap == 1 & balanced == 1 | mover == 0 [aweight=weight], ///
	absorb(id wave pcon) cluster(id pcon)
		outreg2 using followup_het.doc, ///
			keep(et_*) ///
			word append se
	* Conservatives? 
	reghdfe con_vote et_* if mover == 1 & move_gap == 1 & balanced == 1 | mover == 0 [aweight=weight], ///
	absorb(id wave pcon) cluster(id pcon)
		outreg2 using followup_het.doc, ///
			keep(et_*) ///
			word append se
		// SNP & Plaid excluded bc Wales/Scotland specific and excluding Scotland (zoopla) still shows results
* 3 Anti-System: Green Party or PFR // Re-Do
	* Full panel
	reghdfe vote_pop et_* [aweight=weight], /// 
	absorb(id wave pcon) cluster(id pcon)
		outreg2 using robust_vars.doc, keep(et_*) word append se
	* Balanced panel
	reghdfe vote_pop et_* if mover == 1 & move_gap == 1 & balanced == 1 | mover == 0 [aweight=weight], ///
	absorb(id wave pcon) cluster(id pcon)
		outreg2 using robust_vars.doc, keep(et_*) word append se
* 4 Redistribution
	* Full panel
	reghdfe redSelf_rev et_* [aweight=weight], /// 
	absorb(id wave pcon) cluster(id pcon)
		outreg2 using robust_vars2.doc, ///
			keep(et_*) ///
			word replace se
	* Balanced panel
	reghdfe redSelf_rev et_* if mover == 1 & move_gap == 1 & balanced == 1 | mover == 0 [aweight=weight], ///
	absorb(id wave pcon) cluster(id pcon)
		outreg2 using robust_vars2.doc, keep(et_*) word append se
* 5 Authoritarian-Libertarian Scale
	* Full panel
	reghdfe al_scale et_* [aweight=weight], /// 
	absorb(id wave pcon) cluster(id pcon)
		outreg2 using robust_vars2.doc, keep(et_*) word append se
	* Balanced panel
	reghdfe al_scale et_* if mover == 1 & move_gap == 1 & balanced == 1 | mover == 0 [aweight=weight], ///
	absorb(id wave pcon) cluster(id pcon)
		outreg2 using robust_vars2.doc, keep(et_*) word append se
* 6 Left-Right Scale
	* Full panel
	reghdfe lr_scale et_* [aweight=weight], /// 
	absorb(id wave pcon) cluster(id pcon)
		outreg2 using robust_vars2.doc, keep(et_*) word append se
	* Balanced panel
	reghdfe lr_scale et_* if mover == 1 & move_gap == 1 & balanced == 1 | mover == 0 [aweight=weight], ///
	absorb(id wave pcon) cluster(id pcon)
		outreg2 using robust_vars2.doc, keep(et_*) word append se
* 7 Like Lab Leader
// First replace missing/don't know
replace likeLabLeader = . if likeLabLeader == 9999
	* Full panel
	reghdfe likeLabLeader et_* [aweight=weight], /// 
	absorb(id wave pcon) cluster(id pcon)
		outreg2 using robust_vars2.doc, keep(et_*) word append se
	* Balanced panel
	reghdfe likeLabLeader et_* if mover == 1 & move_gap == 1 & balanced == 1 | mover == 0 [aweight=weight], ///
	absorb(id wave pcon) cluster(id pcon)
		outreg2 using robust_vars2.doc, keep(et_*) word append se
* -----------------------------------------------------------------
* X PFR Event Study Alternative Binning
* -----------------------------------------------------------------
	* A: Main Model Full mover panel
	reghdfe vote_pfr alt_et_* [aweight=weight], /// 
	absorb(id wave pcon) cluster(id pcon)
		* Export
		outreg2 using alt_binning.doc, ///
			keep(alt_et_*) ///
			word replace se
	* A: Main Model Balanced panel (no move gap, min observations)
	reghdfe vote_pfr alt_et_* if mover == 1 & move_gap == 1 & balanced == 1 | mover == 0 [aweight=weight], ///
	absorb(id wave pcon) cluster(id pcon)
		outreg2 using alt_binning.doc, keep(alt_et_*) word append se
* -----------------------------------------------------------------
* Y PFR Event Study Fake Treatment
* -----------------------------------------------------------------		
	* Random Assignment
	preserve
		keep if mover == 1
		keep id delta_gini
		duplicates drop
		gen rand = runiform()
		sort rand
		gen fake_delta = delta_gini[_n+1]   // circular shift
		replace fake_delta = delta_gini[1] if missing(fake_delta)
		save fake_assignments, replace
	restore
	merge m:1 id using fake_assignments, keepusing(fake_delta)
	drop _merge
	foreach t of numlist 1/6 {
	gen xet_`t' = et_`t'
}
		foreach t of numlist 8/17 {
			gen xet_`t' = et_`t'
		}
	* Create interactions
	foreach var of varlist xet_* {
		gen `var'_X_fake = `var' * fake_delta
	}
	* Run Fake Delta_Gini Check Full Mover Panel
	reghdfe vote_pfr xet_* [aweight=weight], ///
		absorb(id wave pcon) cluster(id pcon)
		outreg2 using eventstudy_fake.doc, ///
			keep(xet_*) ///
			word replace se
	* Run Fake Delta_Gini Check Balanced Panel
	reghdfe vote_pfr xet_* if mover == 1 & move_gap == 1 & balanced == 1 | mover == 0 [aweight=weight], ///
	absorb(id wave pcon) cluster(id pcon)
		outreg2 using eventstudy_fake.doc, keep(xet_*) word append se
		
* -----------------------------------------------------------------
* Z Heterogeneity 
* -----------------------------------------------------------------	

* Age Comparison, new grouping to cut categories
	gen age_move = age if mover == 1 & event_time == 0
	egen age_move_all = max(age_move), by(id) // assign for all id observations
		gen agegroupH = .
		replace agegroupH = 1 if age_move_all >= 18 & age_move_all <= 29
		replace agegroupH = 2 if age_move_all >= 30 & age_move_all <= 44
		replace agegroupH = 3 if age_move_all >= 45 & age_move_all <= 60
		replace agegroupH = 4 if age_move_all >= 60
		replace agegroupH = . if mover == 0
		label define ageHlbl 1 "18–29" 2 "30–44" 3 "45-60" 4 "60+"
		label values agegroupH ageHlbl
	* 18-29
		* Full panel 
		reghdfe vote_pfr et_* if agegroupH == 1 | mover == 0 [aweight=weight], /// 
		absorb(id wave pcon) cluster(id pcon)
			* Export
			outreg2 using het_age.doc, ///
				keep(et_*) ///
				word replace se
		* Balanced Panel
		reghdfe vote_pfr et_* if agegroupH == 1 & move_gap == 1 & balanced == 1 | mover == 0 [aweight=weight], ///
		absorb(id wave pcon) cluster(id pcon)
			outreg2 using het_age.doc, keep(et_*) word append se
	* 30-44
		* Full panel 
		reghdfe vote_pfr et_* if agegroupH == 2 | mover == 0 [aweight=weight], /// 
		absorb(id wave pcon) cluster(id pcon)
			outreg2 using het_age.doc, keep(et_*) word append se
		* Balanced Panel
		reghdfe vote_pfr et_* if agegroupH == 2 & move_gap == 1 & balanced == 1 | mover == 0 [aweight=weight], ///
		absorb(id wave pcon) cluster(id pcon)
			outreg2 using het_age.doc, keep(et_*) word append se
	* 45-60
		* Full panel 
		reghdfe vote_pfr et_* if agegroupH == 3 | mover == 0 [aweight=weight], /// 
		absorb(id wave pcon) cluster(id pcon)
			outreg2 using het_age.doc, keep(et_*) word append se
		* Balanced Panel
		reghdfe vote_pfr et_* if agegroupH == 3 & move_gap == 1 & balanced == 1 | mover == 0 [aweight=weight], ///
		absorb(id wave pcon) cluster(id pcon)
			outreg2 using het_age.doc, keep(et_*) word append se
	* 60+
		* Full panel 
		reghdfe vote_pfr et_* if agegroupH == 4 | mover == 0 [aweight=weight], /// 
		absorb(id wave pcon) cluster(id pcon)
			outreg2 using het_age.doc, keep(et_*) word append se
		* Balanced Panel
		reghdfe vote_pfr et_* if agegroupH == 4 & move_gap == 1 & balanced == 1 | mover == 0 [aweight=weight], ///
		absorb(id wave pcon) cluster(id pcon)
			outreg2 using het_age.doc, keep(et_*) word append se
//________________________________________________________________________________

* Education Comparison Below A-level, A-level (4), Higher Education
	gen ed_move = p_edlevel if mover == 1 & event_time == 0
	egen ed_move_all = max(ed_move), by(id) // assign for all id observations
		gen edlevelH = .
		replace edlevelH = 1 if ed_move_all < 4
		replace edlevelH = 2 if ed_move_all == 4
		replace edlevelH = 3 if ed_move_all > 4
		replace edlevelH = . if mover == 0 // overkill but not super focused rn so better safe than sorry
		label define edHlbl 1 "Below A-level" 2 "A-level" 3 "Higher Ed"
		label values edlevelH edHlbl
	* Below A-level
		* Full panel 
		reghdfe vote_pfr et_* if edlevelH == 1 | mover == 0 [aweight=weight], /// 
		absorb(id wave pcon) cluster(id pcon)
		{
    * Matrix for interaction coefficients
    matrix results = J(11, 4, .)  // periods from -5 to +5
    
    * Extract coefficients for et_3 through et_13 (which correspond to -5 to +5)
    * Mapping: et_1 = -7, et_2 = -6, ..., et_7 = -1 (omitted), et_8 = 0, ..., et_17 = 9
    local row = 1
    forvalues i = 3/13 {
        if `i' == 7 {  // Reference period (event_time = -1, et_7 is omitted)
            matrix results[`row', 1] = -1   // Event time = -1
            matrix results[`row', 2] = 0    // Coefficient = 0
            matrix results[`row', 3] = 0    // Lower CI = 0
            matrix results[`row', 4] = 0    // Upper CI = 0
        }
        else {
            * Map et number to event time
            * et_1 = -7, et_2 = -6, ..., et_7 = -1, et_8 = 0, ..., et_17 = 9
            * So event_time = `i' - 8
            local event_time = `i' - 8
            
            matrix results[`row', 1] = `event_time'
            matrix results[`row', 2] = _b[et_`i'_X_delta]
            matrix results[`row', 3] = _b[et_`i'_X_delta] - 1.96*_se[et_`i'_X_delta]
            matrix results[`row', 4] = _b[et_`i'_X_delta] + 1.96*_se[et_`i'_X_delta]
        }
        local row = `row' + 1
    }
    
    preserve
        clear
        svmat results
        rename results1 event_time
        rename results2 coef
        rename results3 ci_lower
        rename results4 ci_upper
        
        * Clean version without title
        twoway (connected coef event_time, lcolor(navy) mcolor(navy) msymbol(O)) ///
               (rcap ci_lower ci_upper event_time, lcolor(navy) lpattern(dash)), ///
               xline(-0.5, lcolor(red) lpattern(dash)) ///
               yline(0, lcolor(black)) ///
               xlabel(-5(1)5, labsize(small)) ///
               ylabel(, angle(0) labsize(small)) ///
               xtitle("Waves Relative to Move", size(small)) ///
               ytitle("Effect on PFR Voting", size(small)) ///
               legend(off) ///
               graphregion(color(white)) ///
               plotregion(margin(medium)) ///
               scheme(s2mono)
        
        graph export "het_lowered_full.png", replace width(2400) height(1600)
    restore
}
// 			* Export
// 			outreg2 using het_ed.doc, ///
// 				keep(et_*) ///
// 				word replace se
		* Balanced Panel
		reghdfe vote_pfr et_* if edlevelH == 1 & move_gap == 1 & balanced == 1 | mover == 0 [aweight=weight], ///
		absorb(id wave pcon) cluster(id pcon)
		{
    * Matrix for interaction coefficients
    matrix results = J(11, 4, .)  // periods from -5 to +5
    
    * Extract coefficients for et_3 through et_13 (which correspond to -5 to +5)
    * Mapping: et_1 = -7, et_2 = -6, ..., et_7 = -1 (omitted), et_8 = 0, ..., et_17 = 9
    local row = 1
    forvalues i = 3/13 {
        if `i' == 7 {  // Reference period (event_time = -1, et_7 is omitted)
            matrix results[`row', 1] = -1   // Event time = -1
            matrix results[`row', 2] = 0    // Coefficient = 0
            matrix results[`row', 3] = 0    // Lower CI = 0
            matrix results[`row', 4] = 0    // Upper CI = 0
        }
        else {
            * Map et number to event time
            * et_1 = -7, et_2 = -6, ..., et_7 = -1, et_8 = 0, ..., et_17 = 9
            * So event_time = `i' - 8
            local event_time = `i' - 8
            
            matrix results[`row', 1] = `event_time'
            matrix results[`row', 2] = _b[et_`i'_X_delta]
            matrix results[`row', 3] = _b[et_`i'_X_delta] - 1.96*_se[et_`i'_X_delta]
            matrix results[`row', 4] = _b[et_`i'_X_delta] + 1.96*_se[et_`i'_X_delta]
        }
        local row = `row' + 1
    }
    
    preserve
        clear
        svmat results
        rename results1 event_time
        rename results2 coef
        rename results3 ci_lower
        rename results4 ci_upper
        
        * Clean version without title
        twoway (connected coef event_time, lcolor(navy) mcolor(navy) msymbol(O)) ///
               (rcap ci_lower ci_upper event_time, lcolor(navy) lpattern(dash)), ///
               xline(-0.5, lcolor(red) lpattern(dash)) ///
               yline(0, lcolor(black)) ///
               xlabel(-5(1)5, labsize(small)) ///
               ylabel(, angle(0) labsize(small)) ///
               xtitle("Waves Relative to Move", size(small)) ///
               ytitle("Effect on PFR Voting", size(small)) ///
               legend(off) ///
               graphregion(color(white)) ///
               plotregion(margin(medium)) ///
               scheme(s2mono)
        
        graph export "het_lowered_balanced.png", replace width(2400) height(1600)
    restore
}
// 			outreg2 using het_ed.doc, keep(et_*) word append se
	* A-level
		* Full panel 
		reghdfe vote_pfr et_* if edlevelH == 2 | mover == 0 [aweight=weight], /// 
		absorb(id wave pcon) cluster(id pcon)
			outreg2 using het_ed.doc, keep(et_*) word append se
		* Balanced Panel
		reghdfe vote_pfr et_* if edlevelH == 2 & move_gap == 1 & balanced == 1 | mover == 0 [aweight=weight], ///
		absorb(id wave pcon) cluster(id pcon)
			outreg2 using het_ed.doc, keep(et_*) word append se
	* Higher Education
		* Full panel 
		reghdfe vote_pfr et_* if edlevelH == 3 | mover == 0 [aweight=weight], /// 
		absorb(id wave pcon) cluster(id pcon)
			outreg2 using het_ed.doc, keep(et_*) word append se
		* Balanced Panel
		reghdfe vote_pfr et_* if edlevelH == 3 & move_gap == 1 & balanced == 1 | mover == 0 [aweight=weight], ///
		absorb(id wave pcon) cluster(id pcon)
			outreg2 using het_ed.doc, keep(et_*) word append se
//________________________________________________________________________________

* Home Ownership Own or Rent 
	gen owns_at_move = (rent_own == "Own") if mover == 1 & event_time == 0
	gen rents_at_move = (rent_own == "Rent") if mover == 1 & event_time == 0
	egen ownH = max(owns_at_move), by(id)
	egen rentH = max(rents_at_move), by(id)
	replace ownH = . if mover == 0
	replace rentH = . if mover == 0
	* Owners
		* Full panel 
		reghdfe vote_pfr et_* if ownH == 1 | mover == 0 [aweight=weight], /// 
		absorb(id wave pcon) cluster(id pcon)
			* Export
			outreg2 using het_vars.doc, ///
				keep(et_*) ///
				word replace se
		* Balanced Panel
		reghdfe vote_pfr et_* if ownH == 1 & move_gap == 1 & balanced == 1 | mover == 0 [aweight=weight], ///
		absorb(id wave pcon) cluster(id pcon)
			outreg2 using het_vars.doc, keep(et_*) word append se
	* Renters
		* Full panel 
		reghdfe vote_pfr et_* if rentH == 1 | mover == 0 [aweight=weight], /// 
		absorb(id wave pcon) cluster(id pcon)
			outreg2 using het_vars.doc, keep(et_*) word append se
		* Balanced Panel
		reghdfe vote_pfr et_* if rentH == 1 & move_gap == 1 & balanced == 1 | mover == 0 [aweight=weight], ///
		absorb(id wave pcon) cluster(id pcon)
			outreg2 using het_vars.doc, keep(et_*) word append se
//________________________________________________________________________________

* Income High or Low
	gen highinc_move = (inc_high == 1) if mover == 1 & event_time == 0
	gen lowinc_move = (inc_low == 1) if mover == 1 & event_time == 0
	egen highincH = max(highinc_move), by(id)
	egen lowincH = max(lowinc_move), by(id)
	replace highincH = . if mover == 0
	replace lowincH = . if mover == 0
	* High Income
		* Full panel 
		reghdfe vote_pfr et_* if highincH == 1 | mover == 0 [aweight=weight], /// 
		absorb(id wave pcon) cluster(id pcon)
			outreg2 using het_vars.doc, keep(et_*) word append se
		* Balanced Panel
		reghdfe vote_pfr et_* if highincH == 1 & move_gap == 1 & balanced == 1 | mover == 0 [aweight=weight], ///
		absorb(id wave pcon) cluster(id pcon)
			outreg2 using het_vars.doc, keep(et_*) word append se
	* Low Income
		* Full panel 
		reghdfe vote_pfr et_* if lowincH == 1 | mover == 0 [aweight=weight], /// 
		absorb(id wave pcon) cluster(id pcon)
			outreg2 using het_vars.doc, keep(et_*) word append se
		* Balanced Panel
		reghdfe vote_pfr et_* if lowincH == 1 & move_gap == 1 & balanced == 1 | mover == 0 [aweight=weight], ///
		absorb(id wave pcon) cluster(id pcon)
			outreg2 using het_vars.doc, keep(et_*) word append se
//________________________________________________________________________________

* Non-believers vs Christians
	gen christ_move = (rel_group == "Christian") if mover == 1 & event_time == 0
	gen nonbelief_move = (rel_group == "None") if mover == 1 & event_time == 0
	egen christH = max(christ_move), by(id)
	egen nonbeliefH = max(nonbelief_move), by(id)
	replace christH = . if mover == 0
	replace nonbeliefH = . if mover == 0
	* Christians
		* Full panel 
		reghdfe vote_pfr et_* if christH == 1 | mover == 0 [aweight=weight], /// 
		absorb(id wave pcon) cluster(id pcon)
			* Export
			outreg2 using het_vars2.doc, ///
				keep(et_*) ///
				word replace se
		* Balanced Panel
		reghdfe vote_pfr et_* if christH == 1 & move_gap == 1 & balanced == 1 | mover == 0 [aweight=weight], ///
		absorb(id wave pcon) cluster(id pcon)
			outreg2 using het_vars2.doc, keep(et_*) word append se
	* Non-Religious
		* Full panel 
		reghdfe vote_pfr et_* if nonbeliefH == 1 | mover == 0 [aweight=weight], /// 
		absorb(id wave pcon) cluster(id pcon)
			outreg2 using het_vars2.doc, keep(et_*) word append se
		* Balanced Panel
		reghdfe vote_pfr et_* if nonbeliefH == 1 & move_gap == 1 & balanced == 1 | mover == 0 [aweight=weight], ///
		absorb(id wave pcon) cluster(id pcon)
			outreg2 using het_vars2.doc, keep(et_*) word append se
//________________________________________________________________________________

* Immigration Attitudes </> 4
	gen immig_move = immigEcon if mover == 1 & event_time == 0
	egen immig_move_all = max(immig_move), by(id) // assign for all id observations
		gen immigH = .
		replace immigH = 1 if immig_move_all < 4
		replace immigH = 2 if immig_move_all > 4
		label define immigHlbl 1 "AntiMig" 2 "ProMig"
		label values immigH immigHlbl
	* Anti Immigration
		* Full panel 
		reghdfe vote_pfr et_* if immigH == 1 | mover == 0 [aweight=weight], /// 
		absorb(id wave pcon) cluster(id pcon)
			outreg2 using het_vars2.doc, keep(et_*) word append se
		* Balanced Panel
		reghdfe vote_pfr et_* if immigH == 1 & move_gap == 1 & balanced == 1 | mover == 0 [aweight=weight], ///
		absorb(id wave pcon) cluster(id pcon)
			outreg2 using het_vars2.doc, keep(et_*) word append se
	* Pro Immigration
		* Full panel 
		reghdfe vote_pfr et_* if immigH == 2 | mover == 0 [aweight=weight], /// 
		absorb(id wave pcon) cluster(id pcon)
			outreg2 using het_vars2.doc, keep(et_*) word append se
		* Balanced Panel
		reghdfe vote_pfr et_* if immigH == 2 & move_gap == 1 & balanced == 1 | mover == 0 [aweight=weight], ///
		absorb(id wave pcon) cluster(id pcon)
			outreg2 using het_vars2.doc, keep(et_*) word append se
//________________________________________________________________________________

* Con vs Labour Affiliated
	gen partyid_move = partyId if mover == 1 & event_time == 0
	egen partyid_move_all = max(partyid_move), by(id) // assign for all id observations
		gen partyidH = .
		replace partyidH = 1 if partyid_move_all == 1
		replace partyidH = 2 if partyid_move_all == 2
		label define partyidHlbl 1 "Con" 2 "Lab"
		label values partyidH partyidHlbl
	* Conservative
		* Full panel 
		reghdfe vote_pfr et_* if partyidH == 1 | mover == 0 [aweight=weight], /// 
		absorb(id wave pcon) cluster(id pcon)
			outreg2 using het_vars3.doc, ///
				keep(et_*) ///
				word replace se
		* Balanced Panel
		reghdfe vote_pfr et_* if partyidH == 1 & move_gap == 1 & balanced == 1 | mover == 0 [aweight=weight], ///
		absorb(id wave pcon) cluster(id pcon)
			outreg2 using het_vars3.doc, keep(et_*) word append se
	* Labour
		* Full panel 
		reghdfe vote_pfr et_* if partyidH == 2 | mover == 0 [aweight=weight], /// 
		absorb(id wave pcon) cluster(id pcon)
			outreg2 using het_vars3.doc, keep(et_*) word append se
		* Balanced Panel
		reghdfe vote_pfr et_* if partyidH == 2 & move_gap == 1 & balanced == 1 | mover == 0 [aweight=weight], ///
		absorb(id wave pcon) cluster(id pcon)
			outreg2 using het_vars3.doc, keep(et_*) word append se
			* Very significant no PFR, but do they become more likely to vote Labour then? 
		reghdfe lab_vote et_* if partyidH == 2 & move_gap == 1 & balanced == 1 | mover == 0 [aweight=weight], ///
		absorb(id wave pcon) cluster(id pcon)
			outreg2 using followup_het.doc, keep(et_*) word append se
//________________________________________________________________________________

* Female
	* Full panel
		reghdfe vote_pfr et_* if mover == 1 & female == 1 | mover == 0 [aweight=weight], /// 
		absorb(id wave pcon) cluster(id pcon)
			outreg2 using het_vars3.doc, keep(et_*) word append se
	* Balanced Panel
		reghdfe vote_pfr et_* if (mover == 1 & female == 1 & move_gap == 1 & balanced == 1) | mover == 0 [aweight=weight], ///
		absorb(id wave pcon) cluster(id pcon)
			outreg2 using het_vars3.doc, keep(et_*) word append se
			
	* Male for double-check
	* Full panel
		reghdfe vote_pfr et_* if mover == 1 & female == 0 | mover == 0 [aweight=weight], /// 
		absorb(id wave pcon) cluster(id pcon)
			outreg2 using het_vars3.doc, keep(et_*) word append se
	* Balanced Panel
		reghdfe vote_pfr et_* if (mover == 1 & female == 0 & move_gap == 1 & balanced == 1) | mover == 0 [aweight=weight], ///
		absorb(id wave pcon) cluster(id pcon)
			outreg2 using het_vars3.doc, keep(et_*) word append se
//________________________________________________________________________________

* Addition: Trust in MPs </> 4
	gen trust_move = trustMPs if mover == 1 & event_time == 0
	egen trust_move_all = max(trust_move), by(id) // assign for all id observations
		gen trustH = .
		replace trustH = 1 if trust_move_all < 4
		replace trustH = 2 if trust_move_all > 4
		label define trustHlbl 1 "LowTrust" 2 "HighTrust"
		label values trustH trustHlbl
	* Low Trust
		* Full panel 
		reghdfe vote_pfr et_* if trustH == 1 | mover == 0 [aweight=weight], /// 
		absorb(id wave pcon) cluster(id pcon)
			outreg2 using het_vars_add.doc, keep(et_*) word replace se
		* Balanced Panel
		reghdfe vote_pfr et_* if trustH == 1 & move_gap == 1 & balanced == 1 | mover == 0 [aweight=weight], ///
		absorb(id wave pcon) cluster(id pcon)
			outreg2 using het_vars_add.doc, keep(et_*) word append se
	* High Trust
		* Full panel 
		reghdfe vote_pfr et_* if trustH == 2 | mover == 0 [aweight=weight], /// 
		absorb(id wave pcon) cluster(id pcon)
			outreg2 using het_vars_add.doc, keep(et_*) word append se
		* Balanced Panel
		reghdfe vote_pfr et_* if trustH == 2 & move_gap == 1 & balanced == 1 | mover == 0 [aweight=weight], ///
		absorb(id wave pcon) cluster(id pcon)
			outreg2 using het_vars_add.doc, keep(et_*) word append se
//________________________________________________________________________________

* Addition Economic Situation </> 3
	gen econ_move = econPersonalRetro if mover == 1 & event_time == 0
	egen econ_move_all = max(econ_move), by(id) // assign for all id observations
		gen econH = .
		replace econH = 1 if econ_move_all < 3
		replace econH = 2 if econ_move_all > 3
		label define econHlbl 1 "Worse" 2 "Better"
		label values econH econHlbl
	* Worse
		* Full panel 
		reghdfe vote_pfr et_* if econH == 1 | mover == 0 [aweight=weight], /// 
		absorb(id wave pcon) cluster(id pcon)
			outreg2 using het_vars_add.doc, keep(et_*) word append se
		* Balanced Panel
		reghdfe vote_pfr et_* if econH == 1 & move_gap == 1 & balanced == 1 | mover == 0 [aweight=weight], ///
		absorb(id wave pcon) cluster(id pcon)
			outreg2 using het_vars_add.doc, keep(et_*) word append se
	* Better
		* Full panel 
		reghdfe vote_pfr et_* if econH == 2 | mover == 0 [aweight=weight], /// 
		absorb(id wave pcon) cluster(id pcon)
			outreg2 using het_vars_add.doc, keep(et_*) word append se
		* Balanced Panel
		reghdfe vote_pfr et_* if econH == 2 & move_gap == 1 & balanced == 1 | mover == 0 [aweight=weight], ///
		absorb(id wave pcon) cluster(id pcon)
			outreg2 using het_vars_add.doc, keep(et_*) word append se
//________________________________________________________________________________

* Addition Redistribution </> 5
	gen red_move = redSelf_rev if mover == 1 & event_time == 0
	egen red_move_all = max(red_move), by(id) // assign for all id observations
		gen redH = .
		replace redH = 1 if red_move_all < 5
		replace redH = 2 if red_move_all > 5
		label define redHlbl 1 "Negative" 2 "Positive"
		label values redH redHlbl
	* Worse
		* Full panel 
		reghdfe vote_pfr et_* if redH == 1 | mover == 0 [aweight=weight], /// 
		absorb(id wave pcon) cluster(id pcon)
			outreg2 using het_vars_add2.doc, keep(et_*) word replace se
		* Balanced Panel
		reghdfe vote_pfr et_* if redH == 1 & move_gap == 1 & balanced == 1 | mover == 0 [aweight=weight], ///
		absorb(id wave pcon) cluster(id pcon)
			outreg2 using het_vars_add2.doc, keep(et_*) word append se
	* Better
		* Full panel 
		reghdfe vote_pfr et_* if redH == 2 | mover == 0 [aweight=weight], /// 
		absorb(id wave pcon) cluster(id pcon)
			outreg2 using het_vars_add2.doc, keep(et_*) word append se
		* Balanced Panel
		reghdfe vote_pfr et_* if redH == 2 & move_gap == 1 & balanced == 1 | mover == 0 [aweight=weight], ///
		absorb(id wave pcon) cluster(id pcon)
			outreg2 using het_vars_add2.doc, keep(et_*) word append se
//________________________________________________________________________________

* -----------------------------------------------------------------
* Mover Counts for Heterogeneity Analysis
* -----------------------------------------------------------------

* Local macro for storing
local count_results ""

* AGE GROUPS
* 18-29
qui count if agegroupH == 1 & mover == 1 & event_time == 0
local count_age1_full = r(N)
qui count if agegroupH == 1 & mover == 1 & event_time == 0 & move_gap == 1 & balanced == 1
local count_age1_bal = r(N)

* 30-44
qui count if agegroupH == 2 & mover == 1 & event_time == 0
local count_age2_full = r(N)
qui count if agegroupH == 2 & mover == 1 & event_time == 0 & move_gap == 1 & balanced == 1
local count_age2_bal = r(N)

* 45-60
qui count if agegroupH == 3 & mover == 1 & event_time == 0
local count_age3_full = r(N)
qui count if agegroupH == 3 & mover == 1 & event_time == 0 & move_gap == 1 & balanced == 1
local count_age3_bal = r(N)

* 60+
qui count if agegroupH == 4 & mover == 1 & event_time == 0
local count_age4_full = r(N)
qui count if agegroupH == 4 & mover == 1 & event_time == 0 & move_gap == 1 & balanced == 1
local count_age4_bal = r(N)

* EDUCATION
* Below A-level
qui count if edlevelH == 1 & mover == 1 & event_time == 0
local count_ed1_full = r(N)
qui count if edlevelH == 1 & mover == 1 & event_time == 0 & move_gap == 1 & balanced == 1
local count_ed1_bal = r(N)

* A-level
qui count if edlevelH == 2 & mover == 1 & event_time == 0
local count_ed2_full = r(N)
qui count if edlevelH == 2 & mover == 1 & event_time == 0 & move_gap == 1 & balanced == 1
local count_ed2_bal = r(N)

* Higher Ed
qui count if edlevelH == 3 & mover == 1 & event_time == 0
local count_ed3_full = r(N)
qui count if edlevelH == 3 & mover == 1 & event_time == 0 & move_gap == 1 & balanced == 1
local count_ed3_bal = r(N)

* HOME OWNERSHIP
* Owners
qui count if ownH == 1 & mover == 1 & event_time == 0
local count_own_full = r(N)
qui count if ownH == 1 & mover == 1 & event_time == 0 & move_gap == 1 & balanced == 1
local count_own_bal = r(N)

* Renters
qui count if rentH == 1 & mover == 1 & event_time == 0
local count_rent_full = r(N)
qui count if rentH == 1 & mover == 1 & event_time == 0 & move_gap == 1 & balanced == 1
local count_rent_bal = r(N)

* INCOME
* High Income
qui count if highincH == 1 & mover == 1 & event_time == 0
local count_highinc_full = r(N)
qui count if highincH == 1 & mover == 1 & event_time == 0 & move_gap == 1 & balanced == 1
local count_highinc_bal = r(N)

* Low Income
qui count if lowincH == 1 & mover == 1 & event_time == 0
local count_lowinc_full = r(N)
qui count if lowincH == 1 & mover == 1 & event_time == 0 & move_gap == 1 & balanced == 1
local count_lowinc_bal = r(N)

* RELIGION
* Christians
qui count if christH == 1 & mover == 1 & event_time == 0
local count_christ_full = r(N)
qui count if christH == 1 & mover == 1 & event_time == 0 & move_gap == 1 & balanced == 1
local count_christ_bal = r(N)

* Non-Religious
qui count if nonbeliefH == 1 & mover == 1 & event_time == 0
local count_nonbelief_full = r(N)
qui count if nonbeliefH == 1 & mover == 1 & event_time == 0 & move_gap == 1 & balanced == 1
local count_nonbelief_bal = r(N)

* IMMIGRATION ATTITUDES
* Anti Immigration
qui count if immigH == 1 & mover == 1 & event_time == 0
local count_antimig_full = r(N)
qui count if immigH == 1 & mover == 1 & event_time == 0 & move_gap == 1 & balanced == 1
local count_antimig_bal = r(N)

* Pro Immigration
qui count if immigH == 2 & mover == 1 & event_time == 0
local count_promig_full = r(N)
qui count if immigH == 2 & mover == 1 & event_time == 0 & move_gap == 1 & balanced == 1
local count_promig_bal = r(N)

* TRUST
* Low Trust
qui count if trustH == 1 & mover == 1 & event_time == 0
local count_lowtrust_full = r(N)
qui count if trustH == 1 & mover == 1 & event_time == 0 & move_gap == 1 & balanced == 1
local count_lowtrust_bal = r(N)

* High Trust
qui count if trustH == 2 & mover == 1 & event_time == 0
local count_hightrust_full = r(N)
qui count if trustH == 2 & mover == 1 & event_time == 0 & move_gap == 1 & balanced == 1
local count_hightrust_bal = r(N)

* ECONOMIC RETROSPECT
* Worse
qui count if econH == 1 & mover == 1 & event_time == 0
local count_econworse_full = r(N)
qui count if econH == 1 & mover == 1 & event_time == 0 & move_gap == 1 & balanced == 1
local count_econworse_bal = r(N)

* Better
qui count if econH == 2 & mover == 1 & event_time == 0
local count_econbetter_full = r(N)
qui count if econH == 2 & mover == 1 & event_time == 0 & move_gap == 1 & balanced == 1
local count_econbetter_bal = r(N)

* REDISTRIBUTION
* Negative
qui count if redH == 1 & mover == 1 & event_time == 0
local count_redneg_full = r(N)
qui count if redH == 1 & mover == 1 & event_time == 0 & move_gap == 1 & balanced == 1
local count_redneg_bal = r(N)

* Positive
qui count if redH == 2 & mover == 1 & event_time == 0
local count_redpos_full = r(N)
qui count if redH == 2 & mover == 1 & event_time == 0 & move_gap == 1 & balanced == 1
local count_redpos_bal = r(N)

* PARTY ID
* Conservative
qui count if partyidH == 1 & mover == 1 & event_time == 0
local count_con_full = r(N)
qui count if partyidH == 1 & mover == 1 & event_time == 0 & move_gap == 1 & balanced == 1
local count_con_bal = r(N)

* Labour
qui count if partyidH == 2 & mover == 1 & event_time == 0
local count_lab_full = r(N)
qui count if partyidH == 2 & mover == 1 & event_time == 0 & move_gap == 1 & balanced == 1
local count_lab_bal = r(N)

* GENDER
* Female
qui count if female == 1 & mover == 1 & event_time == 0
local count_female_full = r(N)
qui count if female == 1 & mover == 1 & event_time == 0 & move_gap == 1 & balanced == 1
local count_female_bal = r(N)

* Male
qui count if female == 0 & mover == 1 & event_time == 0
local count_male_full = r(N)
qui count if female == 0 & mover == 1 & event_time == 0 & move_gap == 1 & balanced == 1
local count_male_bal = r(N)

* Export
file open myfile using "mover_counts.txt", write replace
file write myfile "Heterogeneity Group,Full Panel,Balanced Panel" _n
file write myfile "Age 18-29,`count_age1_full',`count_age1_bal'" _n
file write myfile "Age 30-44,`count_age2_full',`count_age2_bal'" _n
file write myfile "Age 45-60,`count_age3_full',`count_age3_bal'" _n
file write myfile "Age 60+,`count_age4_full',`count_age4_bal'" _n
file write myfile "Below A-level,`count_ed1_full',`count_ed1_bal'" _n
file write myfile "A-level,`count_ed2_full',`count_ed2_bal'" _n
file write myfile "Higher Ed,`count_ed3_full',`count_ed3_bal'" _n
file write myfile "Owners,`count_own_full',`count_own_bal'" _n
file write myfile "Renters,`count_rent_full',`count_rent_bal'" _n
file write myfile "High Income,`count_highinc_full',`count_highinc_bal'" _n
file write myfile "Low Income,`count_lowinc_full',`count_lowinc_bal'" _n
file write myfile "Christians,`count_christ_full',`count_christ_bal'" _n
file write myfile "Non-Religious,`count_nonbelief_full',`count_nonbelief_bal'" _n
file write myfile "Anti-Immigration,`count_antimig_full',`count_antimig_bal'" _n
file write myfile "Pro-Immigration,`count_promig_full',`count_promig_bal'" _n
file write myfile "Low Trust,`count_lowtrust_full',`count_lowtrust_bal'" _n
file write myfile "High Trust,`count_hightrust_full',`count_hightrust_bal'" _n
file write myfile "Econ Worse,`count_econworse_full',`count_econworse_bal'" _n
file write myfile "Econ Better,`count_econbetter_full',`count_econbetter_bal'" _n
file write myfile "Red Neg,`count_redneg_full',`count_redneg_bal'" _n
file write myfile "Red Pos,`count_redpos_full',`count_redpos_bal'" _n
file write myfile "Conservative,`count_con_full',`count_con_bal'" _n
file write myfile "Labour,`count_lab_full',`count_lab_bal'" _n
file write myfile "Female,`count_female_full',`count_female_bal'" _n
file write myfile "Male,`count_male_full',`count_male_bal'" _n
file close myfile

* -----------------------------------------------------------------
* Mover Counts for Robustness Checks
* -----------------------------------------------------------------

* 1. Land Registry Data Only (zoopla_id == 0)
qui count if zoopla_id == 0 & mover == 1 & event_time == 0
local count_landreg_full = r(N)
qui count if zoopla_id == 0 & mover == 1 & event_time == 0 & move_gap == 1 & balanced == 1
local count_landreg_bal = r(N)

* 2. Higher inequality (delta_gini > 0)
qui count if delta_gini > 0 & mover == 1 & event_time == 0
local count_higherineq_full = r(N)
qui count if delta_gini > 0 & mover == 1 & event_time == 0 & move_gap == 1 & balanced == 1
local count_higherineq_bal = r(N)

* 3. Lower inequality (delta_gini < 0)
qui count if delta_gini < 0 & mover == 1 & event_time == 0
local count_lowerineq_full = r(N)
qui count if delta_gini < 0 & mover == 1 & event_time == 0 & move_gap == 1 & balanced == 1
local count_lowerineq_bal = r(N)

* 4a. Above median absolute inequality changes (abs_delta_gini > 0.0334603)
qui count if abs_delta_gini > 0.0334603 & mover == 1 & event_time == 0
local count_abovemed_full = r(N)
qui count if abs_delta_gini > 0.0334603 & mover == 1 & event_time == 0 & move_gap == 1 & balanced == 1
local count_abovemed_bal = r(N)

* 4b. Top 25% absolute inequality changes (abs_delta_gini > 0.0599993)
qui count if abs_delta_gini > 0.0599993 & mover == 1 & event_time == 0
local count_top25_full = r(N)
qui count if abs_delta_gini > 0.0599993 & mover == 1 & event_time == 0 & move_gap == 1 & balanced == 1
local count_top25_bal = r(N)

* Export to file
file open robustnesscount using "robustness_counts.txt", write replace
file write robustnesscount "Robustness Check,Full Panel,Balanced Panel" _n
file write robustnesscount "Land Registry Only,`count_landreg_full',`count_landreg_bal'" _n
file write robustnesscount "Higher Inequality,`count_higherineq_full',`count_higherineq_bal'" _n
file write robustnesscount "Lower Inequality,`count_lowerineq_full',`count_lowerineq_bal'" _n
file write robustnesscount "Above Median Change,`count_abovemed_full',`count_abovemed_bal'" _n
file write robustnesscount "Top 25% Change,`count_top25_full',`count_top25_bal'" _n
file close robustnesscount

*** Further Robustness ***
	*Get mover count for those that have values for redistributive attitudes at time of move
	count if mover == 1 & event_time == 0 & !missing(redSelf_rev) // 3,759
	sum redSelf_rev if mover == 1 & event_time == 0 & !missing(redSelf_rev)

// // Check whether LSOA can offer more granular analysis
// count if pcon_change == 1 // 4,831
// count if lsoa_change == 1 // 4,152, what?
// // Check: oslaua variable in BES data is NOT LSOA but ONS Local Authority District, NOT more granular. 



