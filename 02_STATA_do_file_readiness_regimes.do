
******************************************************		
**************/* Markov on imputed data */************
******************************************************

* Load data
use "/Readiness_Regimes_1.dta", clear

* Transition Matrix 
bysort country (year): gen cluster_lag = cluster_nr[_n-1]  
tab cluster_lag cluster_nr

asdoc markov cluster_nr, save(M) replace
tab cluster_lag cluster_nr, row nofreq


******************************************************		
*********/* Modelling */****
******************************************************

* =========================
* Load data for Modelling (not imputed)
* =========================

use "/Readiness_regimes_2.dta", clear

* =========================
* Panel definition
* =========================

encode country, gen(country_id)
xtset country_id year


* =========================
* Dependent variable
* =========================

/** Regime ***/
gen regime = cluster_nr_
label define regime_lbl 1 "Outliers" ///
                        2 "Followers" ///
                        3 "Leaders"
label values regime regime_lbl
gen regime_lag = L.regime
gen gap = year - L.year
replace regime_lag = . if gap != 1
drop gap

/** Robustness: Transition up ***/
gen transition_anyup = (regime > regime_lag) if !missing(regime, regime_lag)
label var transition_anyup "Any upward transition (1->2,2->3,1->3) t-1->t"


* =========================
* Main regressors: lagged levels
* =========================

gen l_skills = L1.ftri_skills
gen l_reg_ict = L1.reg_ict
gen l_innov_ht = L1.hittech_exp
gen l_innov_rd = L1.ftri_rd
gen l_connect_int = L1.int_users
gen l_connect_bb = L1.broad_pop

egen z_l_skills = std(l_skills)
egen z_l_reg_ict = std(l_reg_ict)
egen z_l_innov_ht = std(l_innov_ht)
egen z_l_innov_rd = std(l_innov_rd)
egen z_l_connect_int = std(l_connect_int)
egen z_l_connect_bb = std(l_connect_bb)


* =========================
* Robustness: differences t-1 to t
* =========================

gen d_skills_t = ftri_skills - L1.ftri_skills
gen d_reg_env_t = reg_ict - L1.reg_ict
gen d_innov_ht_t = hittech_exp - L1.hittech_exp
gen d_connect_int_t = int_users - L1.int_users
gen d_connect_bb_t = broad_pop - L1.broad_pop

egen z_skills_t = std(d_skills_t)
egen z_reg_env_t    = std(d_reg_env_t)
egen z_innov_ht_t = std(d_innov_ht_t)
egen z_conn_int_t = std(d_connect_int_t)
egen z_conn_bb_t = std(d_connect_bb_t)


* =========================
* Robustness: differences t-2 to t-1
* =========================

gen d_skills = L1.ftri_skills - L2.ftri_skill
gen d_reg_env = L1.reg_ict - L2.reg_ict
gen d_innov_rd = L1.ftri_rd - L2.ftri_rd
gen d_innov_ht = L1.hittech_exp- L2.hittech_exp
gen d_connect_bb = L1.broad_pop - L2.broad_pop
gen d_connect_int = L1.int_users - L2.int_users

egen z_skills = std(d_skills)
egen z_reg_env    = std(d_reg_env)
egen z_innov_rd = std(d_innov_rd)
egen z_innov_ht = std(d_innov_ht)
egen z_conn_bb = std(d_connect_bb)
egen z_conn_int = std(d_connect_int)


* =========================
* Correlations
* =========================

asdoc pwcorr ftri_ict ftri_skills ftri_rd ftri_indact ftri_accfin ftri_overall reg_ict hittech_exp semicond_share serv_va_pop, save(correlations) replace

asdoc pwcorr z_skills z_reg_env z_conn_bb z_conn_int z_innov_rd z_innov_ht, save(correlations)


* =========================
* Main models: multinomial logit
* =========================


asdoc mlogit regime z_l_skills z_l_reg_ict z_l_connect_int z_l_innov_ht i.year, 	baseoutcome(1) vce(cluster country_id) save(m_clust) replace
est store model_l1

asdoc mlogit regime z_l_skills z_l_reg_ict z_l_connect_bb z_l_innov_ht i.year, baseoutcome(1) vce(cluster country_id) save(m_clust) 
est store model_l2

asdoc mlogit regime z_l_skills z_l_reg_ict z_l_connect_int z_l_innov_rd i.year, baseoutcome(1) vce(cluster country_id) save(m_clust) 
est store model_l3

asdoc mlogit regime z_l_skills z_l_reg_ict z_l_connect_bb z_l_innov_rd i.year, baseoutcome(1) vce(cluster country_id) save(m_clust) 
est store model_l4


etable, estimates(model_l1 model_l2 model_l3 model_l4) showstars showstarsnote title("Table 1. Multinomial logit models") export(mydoc.docx, replace)

* =========================
* Main models: multinomial logit
* Marginal Effects
* =========================

asdoc margins, dydx(z_l_skills z_l_reg_ict z_l_connect_int z_l_innov_ht) predict(outcome(2)), save(margeff) replace
est store marg11
asdoc margins, dydx(z_l_skills z_l_reg_ict z_l_connect_int z_l_innov_ht) predict(outcome(3)), save(margeff) 
est store marg12

asdoc margins, dydx(z_l_skills z_l_reg_ict z_l_connect_bb z_l_innov_ht) predict(outcome(2)), save(margeff) 
est store marg21
asdoc margins, dydx(z_l_skills z_l_reg_ict z_l_connect_bb z_l_innov_ht) predict(outcome(3)), save(margeff) 
est store marg22

asdoc margins, dydx(z_l_skills z_l_reg_ict z_l_connect_int z_l_innov_rd) predict(outcome(2)), save(margeff) 
est store marg31
asdoc margins, dydx(z_l_skills z_l_reg_ict z_l_connect_int z_l_innov_rd) predict(outcome(3)), save(margeff) 
est store marg32

asdoc margins, dydx(z_l_skills z_l_reg_ict z_l_connect_bb z_l_innov_rd) predict(outcome(2)), save(margeff) 
est store marg41
asdoc margins, dydx(z_l_skills z_l_reg_ict z_l_connect_bb z_l_innov_rd) predict(outcome(3)), save(margeff) 
est store marg42

etable, estimates(marg11 marg12 marg21 marg22 marg31 marg32 marg41 marg42) showstars showstarsnote title("") export(mydoc.docx, replace)

* =========================
* Robustness: Logit for upward transition
* =========================

asdoc logit transition_anyup z_l_skills z_l_reg_ict z_l_connect_int z_l_innov_ht i.year, vce(cluster country_id), save(logit1) replace
est store log1

asdoc logit transition_anyup z_l_skills z_l_reg_ict z_l_connect_bb z_l_innov_ht i.year, vce(cluster country_id), save(logit1) 
est store log2

etable, estimates(log1 log2) showstars showstarsnote title("") export(mydoc.docx, replace)

* =========================
* Robustness: Ordered Logit 
* =========================

asdoc ologit regime z_l_skills z_l_reg_ict z_l_connect_int z_l_innov_ht i.year, vce(cluster country_id), save(logit1) replace
est store olog1

asdoc ologit regime z_l_skills z_l_reg_ict z_l_connect_bb z_l_innov_ht i.year, vce(cluster country_id), save(logit1) 
est store olog2


etable, estimates(olog1 olog2) showstars showstarsnote title("") export(mydoc.docx, replace)

* =========================
* Robustness: Robustness: t-1 -> t
* Multinomial logit (mlogit)
* =========================

asdoc mlogit regime z_skills_t z_reg_env_t z_conn_int_t z_innov_ht_t i.year, baseoutcome(1) vce(cluster country_id) save(m_clust) replace
est store model_t11

asdoc mlogit regime z_skills_t z_reg_env_t z_conn_bb_t z_innov_ht_t i.year, baseoutcome(1) vce(cluster country_id) save(m_clust) 
est store model_t12

etable, estimates(model_t11 model_t12) showstars showstarsnote title("Table 1. Multinomial logit models") export(mydoc.docx, replace)

* =========================
* Robustness: Robustness: t-2 -> t-1
* Multinomial logit (mlogit)
* =========================

asdoc mlogit regime z_skills z_reg_env z_conn_int z_innov_ht i.year, baseoutcome(1) vce(cluster country_id) save(m_clust) 
est store model1

asdoc mlogit regime z_skills z_reg_env z_conn_bb z_innov_ht i.year, baseoutcome(1) vce(cluster country_id) save(m_clust) 
est store model2

asdoc mlogit regime z_skills z_reg_env z_conn_int z_innov_rd i.year, baseoutcome(1) vce(cluster country_id) save(m_clust) 
est store model3

asdoc mlogit regime z_skills z_reg_env z_conn_bb z_innov_rd i.year, baseoutcome(1) vce(cluster country_id) save(m_clust) replace
est store model4

* =========================
* Robustness: Main model without time-dummies
* Multinomial logit (mlogit)
* =========================

asdoc mlogit regime z_l_skills z_l_reg_ict z_l_connect_int z_l_innov_ht , baseoutcome(1) vce(cluster country_id) save(m_clust) replace
est store model_l1

asdoc mlogit regime z_l_skills z_l_reg_ict z_l_connect_bb z_l_innov_ht , baseoutcome(1) vce(cluster country_id) save(m_clust) 
est store model_l2

asdoc mlogit regime z_l_skills z_l_reg_ict z_l_connect_int z_l_innov_rd , baseoutcome(1) vce(cluster country_id) save(m_clust) 
est store model_l3

asdoc mlogit regime z_l_skills z_l_reg_ict z_l_connect_bb z_l_innov_rd , baseoutcome(1) vce(cluster country_id) save(m_clust) 
est store model_l4


etable, estimates(model_l1 model_l2 model_l3 model_l4) showstars showstarsnote title("Table 1. Multinomial logit models") export(mydoc.docx, replace)



