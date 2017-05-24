//cd /Users/vivalt/Downloads/mm_followup/

/*
clear
import delimited "/Users/vivalt/Downloads/all_ids.csv", encoding(ISO-8859-1)
rename ans mturkcode
sort mturkcode
save all_ids, replace
*/

clear
set memory 1G
insheet using "../data/raw/Animal_advocacy_final__3x3_v2.csv"

/*
rename q201 zip
sort zip
merge zip using zip
keep if _m==1|_m==3
*/

// Data cleaning

// Swapping signs for easier interpretation
foreach i in 21 175 99{
replace q`i'=-q`i'
replace q`i'=q`i'+8
}

foreach i in 23 25 27 29 31 33 177 179 181 185 187 189 101 103{
replace q`i'=-q`i'
replace q`i'=q`i'+6
}

// Defining treatment groups

gen none=(q355!=.|q357!=.|q359!=.)
gen mm=(q41!=.|q59!=.|q77!=.)
gen veg=(mm!=1&none!=1)

label var mm "Cultured meat"
label var veg "Vegetarian substitute"
label var none "No new product"

gen health=(q41!=.|q51!=.|q355!=.)
gen enviro=(q59!=.|q69!=.|q357!=.)
gen ethical=(q77!=.|q87!=.|q359!=.)

label var health "Health message"
label var enviro "Environmental message"
label var ethical "Ethical message"

// Defining variables

gen ethicalveg=q175-q21

gen sentient=q177-q23
gen pain=q179-q25
gen importance=q181-q27

gen revealedweights=q183

/*
label var ethicalveg "Eating vegetarian food is morally preferable"
label var sentient "Farm animals are sentient"
label var pain "How much factory farming harms animals"
label var importance "How much do you care about this harm"
label var revealedweights "Donation to charity"
*/

label var ethicalveg "Morally preferable"
label var sentient "Sentient"
label var pain "How much harm to animals"
label var importance "Importance of harm to animals"
label var revealedweights "Donation"

gen enviroimpact=(q185-q29)
gen howmuchenviroimpact=(q187-q31)
gen importanceenviroimpact=(q189-q31)

label var enviroimpact "Harm to environment"
label var howmuchenviroimpact "How much harm to environment"
label var importanceenviroimpact "Importance of harm to environment"

gen expectedconsumption=q191
gen howmuchexpected=q193
gen moreinfo=(q197==1|q197==2) if mm==1|veg==1
gen moreinfoveg=(q195==1|q195==2)

label var expectedconsumption "Expects to reduce consumption"
label var howmuchexpected "How much expects to reduce consumption"
label var moreinfo "Info: New product"
label var moreinfoveg "Info: Vegetarian options"

gen wtptoavoid=q169
replace wtptoavoid=0 if q167==2

label var wtptoavoid "WTP to avoid information"

cap replace q111_1=v456 if q111_1==.
cap replace q111_2=v457 if q111_2==.

label var q99 "How do you feel about this product?"
label var q101 "How interested are you in this product?"
label var q103 "Would you eat this product?"
label var q111_1 "Ease of reducing meat consumption by 100\%"
label var q111_2 "Ease of reducing meat consumption by 25\%"

gen positivity=q99+q101+q103
gen easiness=q111_1+q111_2

label var positivity "Positivity index"
label var easiness "Easiness index"

forvalues i=2/7{
gen multiplier`i'=.
replace multiplier`i'=120 if q5_`i'==7
replace multiplier`i'=75 if q5_`i'==6
replace multiplier`i'=30 if q5_`i'==5
replace multiplier`i'=21 if q5_`i'==4
replace multiplier`i'=9 if q5_`i'==3
replace multiplier`i'=2 if q5_`i'==2
replace multiplier`i'=0 if q5_`i'==1
gen weighted`i'=q5_`i'*multiplier`i'
}
egen rowtotalmeatw=rowtotal(weighted2 weighted3 weighted4 weighted5 weighted6 weighted7)
replace rowtotalmeatw=rowtotalmeatw/30

label var rowtotalmeatw "Baseline meat consumption"

gen alreadyveg=(q11_8==1|q11_9==1)
cap gen wantstoreduce=(q7_5==1)
gen specialdiet=(q11_1==1|q11_2==1|q11_6==1)

label var alreadyveg "Already vegetarian/vegan"
cap label var wantstoreduce "Wants to reduce meat consumption"
label var specialdiet "Higher-meat diet"

gen doesntunderstand=(q115==3|q141==3)
gen partialunderstand=(q115==3|q115==2|q141==3|q141==2)

label var doesntunderstand "Does not understand"
label var partialunderstand "Partially understands"

gen timing=q172_3
gen novideo=q173
gen didntwatch=(q172_3<72)

gen age=q203
gen gender=q205
recode gender 3=.
gen educ=q207
gen income=q209
gen politics=q211
gen religion=q213
gen urban2=(urban==2|urban==3) if urban!=.
gen christian=(religion==1|religion==2|religion==3|religion==4)
gen jewish=(religion==5)
gen muslim=(religion==6)
gen buddhist=(religion==7)
gen hindu=(religion==8)
gen atheist=(religion==9|religion==10|religion==11)
gen other=(religion==12)
// including hindus in other as they are too small:
gen other2=(religion==8|religion==12)
gen mormon=(religion==3)
// including jews and hindus in other as they are too small:
gen other3=(religion==5|religion==8|religion==12)

label var age "Age"
label var gender "Gender"
label var educ "Education level"
label var income "Income level"
label var politics "Political orientation"
label var urban "Urban index"
label var urban "Urban"
label var christian "Christian"
label var jewish "Jewish"
label var muslim "Muslim"
label var buddhist "Buddhist"
label var atheist "Atheist/Agnostic"
label var other "Other"
label var other2 "Other"
label var mormon "Mormon"
label var other3 "Other"

drop if alreadyveg==1
drop if novideo==1
drop if didntwatch==1
keep if partialunderstand==0

gen diff1=q111_1-q37_1
gen diff2=q111_2-q37_2
gen diffindex=diff1+diff2

ttest diffindex if none==0, by(mm)
ttest diffindex if none==0, by(veg)
ttest diffindex, by(none)

label var diffindex "Changes in easiness index"

cap gen healthscale=(q9_4==1|q9_5==1)

cap label var healthscale "Health problems"

gen util1=1 if q3_5>q3_10
gen util2=1 if q3_7>=3
gen util3=1 if q3_6>=3
gen util4=1 if q3_12>q3_8

egen utilscale=rowtotal(util1 util2 util3 util4)

label var utilscale "Utilitarian index"

gen natbase1=(q109_5==1)
gen natbase2=(q109_1==1|q109_2==1|q109_5==1)

drop _m
sort mturkcode
/*
merge mturkcode using all_ids
keep if _m==3
drop _m
sort workerid

save baseline_nat, replace
*/

gen ethframing=(q361!=.)
gen procframing=(q396!=.)

reg sentient rowtotalmeatw q23 ethframing procframing if partialunderstand==0, r
eststo r1
reg pain rowtotalmeatw q25 ethframing procframing if partialunderstand==0, r
eststo r2
reg importance rowtotalmeatw q27 ethframing procframing if partialunderstand==0, r
eststo r3
reg enviroimpact rowtotalmeatw q29 ethframing procframing if partialunderstand==0, r
eststo r4
reg howmuchenv rowtotalmeatw q31 ethframing procframing if partialunderstand==0, r
eststo r5
reg importanceenv rowtotalmeatw q33 ethframing procframing if partialunderstand==0, r
eststo r6

esttab r1 r2 r3 r4 r5 r6 using mm_framing.tex, cells(b(star fmt(3)) se(par fmt(2))) starlevels(* 0.05 ** 0.01 *** 0.001) r2(2) style(tex) replace label varlabels(_cons Constant) booktabs wrap title(Ordinary Least Squares Regression of Belief Change on Treatments)

outsheet ../data/cleaned/naturalistic_fallacy_3x3_cleaned.csv, comma
