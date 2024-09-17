infile using "C:\USERS\21429\ICF\DATA ARCHIVE - DOCUMENTS\EXPORT MENU\NP81\NPFW81FL.dct"

label variable fw101    "Fieldworker code"
label variable fw000    "Country code and phase"
label variable fw102    "Fieldworker region of residence"
label variable fw103    "Fieldworker type of place of residence"
label variable fw104    "Fieldworker age"
label variable fw105    "Fieldworker sex"
label variable fw106    "Fieldworker marital status"
label variable fw107    "Fieldworker number of living children"
label variable fw108    "Fieldworker ever had a child who died"
label variable fw110    "Highest grade completed by fieldworker"
label variable fw111    "Fieldworker religion"
label variable fw112    "Fieldworker ethnicity"
label variable fw113a   "Nepali"
label variable fw113b   "Maithali"
label variable fw113c   "Bhojpuri"
label variable fw113d   "Tharu"
label variable fw113e   "Newari"
label variable fw113f   "Tamang"
label variable fw113g   "Abadhi"
label variable fw113x   "Other language"
label variable fw114    "Fieldworker's mother tongue/native language"
label variable fw115a   "Has fieldworker ever worked on a DHS prior to this survey"
label variable fw115c   "Has fieldworker ever worked on any other survey prior to this survey"
label variable fw116    "Was fieldworker working for implementing agency at the time employed for DHS"
label variable fw117    "Is fieldworker a permanent or temporary employee of agency"
label variable fw118    "Does fieldworker has any comments"

#delimit ;
label define FW102   
     1 "Koshi province"
     2 "Madhesh province"
     3 "Bagmati province"
     4 "Gandaki province"
     5 "Lumbini province"
     6 "Karnali province"
     7 "Sudurpacschim province"
;
label define FW103   
     1 "City (Metro/Sub-metro)"
     2 "Town (Municipality)"
     3 "Rural (Gaupalika)"
;
label define FW105   
     1 "Male"
     2 "Female"
;
label define FW106   
     1 "Currently married"
     2 "Living with a man/woman"
     3 "Widowed"
     4 "Divorced"
     5 "Separated"
     6 "Never married or lived with a man/woman"
;
label define FW108   
     1 "Yes"
     2 "No"
;
label define FW110   
     0 "Less than one year at level"
;
label define FW111   
     1 "Hindu"
     2 "Buddhist"
     3 "Muslim"
     4 "Kirat"
     5 "Christian"
    95 "No religion"
    96 "Other"
;
label define FW112   
     1 "Hill brahmin"
     2 "Hill chhetri"
     3 "Terai brahmin/chhetri"
     4 "Other terai caste"
     5 "Hill dalit"
     6 "Terai dalit"
     7 "Newar"
     8 "Hill janajati"
     9 "Terai janajati"
    10 "Muslim"
    96 "Other"
;
label define FW114   
     1 "Nepali"
     2 "Maithili"
     3 "Bhojpuri"
     4 "Tharu"
     5 "Newari"
     6 "Tamang"
     7 "Awadhi"
    96 "Other"
;
label define FW115A  
     1 "Yes"
     2 "No"
;
label define FW115C  
     1 "Yes"
     2 "No"
;
label define FW116   
     1 "Yes"
     3 "No"
;
label define FW117   
     1 "Permanent"
     2 "Temporary"
;
label define FW118   
     1 "Yes"
     2 "No"
;

#delimit cr
label values fw102    FW102   
label values fw103    FW103   
label values fw105    FW105   
label values fw106    FW106   
label values fw108    FW108   
label values fw110    FW110   
label values fw111    FW111   
label values fw112    FW112   
label values fw114    FW114   
label values fw115a   FW115A  
label values fw115c   FW115C  
label values fw116    FW116   
label values fw117    FW117   
label values fw118    FW118   
