infile using "C:\USERS\21429\ICF\DATA ARCHIVE - DOCUMENTS\EXPORT MENU\NP81\NPAI81FL.dct"

label variable hhid     "Case Identification"
label variable haiidx   "Occurrence line number (index)"
label variable hv000    "Country code and phase"
label variable hv001    "Cluster number"
label variable hv002    "Household number"
label variable hv003    "Respondent's line number (answering Household questionnaire)"
label variable hv004    "Ultimate area unit"
label variable hv005    "Household sample weight (6 decimals)"
label variable hv006    "Month of interview"
label variable hv007    "Year of interview"
label variable hv008    "Date of interview (CMC)"
label variable hv008a   "Date of interview Century Day Code (CDC)"
label variable hv009    "Number of household members"
label variable hv010    "Number of eligible women in household"
label variable hv011    "Number of eligible men in household"
label variable hv012    "Number of de jure members"
label variable hv013    "Number of de facto members"
label variable hv014    "Number of children 5 and under (de jure)"
label variable hv015    "Result of household interview"
label variable hv016    "Day of interview"
label variable hv017    "Number of visits"
label variable hv018    "Interviewer identification"
label variable hv019    "NA - Keyer identification"
label variable hv020    "Ever-married sample"
label variable hv021    "Primary sampling unit"
label variable hv022    "Sample strata for sampling errors"
label variable hv023    "Stratification used in sample design"
label variable hv024    "Province"
label variable hv025    "Type of place of residence"
label variable hv026    "NA - Place of residence"
label variable hv027    "Household selected for man's interview"
label variable hv028    "Household weight for male subsample (6 decimals)"
label variable hv030    "Field supervisor"
label variable hv031    "NA - Field editor"
label variable hv032    "NA - Office editor"
label variable hv035    "Number of eligible children for height and weight"
label variable hv040    "Cluster altitude in meters"
label variable hv041    "Total adults measured"
label variable hv042    "Household selected for hemoglobin"
label variable hv044    "Household selected for Domestic Violence module"
label variable hv045a   "Language of questionnaire"
label variable hv045b   "Language of interview"
label variable hv045c   "Native language of respondent"
label variable hv046    "Translator used"
label variable hv047    "Team number"
label variable hv048    "Team supervisor"
label variable hv801    "Time interview started (hhmm - 24 hour clock)"
label variable hv802    "Time interview ended (hhmm - 24 hour clock)"
label variable hv803    "Length of interview in minutes"
label variable hv804    "Number of visits for biomarker"
label variable hv807d   "Day of biomarker visit"
label variable hv807m   "Month of biomarker visit"
label variable hv807y   "Year of biomarker visit"
label variable hv807c   "Date of biomarker (CMC)"
label variable hv807a   "Date of biomarker Century Day Code (CDC)"
label variable hai1     "Accident type"
label variable hai2     "Vehicle involved in traffic accident"
label variable hai3     "Type of accident for accidents other than traffic"
label variable hai4     "How the accident happened for other than traffic"
label variable hai5     "Household member still alive as a result of accident"
label variable hai6     "Sex of household member involved in accident"
label variable hai7     "Age of household member at death"
label variable hai8     "Death related to the accident"
label variable hai9a    "Injures resulting from accident: cut/bite/open wound"
label variable hai9b    "Injures resulting from accident: broken bone"
label variable hai9c    "Injures resulting from accident: burn"
label variable hai9d    "Injures resulting from accident: poisoning"
label variable hai9e    "Injures resulting from accident: head injury"
label variable hai9f    "Injures resulting from accident: internal injury"
label variable hai9g    "Injures resulting from accident: suffocation"
label variable hai9h    "Injures resulting from accident: spinal curd injury - CS"
label variable hai9i    "NA - Injures resulting from accident: CS"
label variable hai9j    "NA - Injures resulting from accident: CS"
label variable hai9k    "NA - Injures resulting from accident: CS"
label variable hai9l    "NA - Injures resulting from accident: CS"
label variable hai9m    "NA - Injures resulting from accident: CS"
label variable hai9n    "NA - Injures resulting from accident: CS"
label variable hai9x    "Injures resulting from accident: other"
label variable hai9z    "Injures resulting from accident: don't know"
label variable hai10    "Household line number of member involved in accident"
label variable hai11    "Current age of household member involved in accident"
label variable hai12    "Household member continues to have health problems"
label variable hai13a   "Problem still has from accident: paralyzed"
label variable hai13b   "Problem still has from accident: brain damage"
label variable hai13c   "Problem still has from accident: disfigurement"
label variable hai13d   "Problem still has from accident: loss of limb"
label variable hai13e   "Problem still has from accident: loss of limb function"
label variable hai13f   "Problem still has from accident: loss of eyesight"
label variable hai13g   "Problem still has from accident: loss of hearing"
label variable hai13h   "Problem still has from accident: chronic pain"
label variable hai13i   "Problem still has from accident: emotional trauma"
label variable hai13j   "NA - Problem still has from accident: CS"
label variable hai13k   "NA - Problem still has from accident: CS"
label variable hai13l   "NA - Problem still has from accident: CS"
label variable hai13m   "NA - Problem still has from accident: CS"
label variable hai13n   "NA - Problem still has from accident: CS"
label variable hai13o   "NA - Problem still has from accident: CS"
label variable hai13p   "NA - Problem still has from accident: CS"
label variable hai13x   "Problem still has from accident: other"
label variable hai13z   "Problem still has from accident: don't know"

#delimit ;
label define HV003   
     0 "Incomplete household"
;
label define HV006   
     1 "Baisakh"
     2 "Jestha"
     3 "Asadh"
     4 "Shrawan"
     5 "Bhadra"
     6 "Ashwin"
     7 "Kartik"
     8 "Mangsir"
     9 "Poush"
    10 "Magh"
    11 "Falgun"
    12 "Chaitra"
;
label define HV015   
     1 "Completed"
     2 "No Household member/no competent member at home"
     3 "Entire Household absent for extended period of time"
     4 "Postponed"
     5 "Refused"
     6 "Dwelling vacant or address not a dwelling"
     7 "Dwelling destroyed"
     8 "Dwelling not found"
     9 "Other"
;
label define HV017   
     9 "Missing"
;
label define HV020   
     0 "All woman sample"
     1 "Ever married sample"
;
label define HV022   
     1 "Koshi - urban"
     2 "Koshi - rural"
     3 "Madhesh province - urban"
     4 "Madhesh province - rural"
     5 "Bagmati province - urban"
     6 "Bagmati province - rural"
     7 "Gandaki province - urban"
     8 "Gandaki province - rural"
     9 "Lumbini province - urban"
    10 "Lumbini province - rural"
    11 "Karnali province - urban"
    12 "Karnali province - rural"
    13 "Sudurpashchim province - urban"
    14 "Sudurpashchim province - rural"
;
label define HV023   
     1 "Koshi - urban"
     2 "Koshi - rural"
     3 "Madhesh province - urban"
     4 "Madhesh province - rural"
     5 "Bagmati province - urban"
     6 "Bagmati province - rural"
     7 "Gandaki province - urban"
     8 "Gandaki province - rural"
     9 "Lumbini province - urban"
    10 "Lumbini province - rural"
    11 "Karnali province - urban"
    12 "Karnali province - rural"
    13 "Sudurpashchim province - urban"
    14 "Sudurpashchim province - rural"
;
label define HV024   
     1 "Koshi province"
     2 "Madhesh province"
     3 "Bagmati province"
     4 "Gandaki province"
     5 "Lumbini province"
     6 "Karnali province"
     7 "Sudurpashchim province"
;
label define HV025   
     1 "Urban"
     2 "Rural"
;
label define HV026   
     0 "Capital, large city"
     1 "Small city"
     2 "Town"
     3 "Countryside"
;
label define HV027   
     0 "Not selected"
     1 "Men's survey"
     2 "Husband's survey"
;
label define HV040   
  9999 "Missing"
;
label define HV042   
     0 "Not selected"
     1 "Selected"
;
label define HV044   
     0 "Household not selected"
     1 "Household selected"
;
label define HV045A  
     1 "English"
     2 "Nepali"
     3 "Maithali"
     4 "Bhojpuri"
     6 "Other language"
;
label define HV045B  
     1 "English"
     2 "Nepali"
     3 "Maithali"
     4 "Bhojpuri"
     6 "Other language"
;
label define HV045C  
     1 "English"
     2 "Nepali"
     3 "Maithali"
     4 "Bhojpuri"
     6 "Other language"
;
label define HV046   
     0 "No"
     1 "Yes"
;
label define HV801   
  9999 "Missing"
;
label define HV802   
  9999 "Missing"
;
label define HV803   
    95 "95+"
    96 "2+ visits"
    97 "Inconsistent"
;
label define HV804   
     9 "Missing"
;
label define HV807M  
     1 "Baisakh"
     2 "Jestha"
     3 "Asadh"
     4 "Shrawan"
     5 "Bhadra"
     6 "Ashwin"
     7 "Kartik"
     8 "Mangsir"
     9 "Poush"
    10 "Magh"
    11 "Falgun"
    12 "Chaitra"
;
label define HAI1    
     1 "Traffic accident"
     2 "Other type of accident"
;
label define HAI2    
     1 "Car"
     2 "Truck"
     3 "Bus"
     4 "Motorcycle"
     5 "Bicycle"
     6 "Pedestrian"
     7 "Three wheel tempo"
    96 "Other vehicle"
    98 "Don't know"
;
label define HAI3    
     1 "Fire/burning"
     2 "Animal bite"
     3 "Fall"
     4 "Drowning/near drowning"
     5 "Poisoning"
     6 "Electrical injury"
     7 "Struck by person/object"
     8 "Cut or stabbed"
     9 "Gunshot"
    10 "Occupational accident (factory)"
    11 "Occupational accident (agriculture)"
    96 "Other"
    98 "Don't know"
;
label define HAI4    
     1 "Accidental"
     2 "Natural disaster"
     3 "Violence/assault"
     4 "Self-harm"
     8 "Don't know"
;
label define HAI5    
     0 "No"
     1 "Yes"
     8 "Don't know"
;
label define HAI6    
     1 "Male"
     2 "Female"
;
label define HAI7    
    95 "95+"
    98 "Don't know"
;
label define HAI8    
     0 "No"
     1 "Yes"
;
label define HAI9A   
     0 "No"
     1 "Yes"
     8 "Don't know"
;
label define HAI9B   
     0 "No"
     1 "Yes"
     8 "Don't know"
;
label define HAI9C   
     0 "No"
     1 "Yes"
     8 "Don't know"
;
label define HAI9D   
     0 "No"
     1 "Yes"
     8 "Don't know"
;
label define HAI9E   
     0 "No"
     1 "Yes"
     8 "Don't know"
;
label define HAI9F   
     0 "No"
     1 "Yes"
     8 "Don't know"
;
label define HAI9G   
     0 "No"
     1 "Yes"
     8 "Don't know"
;
label define HAI9H   
     0 "No"
     1 "Yes"
     8 "Don't know"
;
label define HAI9I   
     0 "No"
     1 "Yes"
     8 "Don't know"
;
label define HAI9J   
     0 "No"
     1 "Yes"
     8 "Don't know"
;
label define HAI9K   
     0 "No"
     1 "Yes"
     8 "Don't know"
;
label define HAI9L   
     0 "No"
     1 "Yes"
     8 "Don't know"
;
label define HAI9M   
     0 "No"
     1 "Yes"
     8 "Don't know"
;
label define HAI9N   
     0 "No"
     1 "Yes"
     8 "Don't know"
;
label define HAI9X   
     0 "No"
     1 "Yes"
     8 "Don't know"
;
label define HAI9Z   
     0 "No"
     1 "Yes"
     8 "Don't know"
;
label define HAI10   
     0 "Not in household"
    95 "Member died"
;
label define HAI11   
    95 "95+"
    98 "Don't know"
;
label define HAI12   
     0 "No"
     1 "Yes"
     8 "Don't know"
;
label define HAI13A  
     0 "No"
     1 "Yes"
     8 "Don't know"
;
label define HAI13B  
     0 "No"
     1 "Yes"
     8 "Don't know"
;
label define HAI13C  
     0 "No"
     1 "Yes"
     8 "Don't know"
;
label define HAI13D  
     0 "No"
     1 "Yes"
     8 "Don't know"
;
label define HAI13E  
     0 "No"
     1 "Yes"
     8 "Don't know"
;
label define HAI13F  
     0 "No"
     1 "Yes"
     8 "Don't know"
;
label define HAI13G  
     0 "No"
     1 "Yes"
     8 "Don't know"
;
label define HAI13H  
     0 "No"
     1 "Yes"
     8 "Don't know"
;
label define HAI13I  
     0 "No"
     1 "Yes"
     8 "Don't know"
;
label define HAI13J  
     0 "No"
     1 "Yes"
     8 "Don't know"
;
label define HAI13K  
     0 "No"
     1 "Yes"
     8 "Don't know"
;
label define HAI13L  
     0 "No"
     1 "Yes"
     8 "Don't know"
;
label define HAI13M  
     0 "No"
     1 "Yes"
     8 "Don't know"
;
label define HAI13N  
     0 "No"
     1 "Yes"
     8 "Don't know"
;
label define HAI13O  
     0 "No"
     1 "Yes"
     8 "Don't know"
;
label define HAI13P  
     0 "No"
     1 "Yes"
     8 "Don't know"
;
label define HAI13X  
     0 "No"
     1 "Yes"
     8 "Don't know"
;
label define HAI13Z  
     0 "No"
     1 "Yes"
;

#delimit cr
label values hv003    HV003   
label values hv006    HV006   
label values hv015    HV015   
label values hv020    HV020   
label values hv022    HV022   
label values hv023    HV023   
label values hv024    HV024   
label values hv025    HV025   
label values hv026    HV026   
label values hv027    HV027   
label values hv042    HV042   
label values hv044    HV044   
label values hv045a   HV045A  
label values hv045b   HV045B  
label values hv045c   HV045C  
label values hv046    HV046   
label values hv803    HV803   
label values hv807m   HV807M  
label values hai1     HAI1    
label values hai2     HAI2    
label values hai3     HAI3    
label values hai4     HAI4    
label values hai5     HAI5    
label values hai6     HAI6    
label values hai7     HAI7    
label values hai8     HAI8    
label values hai9a    HAI9A   
label values hai9b    HAI9B   
label values hai9c    HAI9C   
label values hai9d    HAI9D   
label values hai9e    HAI9E   
label values hai9f    HAI9F   
label values hai9g    HAI9G   
label values hai9h    HAI9H   
label values hai9i    HAI9I   
label values hai9j    HAI9J   
label values hai9k    HAI9K   
label values hai9l    HAI9L   
label values hai9m    HAI9M   
label values hai9n    HAI9N   
label values hai9x    HAI9X   
label values hai9z    HAI9Z   
label values hai10    HAI10   
label values hai11    HAI11   
label values hai12    HAI12   
label values hai13a   HAI13A  
label values hai13b   HAI13B  
label values hai13c   HAI13C  
label values hai13d   HAI13D  
label values hai13e   HAI13E  
label values hai13f   HAI13F  
label values hai13g   HAI13G  
label values hai13h   HAI13H  
label values hai13i   HAI13I  
label values hai13j   HAI13J  
label values hai13k   HAI13K  
label values hai13l   HAI13L  
label values hai13m   HAI13M  
label values hai13n   HAI13N  
label values hai13o   HAI13O  
label values hai13p   HAI13P  
label values hai13x   HAI13X  
label values hai13z   HAI13Z  
