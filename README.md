# Assessing Undernutrition and Anemia Among Children and Women in Nepal Using DHS 2022 Data

This study aimed to determine the prevalence of undernutrition, anemia, and their co-existence in Nepal, as well as determine their contributing factors among children aged 6-59 months using the **2022 Nepal Demographic and Health Survey (NDHS)** dataset. It utilizes survey-weighted analysis, descriptive summaries, regression modeling, and visualization to uncover socio-demographic and maternal determinants.

------------------------------------------------------------------------

## 🧑‍💻 Authors

*Bikram Adhikari1,2*, Biraj Neupane3,2, Jessica Rice1, Niharika Jha1,2, Kajol Dahal4, Parash Mani Sapkota5, Archana Shrestha6,7, Xinhua Yu1, Yu Jiang1

*Affiliation:*

1 School of Public health, University of Memphis, Tennessee, United States of America

2 Sindhu Research and Implementation Institute, Sunkoshi, Sindhupalchok, Nepal.

3 Informatics Program, School of Information Science, University of Illinois Urbana-Champaign, Illinois, United States of America

4 East Tennessee state University, Tennessee, United States of America

5 HERD International, Nepal

6 School of Public Health, Kathmandu University School of Medical Sciences, Dhulikhel, Nepal

7 Institute of Implementation Science and Health, Kathmandu, Nepal

**Correspondence**

📧 Email: [[bdhikari\@memphis.edu](mailto:your_email@example.com){.email}]\
🌐 GitHub: <https://github.com/Adrisyaone>

------------------------------------------------------------------------

## Prerequisites

-   R (≥ 4.3)

-   Quarto (for rendering .qmd files)

-   DHS dataset access (after registration)

------------------------------------------------------------------------

## Data Source

**Dataset:** Nepal Demographic and Health Survey (NDHS) 2022\
**Source:** [DHS Program](https://dhsprogram.com/data/dataset/Nepal_Standard-DHS_2022.cfm?flag=0)\
**Report:** [NDHS 2022 Final Report (FR379)](https://dhsprogram.com/pubs/pdf/FR379/FR379.pdf)

| Dataset | File           | Description             |
|---------|----------------|-------------------------|
| PR      | `NPPR82FL.DTA` | Household member recode |
| KR      | `NPKR82FL.DTA` | Children under 5 recode |
| IR      | `NPIR82FL.DTA` | Women aged 15–49 recode |
| BR      | `NPBR82FL.DTA` | Birth recode            |

## ------------------------------------------------------------------------

## Project Structure

```         
├── Script/
│   ├── 1_Functions.R
│   ├── 2_Load_dataset.R
│   ├── 3_Clean_dataset.R
│   └── 5_Result.qmd
│
├── Datasets/
│   ├── Raw data/
│   ├── Processed data/
│   └── For chart.xlsx
│
├── Outputs/
│   ├── Tables/
│   ├── Figures/
│   └── Reports/
│
└── README.md
```

------------------------------------------------------------------------

## ⚙️ Setup Instructions

### 1. Clone the Repository

``` bash
git clone git@github.com:Adrisyaone/Undernutritiona-and-anemia.git
cd Undernutritiona-and-anemia
```

### 2. Install Required R Packages

``` r
Packages <- c("tidyverse", "haven", "survey", "srvyr", "gtsummary", "foreign",
              "sjPlot", "ggeffects", "performance", "data.table", "expss", 
              "labelled", "readxl", "nnet", "sf", "ggspatial", "forcats", 
              "rockchalk", "marginaleffects")

new_packages <- Packages[!(Packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages, dependencies = TRUE)
lapply(Packages, require, character.only = TRUE)
```

### 3. Load and Clean Data

-   Run `2_Load_dataset.R` to import DHS datasets.\
-   Run `3_Clean_dataset.R` to prepare the analytical dataset.

------------------------------------------------------------------------

## Analyses Conducted

1.  **Descriptive Statistics** — Weighted summaries by demographic variables.\
2.  **Prevalence Estimates** — Stunting, wasting, underweight, and anemia.\
3.  **Regression Models** — Logistic regression for associated factors.\
4.  **Visualization** — Prevalence and risk difference charts.

------------------------------------------------------------------------

## 🧾 Results

| Output                        | Description                         |
|-------------------------------|-------------------------------------|
| `Characteristics.xlsx`        | Summary of participants             |
| `Factors_undernutrition.xlsx` | Regression model for undernutrition |
| `Factors_anemia.xlsx`         | Regression model for anemia         |
| `Weighted_Figures/`           | Visual outputs                      |
| `Final_Report.docx`           | Complete analytical report          |

------------------------------------------------------------------------

## 📜 License

NDHS datasets remain the property of the **Demographic and Health Surveys (DHS) Program)**.

Users must adhere to DHS data usage policies.

------------------------------------------------------------------------

## 💡 Citation

> Adhikari, B. et al (2025). *Assessing Undernutrition and Anemia Among Children and Women in Nepal Using DHS 2022 Data*. University of Memphis.

------------------------------------------------------------------------

*Developed with ❤️ in R and Quarto.*
