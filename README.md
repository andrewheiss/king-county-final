# DC121: King County Metro analysis

## 🚨 MASSIVE HUGE APOLOGY AND CAVEATS 🚨
 
This analysis is **incomplete** and *likely incorrect*. Our team ran into medical emergencies during the last month (3-week-long family-wide bout COVID + hospital/ER visit for COVID positive child) and we ran out of time to complete the analysis and come to firm conclusions. We had every intention (and meticulous schedule) of completing all the analysis, but COVID derailed all that. As such, we did not include formal policy briefs for either of the two questions. 

We have included the data cleaning code and analysis (`question1_code_DC121.R` and `question2_code_DC121.R`), and initial methods reports (`question1_methods_DC121.Rmd` and `question2_methods_DC121.Rmd`) for the sake of transparency and hope that they'll be useful to the research team looking at how different teams approach these questions. But we have very little trust in the accuracy or reliability of our estimates at this point.

## Original raw data location

To avoid reuploading 2+GB of data, the project assumes that the [entire data folder from Google Drive](https://drive.google.com/drive/folders/1ADTVZGJOUHyPv5i-lmQXvik0YkMzr9Nf) is placed in `data/raw_data_from_king_county`:

```
data
└── raw_data_from_king_county
    ├── ACS_Variables_Selected.xlsx
    ├── Data Challenge Information Package.pdf
    ├── KCM_Stops_Data
    │   ├── kcm_stops.cpg
    │   ├── kcm_stops.dbf
    │   ├── kcm_stops.prj
    │   ├── kcm_stops.sbn
    │   ├── kcm_stops.sbx
    │   ├── kcm_stops.shp
    │   ├── kcm_stops.shx
    │   └── kcm_stops.xml
    ├── King_County_ACS_2019_tract.csv
    ├── Question 1_ Fare Reinstatement
    │   ├── alltrips_2020-09_to_2020-10.csv
    │   ├── alltrips_data_dictionary.xlsx
    │   ├── apc_data_dictionary.xlsx
    │   ├── apc_detailed_09-01-2020_10-31-2020.csv
    │   ├── stop_activity_granular_2020-09-01_2020-10-31.csv
    │   └── stop_activity_granular_data_dictionary.xlsx
    ├── Question 2_ Fare Subsidies
    │   ├── LIFT_boardings.csv
    │   ├── LIFT_boardings_2021-11-01_to_2022-03-06.csv
    │   ├── LIFT_data_dictionary.xlsx
    │   ├── LIFT_registry.csv
    │   ├── LIFT_registry_2022-03-22.csv
    │   ├── LIFT_registry_2022-04-01.csv
    │   ├── LIFT_sales.csv
    │   ├── LIFT_sales_2021-11-01_to_2022-03-06.csv
    │   └── LIFT_sales_2022-04-01.csv
    └── README.markdown.md
```
