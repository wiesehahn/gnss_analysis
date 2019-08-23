# GNSS Analysis
**Analysis of GNSS signals received by mobile devices in university class**

project website: https://wiesehahn.github.io/gnss_analysis/index.html

## Data collection
During lectures on GNSS signals data was collected using different mobile devices at 12 reference positions. The data was logged for at least 1 minute using the app "GPSLogger". 

## Data analysis
The nmea-data files are analyzed with R Software. An example report is located in \rmd.

## folder structure

```name_of_project
|--raw_data
    |--WhateverData.xlsx
    |--2017report.csv
    |--2016report.pdf
|--output_data
    |--summary2016_2017.csv
|--rmd
    |--01-analysis.Rmd
|--docs
    |--01-analysis.html
    |--01-analysis.pdf
    |--02-deeper.html
    |--02-deeper.pdf
|--scripts
    |--exploratory_analysis.R
    |--pdf_scraper.R
|--name_of_project.Rproj
|--run_all.R
```

