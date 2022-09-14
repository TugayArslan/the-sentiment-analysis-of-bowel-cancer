## The Sentiment Analysis of Bowel Cancer in Northern Ireland

Please put csv files in the release section under root directory of this repo.

# Project Description
- MSc in Data Analytics/Science (DSA8030)
- Faculty of Engineering and Physical Sciences
- Author: Tugay Arslan
- Supervisor: Dr. Felicity Lamrock

Bowel cancer is the 4th most common cancer in the UK. In Northern Ireland approximately 1,200 people are diagnosed with bowel cancer each year, and around 500 deaths.

In April 2010, the Northern Ireland Bowel Cancer Screening Programme (BCSP) was launched to provide early detection and treatment of bowel cancer with the aim of reducing the mortality and morbidity of the disease within in Northern Ireland. The programme targets asymptomatic 60 to 75-year-olds and screens this group every 2 years. Patients are given a home collection kit where their stool sample is taken and tested to assess if further investigation via a colonoscopy is needed. [1]

Other routes to diagnosis of bowel cancer are hospital via emergency or General Practitioner (GP) referrals. [2]

In general, patients may find it hard to talk to their doctor about changes in bowel habits, and due to COVID-19 doctors interact with patients differently now via telephone triage.

The goal of this project is to establish trends and patterns in social media behaviours towards bowel cancer. By scraping data from Twitter and using sentiment analysis, we will gain more insights into different views of bowel cancer and possibly the impact that COVID-19 has had on this.

[1] https://www.publichealth.hscni.net/directorate-public-health/servicedevelopment-and-screening/bowel-cancer-screening

[2] https://hscbusiness.hscni.net/pdf/Routes%20to%20Diagnosis%20Report%20-%20Main%20Report%20Jan%202020.pdf

# Project Objectives
- Scrape Twitter data from Department of Health, Public Health Agency and other relevant Northern Ireland Twitter accounts regarding bowel cancer
- Data cleaning and pre-processing in R
- Perform exploratory data analysis. For example describe how many tweets were posted and how often by each account. What are the most popular bowel cancer tweets over certain period of time.
- Sentiment analysis – natural language processing using the text mining package tm in R
- Display findings using a Shiny app

# Packages Used
- Following R packages are used: shiny, shinydashboard, tidyverse, ggplot2, plotly, dplyr, lubridate, stringr, tidyr, tidytext, tm, purr, RColorBrewer, shinycssloaders, janitor, knitr, syuzhet, colourpicker and ggwordcloud. Some of the shiny UI design elements are from an open-source project.
- Following python packages are used: snscrape and pandas.
- Dashboard UI has been reused from https://github.com/gadenbuie/tweet-conf-dash. Thank you!
