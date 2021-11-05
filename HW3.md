---
title: "HW3"
author: "Haoran Zhang"
date: "2021/11/3"
output: 
  - html_document
  - github_document
always_allow_html: true
---



```r
library(httr)
library(rvest)
library(xml2)
library(data.table)
library(stringr)
library(dplyr)
library(ggplot2)
library(tidytext)
library(tidyverse)
```

## APIs
1. Using the NCBI API, look for papers that show up under the term “sars-cov-2 trial vaccine.” Look for the data in the pubmed database, and then retrieve the details of the paper as shown in lab 7. How many papers were you able to find?

```r
# Downloading the website
website <- xml2::read_html("https://pubmed.ncbi.nlm.nih.gov/?term=sars-cov-2+trial+vaccine")
# Finding the counts
counts <- xml2::xml_find_first(website, "/html/body/main/div[9]/div[2]/div[2]/div[1]/div[1]/span")
# Turning it into text
counts <- as.character(counts)
# Extracting the data using regex
stringr::str_extract(counts, "[0-9,]+")
```

```
## [1] "2,335"
```

```r
stringr::str_extract(counts, "[[:digit:],]+")
```

```
## [1] "2,335"
```

```r
stringr::str_replace(counts, "[^[:digit:]]+([[:digit:]]+),([[:digit:]]+)[^[:digit:]]+", "\\1\\2")
```

```
## [1] "2335"
```

2. Using the list of pubmed ids you retrieved, download each papers’ details using the query parameter rettype = abstract. If you get more than 250 ids, just keep the first 250.


```r
query_ids <- GET(
  url   = "https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi",
  query = list(
    db = "pubmed",
    term = "sars-cov-2 trial vaccine",
    retmax = 250)
)

GET(
  url   = "https://eutils.ncbi.nlm.nih.gov/",
  path  = "entrez/eutils/esearch.fcgi",
  query = list(
    db     = "pubmed",
    term   = "sars-cov-2 trial vaccine",
    retmax = 250
    )
)
```

```
## Response [https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esearch.fcgi?db=pubmed&term=sars-cov-2%20trial%20vaccine&retmax=250]
##   Date: 2021-11-05 01:07
##   Status: 200
##   Content-Type: text/xml; charset=UTF-8
##   Size: 7.76 kB
## <?xml version="1.0" encoding="UTF-8" ?>
## <!DOCTYPE eSearchResult PUBLIC "-//NLM//DTD esearch 20060628//EN" "https://eu...
## <eSearchResult><Count>1044</Count><RetMax>250</RetMax><RetStart>0</RetStart><...
## <Id>34729549</Id>
## <Id>34726743</Id>
## <Id>34715931</Id>
## <Id>34713912</Id>
## <Id>34711598</Id>
## <Id>34704204</Id>
## <Id>34703690</Id>
## ...
```

```r
# Extracting the content of the response of GET
ids <- httr::content(query_ids)
```

3. Get details about the articles

```r
# Turn the result into a character vector
ids <- as.character(ids)
# Find all the ids 
ids <- stringr::str_extract_all(ids, "<Id>[[:digit:]]+</Id>")[[1]]
# Remove all the leading and trailing <Id> </Id>. Make use of "|"
# stringr::str_remove_all(ids, "</?Id>")
ids <- stringr::str_remove_all(ids, "<Id>|</Id>")
```


```r
publications <- GET(
  url   = "https://eutils.ncbi.nlm.nih.gov/",
  path  = "entrez/eutils/efetch.fcgi",
  query = list(
    db = "pubmed",
    id = I(paste(ids, collapse=",")),
    retmax = 250,
    rettype = "abstract"
    )
)
# Turning the output into character vector
publications <- httr::content(publications)
publications_txt <- as.character(publications)
```
As we did in lab 7. Create a dataset containing the following:

Pubmed ID number,
Title of the paper,
Name of the journal where it was published,
Publication date, and
Abstract of the paper (if any).

```r
pub_char_list <- xml_children(publications)
pub_char_list <- sapply(pub_char_list, as.character)
# abstracts <- str_extract(pub_char_list, "<Abstract>[[:alnum:].%<>/&|=+-]+</Abstract>")
abstracts <- str_extract(pub_char_list, "<Abstract>[[:print:][:space:]]+</Abstract>")
abstracts <- str_remove_all(abstracts, "</?[[:alnum:]- =\"]+.")
abstracts <- str_replace_all(abstracts, "[[:space:]]+", " ")

titles <- str_extract(pub_char_list, "<ArticleTitle>[[:print:][:space:]]+</ArticleTitle>")
titles <- str_remove_all(titles, "</?[[:alnum:]- =\"]+>")

journals <- str_extract(pub_char_list, "<Title>[[:print:][:space:]]+</Title>")
journals <- str_remove_all(journals, "</?[[:alnum:]- =\"]+>")

#dates <- str_extract(pub_char_list, "<PubDate>[[:print:][:space:]]+</PubDate>")
dates <- str_extract(pub_char_list, "<PubDate>[[:print:][:space:]]+</PubDate>")
dates <- str_remove_all(dates, "</?[[:alnum:]]+>")
dates <- str_replace_all(dates, "\\s+", " ")
database <- data.frame(
  PubMedId = ids,
  Title    = titles,
  Journal  = journals,
  Date     = dates,
  Abstract = abstracts
)
knitr::kable(database[1:10,], caption = "Papers about COVID-19 Vaccine")
```



Table: Papers about COVID-19 Vaccine

|PubMedId |Title                                                                                                                                              |Journal                                                                                              |Date        |Abstract                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                 |
|:--------|:--------------------------------------------------------------------------------------------------------------------------------------------------|:----------------------------------------------------------------------------------------------------|:-----------|:------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
|34729549 |Adverse events of active and placebo groups in SARS-CoV-2 vaccine randomized trials: A systematic review.                                          |The Lancet regional health. Europe                                                                   |2021 Oct 28 |For safety assessment in clinical trials, adverse events (AEs) are reported for the drug under evaluation and compared with AEs in the placebo group. Little is known about the nature of the AEs associated with clinical trials of SARS-CoV-2 vaccines and the extent to which these can be traced to nocebo effects, where negative treatment-related expectations favor their occurrence. In our systematic review, we compared the rates of solicited AEs in the active and placebo groups of SARS-CoV-2 vaccines approved by the Western pharmaceutical regulatory agencies.We implemented a search strategy to identify trial-III studies of SARS-CoV-2 vaccines through the PubMed database. We adopted the PRISMA Statement to perform the study selection and the data collection and identified three trial: two mRNA-based (37590 participants) and one adenovirus type (6736 participants). Relative risks showed that the occurrence of AEs reported in the vaccine groups was higher compared with the placebo groups. The most frequently AEs in both groups were fatigue, headache, local pain, as injection site reactions, and myalgia. In particular, for first doses in placebo recipients, fatigue was reported in 29% and 27% in BNT162b2 and mRNA-1273 groups, respectively, and in 21% of Ad26.COV2.S participants. Headache was reported in 27% in both mRNA groups and in 24% of Ad26.COV2.S recipients. Myalgia was reported in 10% and 14% in mRNA groups (BNT162b2 and mRNA-1273, respectively) and in 13% of Ad26.COV2.S participants. Local pain was reported in 12% and 17% in mRNA groups (BNT162b2 and mRNA-1273, respectively), and in 17% of Ad26.COV2.S recipients. These AEs are more common in the younger population and in the first dose of placebo recipients of the mRNA vaccines. Our results are in agreement with the expectancy theory of nocebo effects and suggest that the AEs associated with COVID-19 vaccines may be related to the nocebo effect. Fondazione CRT - Cassa di Risparmio di Torino, IT (grant number 66346, "GAIA-MENTE" 2019). <U+00A9> 2021 The Authors.                                                                                                                                                                                                                                                                                                                                                                                          |
|34726743 |Analysis of the Effectiveness of the Ad26.COV2.S Adenoviral Vector Vaccine for Preventing COVID-19.                                                |JAMA network open                                                                                    |2021 11 01  |Continuous assessment of the effectiveness and safety of the US Food and Drug Administration-authorized SARS-CoV-2 vaccines is critical to amplify transparency, build public trust, and ultimately improve overall health outcomes. To evaluate the effectiveness of the Johnson &amp; Johnson Ad26.COV2.S vaccine for preventing SARS-CoV-2 infection. Setting, and Participants">This comparative effectiveness research study used large-scale longitudinal curation of electronic health records from the multistate Mayo Clinic Health System (Minnesota, Arizona, Florida, Wisconsin, and Iowa) to identify vaccinated and unvaccinated adults between February 27 and July 22, 2021. The unvaccinated cohort was matched on a propensity score derived from age, sex, zip code, race, ethnicity, and previous number of SARS-CoV-2 polymerase chain reaction tests. The final study cohort consisted of 8889 patients in the vaccinated group and 88 898 unvaccinated matched patients. Single dose of the Ad26.COV2.S vaccine. The incidence rate ratio of SARS-CoV-2 infection in the vaccinated vs unvaccinated control cohorts, measured by SARS-CoV-2 polymerase chain reaction testing. The study was composed of 8889 vaccinated patients (4491 men [50.5%]; mean [SD] age, 52.4 [16.9] years) and 88 898 unvaccinated patients (44 748 men [50.3%]; mean [SD] age, 51.7 [16.7] years). The incidence rate ratio of SARS-CoV-2 infection in the vaccinated vs unvaccinated control cohorts was 0.26 (95% CI, 0.20-0.34) (60 of 8889 vaccinated patients vs 2236 of 88 898 unvaccinated individuals), which corresponds to an effectiveness of 73.6% (95% CI, 65.9%-79.9%) and a 3.73-fold reduction in SARS-CoV-2 infections. This study's findings are consistent with the clinical trial-reported efficacy of Ad26.COV2.S and the first retrospective analysis, suggesting that the vaccine is effective at reducing SARS-CoV-2 infection, even with the spread of variants such as Alpha or Delta that were not present in the original studies, and reaffirm the urgent need to continue mass vaccination efforts globally.                                                                                                                                                                                                                                                                                                                                                                           |
|34715931 |Lessons from Israel's COVID-19 Green Pass program.                                                                                                 |Israel journal of health policy research                                                             |2021 10 29  |As of the beginning of March 2021, Israeli law requires the presentation of a Green Pass as a precondition for entering certain businesses and public spheres. Entitlement for a Green Pass is granted to Israelis who have been vaccinated with two doses of COVID-19 vaccine, who have recovered from COVID-19, or who are participating in a clinical trial for vaccine development in Israel. The Green Pass is essential for retaining immune individuals' freedom of movement and for promoting the public interest in reopening the economic, educational, and cultural spheres of activity. Nonetheless, and as the Green Pass imposes restrictions on the movement of individuals who had not been vaccinated or who had not recovered, it is not consonant with solidarity and trust building. Implementing the Green Pass provision while advancing its effectiveness on the one hand, and safeguarding equality, proportionality, and fairness on the other hand may imbue this measure with ethical legitimacy despite involving a potential breach of trust and solidarity. <U+00A9> 2021. The Author(s).                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                  |
|34713912 |Vaccine development and technology for SARS-CoV-2: current insights.                                                                               |Journal of medical virology                                                                          |2021 Oct 29 |SARS-CoV-2 is associated to a severe respiratory disease in China, that rapidly spread across continents. Since the beginning of the pandemic, available data suggested the asymptomatic transmission and patients were treated with specific drugs with efficacy and safety data not always satisfactory. The aim of this review is to describe the vaccines developed by three companies, Pfizer-BioNTech, Moderna and University of Oxford/AstraZeneca, in terms of both technological and pharmaceutical formulation, safety, efficacy and immunogenicity. A critical analysis of phase 1, 2 and 3 clinical trial results available was conducted, comparing the three vaccine candidates, underlining their similarities and differences. All candidates showed consistent efficacy and tolerability; although some differences can be noted, such as their technological formulation, temperature storage, which will be related to logistics and costs. Further studies will be necessary to evaluate long-term effects and to assess the vaccine safety and efficacy in the general population. This article is protected by copyright. All rights reserved. This article is protected by copyright. All rights reserved.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        |
|34711598 |BCG vaccination to reduce the impact of COVID-19 in healthcare workers: Protocol for a randomised controlled trial (BRACE trial).                  |BMJ open                                                                                             |2021 10 28  |BCG vaccination modulates immune responses to unrelated pathogens. This off-target effect could reduce the impact of emerging pathogens. As a readily available, inexpensive intervention that has a well-established safety profile, BCG is a good candidate for protecting healthcare workers (HCWs) and other vulnerable groups against COVID-19. This international multicentre phase III randomised controlled trial aims to determine if BCG vaccination reduces the incidence of symptomatic and severe COVID-19 at 6 months (co-primary outcomes) compared with no BCG vaccination. We plan to randomise 10 078 HCWs from Australia, The Netherlands, Spain, the UK and Brazil in a 1:1 ratio to BCG vaccination or no BCG (control group). The participants will be followed for 1 year with questionnaires and collection of blood samples. For any episode of illness, clinical details will be collected daily, and the participant will be tested for SARS-CoV-2 infection. The secondary objectives are to determine if BCG vaccination reduces the rate, incidence, and severity of any febrile or respiratory illness (including SARS-CoV-2), as well as work absenteeism. The safety of BCG vaccination in HCWs will also be evaluated. Immunological analyses will assess changes in the immune system following vaccination, and identify factors associated with susceptibility to or protection against SARS-CoV-2 and other infections. Ethical and governance approval will be obtained from participating sites. Results will be published in peer-reviewed open-access journals. The final cleaned and locked database will be deposited in a data sharing repository archiving system. ClinicalTrials.gov NCT04327206. <U+00A9> Author(s) (or their employer(s)) 2021. Re-use permitted under CC BY. Published by BMJ.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                         |
|34704204 |COVID-19 Testing and Vaccine Acceptability Among Homeless-Experienced Adults: Qualitative Data from Two Samples.                                   |Journal of general internal medicine                                                                 |2021 Oct 26 |Homeless-experienced populations are at increased risk of exposure to SARS-CoV-2 due to their living environments and face an increased risk of severe COVID-19 disease due to underlying health conditions. Little is known about COVID-19 testing and vaccination acceptability among homeless-experienced populations. To understand the facilitators and barriers to COVID-19 testing and vaccine acceptability among homeless-experienced adults. We conducted in-depth interviews with participants from July to October 2020. We purposively recruited participants from (1) a longitudinal cohort of homeless-experienced older adults in Oakland, CA (n=37) and (2) a convenience sample of people (n=57) during a mobile outreach COVID-19 testing event in San Francisco. Adults with current or past experience of homelessness. We asked participants about their experiences with and attitudes towards COVID-19 testing and their perceptions of COVID-19 vaccinations. We used participant observation techniques to document the interactions between testing teams and those approached for testing. We audio-recorded, transcribed, and content analyzed all interviews and identified major themes and subthemes. Participants found incentivized COVID-19 testing administered in unsheltered settings and supported by community health outreach workers (CHOWs) to be acceptable. The majority of participants expressed a positive inclination toward vaccine acceptability, citing a desire to return to routine life and civic responsibility. Those who expressed hesitancy cited a desire to see trial data, concerns that vaccines included infectious materials, and mistrust of the government. Participants expressed positive evaluations of the incentivized, mobile COVID-19 testing supported by CHOWs in unsheltered settings. The majority of participants expressed a positive inclination toward vaccination. Vaccine hesitancy concerns must be addressed when designing vaccine delivery strategies that overcome access challenges. Based on the successful implementation of COVID-19 testing, we recommend mobile delivery of vaccines using trusted CHOWs to address concerns and facilitate wider access to and uptake of the COVID vaccine. <U+00A9> 2021. Society of General Internal Medicine.                                                                                                                                                                         |
|34703690 |A Rare Variant of Guillain-Barre Syndrome Following Ad26.COV2.S Vaccination.                                                                       |Cureus                                                                                               |2021 Sep    |Efforts to combat the global pandemic caused by severe acute respiratory syndrome coronavirus 2 (SARS-CoV-2) range from adequate diagnostic testing and contract tracing to vaccination for the prevention of coronavirus disease 2019 (COVID-19). In the United States alone, three vaccinations have been authorized for emergency use (EUA) or approved to prevent COVID-19. The Ad26.COV2.S vaccine by Johnson and Johnson (New Brunswick, New Jersey) is the only adenovirus-based vaccine and deemed relatively effective and safe by the US Food and Drug Administration (FDA) following its clinical trial. Since its introduction, the US FDA has placed a warning on the vaccine adverse event reporting system (VAERS) after more than 100 cases of Guillain-Barre Syndrome (GBS) were reported. Herein, we outline the hospital course of a generally healthy 49-year-old female who experienced an axonal form of GBS nine days after receiving the Ad26.COV2.S vaccine. Copyright <U+00A9> 2021, Morehouse et al.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                          |
|34702753 |Humoral immunogenicity of the seasonal influenza vaccine before and after CAR-T-cell therapy: a prospective observational study.                   |Journal for immunotherapy of cancer                                                                  |2021 10     |Recipients of chimeric antigen receptor-modified T (CAR-T) cell therapies for B cell malignancies have profound and prolonged immunodeficiencies and are at risk for serious infections, including respiratory virus infections. Vaccination may be important for infection prevention, but there are limited data on vaccine immunogenicity in this population. We conducted a prospective observational study of the humoral immunogenicity of commercially available 2019-2020 inactivated influenza vaccines in adults immediately prior to or while in durable remission after CD19-, CD20-, or B cell maturation antigen-targeted CAR-T-cell therapy, as well as controls. We tested for antibodies to all four vaccine strains using neutralization and hemagglutination inhibition (HAI) assays. Antibody responses were defined as at least fourfold titer increases from baseline. Seroprotection was defined as a HAI titer ≥40. Enrolled CAR-T-cell recipients were vaccinated 14-29 days prior to (n=5) or 13-57 months following therapy (n=13), and the majority had hypogammaglobulinemia and cellular immunodeficiencies prevaccination. Eight non-immunocompromised adults served as controls. Antibody responses to ≥1 vaccine strain occurred in 2 (40%) individuals before CAR-T-cell therapy and in 4 (31%) individuals vaccinated after CAR-T-cell therapy. An additional 1 (20%) and 6 (46%) individuals had at least twofold increases, respectively. One individual vaccinated prior to CAR-T-cell therapy maintained a response for &gt;3 months following therapy. Across all tested vaccine strains, seroprotection was less frequent in CAR-T-cell recipients than in controls. There was evidence of immunogenicity even among individuals with low immunoglobulin, CD19+ B cell, and CD4+ T-cell counts. These data support consideration for vaccination before and after CAR-T-cell therapy for influenza and other relevant pathogens such as SARS-CoV-2, irrespective of hypogammaglobulinemia or B cell aplasia. However, relatively impaired humoral vaccine immunogenicity indicates the need for additional infection-prevention strategies. Larger studies are needed to refine our understanding of potential correlates of vaccine immunogenicity, and durability of immune responses, in CAR-T-cell therapy recipients. <U+00A9> Author(s) (or their employer(s)) 2021. Re-use permitted under CC BY-NC. No commercial re-use. See rights and permissions. Published by BMJ. |
|34698827 |Measuring vaccine efficacy against infection and disease in clinical trials: sources and magnitude of bias in COVID-19 vaccine efficacy estimates. |Clinical infectious diseases : an official publication of the Infectious Diseases Society of America |2021 Oct 26 |Phase III trials have estimated COVID-19 vaccine efficacy (VE) against symptomatic and asymptomatic infection. We explore the direction and magnitude of potential biases in these estimates and their implications for vaccine protection against infection and against disease in breakthrough infections. We developed a mathematical model that accounts for natural and vaccine-induced immunity, changes in serostatus and imperfect sensitivity and specificity of tests for infection and antibodies. We estimated expected biases in VE against symptomatic, asymptomatic and any SARS<U+034F>CoV2 infections and against disease following infection for a range of vaccine characteristics and measurement approaches, and the likely overall biases for published trial results that included asymptomatic infections. VE against asymptomatic infection measured by PCR or serology is expected to be low or negative for vaccines that prevent disease but not infection. VE against any infection is overestimated when asymptomatic infections are less likely to be detected than symptomatic infections and the vaccine protects against symptom development. A competing bias towards underestimation arises for estimates based on tests with imperfect specificity, especially when testing is performed frequently. Our model indicates considerable uncertainty in Oxford-AstraZeneca ChAdOx1 and Janssen Ad26.COV2.S VE against any infection, with slightly higher than published, bias-adjusted values of 59.0% (95% uncertainty interval [UI] 38.4 to 77.1) and 70.9% (95% UI 49.8 to 80.7) respectively. Multiple biases are likely to influence COVID-19 VE estimates, potentially explaining the observed difference between ChAdOx1 and Ad26.COV2.S vaccines. These biases should be considered when interpreting both efficacy and effectiveness study results. <U+00A9> The Author(s) 2021. Published by Oxford University Press for the Infectious Diseases Society of America.                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                        |
|34697214 |Author Response: Guillain-Barré Syndrome in the Placebo and Active Arms of a COVID-19 Vaccine Clinical Trial.                                      |Neurology                                                                                            |2021 10 26  |NA                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                       |
## Test Mining



```r
pubmed <-"pubmed.csv"
if(!file.exists(pubmed))
  download.file("https://raw.githubusercontent.com/USCbiostats/data-science-data/master/03_pubmed/pubmed.csv",destfile = pubmed)
pubmed<-read.csv("pubmed.csv")
```
## 1. Tokenize the abstracts and count the number of each token. Do you see anything interesting? Does removing stop words change what tokens appear as the most frequent? What are the 5 most common tokens for each search term after removing stopwords?

```r
tokabs<-pubmed %>%
  unnest_tokens(token, abstract) %>%
  count(token) %>%
  top_n(5,n)
tokabs
```

```
##   token     n
## 1   and 19993
## 2    in 14653
## 3    of 24760
## 4   the 28126
## 5    to 10920
```
The top five frequent tokens are all preposition words.

```r
pubmed %>%
  unnest_tokens(output = word, input = abstract) %>%
  count(word,sort=TRUE)%>%
  anti_join(stop_words,by="word") %>%
  ## filter(!grepl(pattern = "^[0-9]+$", x = word))%>%
  top_n(5,n) 
```

```
##       word    n
## 1    covid 7271
## 2       19 7080
## 3 patients 4674
## 4   cancer 3999
## 5 prostate 3832
```
After remove stop words, the top five frequent tokens all relate to disease.

Now we will find out the five most frequent tokens with search terms "covid", "cystic fibrosis", "meningitis", "preeclampsia" and "prostate cancer".

```r
pubmed %>%
  filter(term=="covid")%>%
  unnest_tokens(output = token,input = abstract) %>%
  count(token, sort = TRUE) %>%
  anti_join(stop_words, by = c("token" = "word")) %>%
  top_n(5,n) %>%
  knitr::kable(caption = "Frequent Tokens with covid")
```



Table: Frequent Tokens with covid

|token    |    n|
|:--------|----:|
|covid    | 7271|
|19       | 7035|
|patients | 2293|
|disease  |  943|
|pandemic |  800|

```r
pubmed %>%
  filter(term=="cystic fibrosis")%>%
  unnest_tokens(output = token,input = abstract) %>%
  count(token, sort = TRUE) %>%
  anti_join(stop_words, by = c("token" = "word")) %>%
  top_n(5,n) %>%
  knitr::kable(caption = "Frequent Tokens with cystic fibrosis")
```



Table: Frequent Tokens with cystic fibrosis

|token    |   n|
|:--------|---:|
|fibrosis | 867|
|cystic   | 862|
|cf       | 625|
|patients | 586|
|disease  | 400|

```r
pubmed %>%
  filter(term=="meningitis")%>%
  unnest_tokens(output = token,input = abstract) %>%
  count(token, sort = TRUE) %>%
  anti_join(stop_words, by = c("token" = "word")) %>%
  top_n(5,n) %>%
  knitr::kable(caption = "Frequent Tokens with meningitis")
```



Table: Frequent Tokens with meningitis

|token      |   n|
|:----------|---:|
|patients   | 446|
|meningitis | 429|
|meningeal  | 219|
|csf        | 206|
|clinical   | 187|

```r
pubmed %>%
  filter(term=="preeclampsia")%>%
  unnest_tokens(output = token,input = abstract) %>%
  count(token, sort = TRUE) %>%
  anti_join(stop_words, by = c("token" = "word")) %>%
  top_n(5,n) %>%
  knitr::kable(caption = "Frequent Tokens with preeclampsia")
```



Table: Frequent Tokens with preeclampsia

|token        |    n|
|:------------|----:|
|pre          | 2038|
|eclampsia    | 1998|
|preeclampsia | 1863|
|women        | 1196|
|pregnancy    |  969|

```r
pubmed %>%
  filter(term=="prostate cancer")%>%
  unnest_tokens(output = token,input = abstract) %>%
  count(token, sort = TRUE) %>%
  anti_join(stop_words, by = c("token" = "word")) %>%
  top_n(5,n) %>%
  knitr::kable(caption = "Frequent Tokens with prostate cancer")
```



Table: Frequent Tokens with prostate cancer

|token     |    n|
|:---------|----:|
|cancer    | 3840|
|prostate  | 3832|
|patients  |  934|
|treatment |  926|
|disease   |  652|
## 2. Tokenize the abstracts into bigrams. Find the 10 most common bigram and visualize them with ggplot2.
Bi-grams

```r
bigabs<-pubmed %>%
  unnest_ngrams(bigram, abstract, n = 2) %>%
  count(bigram, sort = TRUE) %>%
  top_n(10,n)
bigabs
```

```
##             bigram    n
## 1         covid 19 6960
## 2           of the 3883
## 3           in the 3418
## 4  prostate cancer 3222
## 5    pre eclampsia 1847
## 6    patients with 1587
## 7         of covid 1519
## 8          and the 1154
## 9           to the 1061
## 10     of prostate  870
```
Plot

```r
bigabs %>%
  ggplot(aes(n, fct_reorder(bigram, n))) +
  geom_col()
```

![](HW3_files/figure-html/bigram-plot-1.png)<!-- -->
## 3. Calculate the TF-IDF value for each word-search term combination.

```r
pubmed %>%
  unnest_tokens(word, abstract) %>%
  count(word, term) %>%
  bind_tf_idf(word, term, n) %>%
  group_by(term) %>%
  arrange(desc(tf_idf), .by_group=TRUE) %>%
  top_n(5,tf_idf) %>%
  knitr::kable()
```



|word            |term            |    n|        tf|       idf|    tf_idf|
|:---------------|:---------------|----:|---------:|---------:|---------:|
|covid           |covid           | 7271| 0.0370280| 1.6094379| 0.0595942|
|pandemic        |covid           |  800| 0.0040740| 1.6094379| 0.0065569|
|coronavirus     |covid           |  647| 0.0032949| 1.6094379| 0.0053029|
|sars            |covid           |  372| 0.0018944| 1.6094379| 0.0030490|
|cov             |covid           |  333| 0.0016958| 1.6094379| 0.0027293|
|cf              |cystic fibrosis |  625| 0.0127144| 0.9162907| 0.0116501|
|fibrosis        |cystic fibrosis |  867| 0.0176374| 0.5108256| 0.0090096|
|cystic          |cystic fibrosis |  862| 0.0175357| 0.5108256| 0.0089577|
|cftr            |cystic fibrosis |   86| 0.0017495| 1.6094379| 0.0028157|
|sweat           |cystic fibrosis |   83| 0.0016885| 1.6094379| 0.0027175|
|meningitis      |meningitis      |  429| 0.0091826| 1.6094379| 0.0147788|
|meningeal       |meningitis      |  219| 0.0046876| 1.6094379| 0.0075444|
|pachymeningitis |meningitis      |  149| 0.0031893| 1.6094379| 0.0051329|
|csf             |meningitis      |  206| 0.0044093| 0.9162907| 0.0040402|
|meninges        |meningitis      |  106| 0.0022689| 1.6094379| 0.0036516|
|eclampsia       |preeclampsia    | 1998| 0.0142109| 1.6094379| 0.0228716|
|preeclampsia    |preeclampsia    | 1863| 0.0132507| 1.6094379| 0.0213262|
|pregnancy       |preeclampsia    |  969| 0.0068921| 0.5108256| 0.0035207|
|maternal        |preeclampsia    |  797| 0.0056687| 0.5108256| 0.0028957|
|gestational     |preeclampsia    |  191| 0.0013585| 1.6094379| 0.0021864|
|prostate        |prostate cancer | 3832| 0.0311735| 1.6094379| 0.0501718|
|androgen        |prostate cancer |  305| 0.0024812| 1.6094379| 0.0039933|
|psa             |prostate cancer |  282| 0.0022941| 1.6094379| 0.0036922|
|prostatectomy   |prostate cancer |  215| 0.0017490| 1.6094379| 0.0028150|
|castration      |prostate cancer |  148| 0.0012040| 1.6094379| 0.0019377|

## Conclusion: 
Only "prostate" is listed in the most frequent tokens with "prostate cancer".   

"eclampsia", "preeclampsia" and "pregnancy" are the most frequent tokens of "preeclampsia".   

"meningitis", "meningeal" and "csf" are the most frequent tokens of "meningitis".  

"cf", "fibrosis" and "cystic" are the most frequent tokens of "cystic fibrosis".  

"covid" and "pandemic" are the most frequent tokens of "covid".



