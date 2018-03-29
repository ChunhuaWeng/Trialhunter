# TrialHunter

TrialHunter is an R shinny app designed to generate dynamic questionnaire for efficient patient search of trials. This is a demo to show how it works. This demo only provide the search for clinical trials related to Alzheimer's diseases.

## Getting Started

These instructions will get you a copy of the project up and running on your local machine for development and testing purposes. See deployment for notes on how to deploy the project on a live system.

### Prerequisites

R and required R package should be installed before using the app.

```
install.packages("dplyr")
library("shiny")
library("stringr")
library("tidyr")
library("DT")
```

### Start Shinny Server

You could either start an Shinny server on Rstudio or lauch it from command line.
```
R -e "shiny::runApp('TrialHunterNCTServer.R')"
```

## Contributing

Cong Liu, Chi Yuan,and Chunhua Weng

## Versioning
v_0.1.0

## Future work
- Better NLP for concept extraction.
- Concept clustering.
- Add error penalty in optimization function.
- Add prior distribution in optimization function.
- Expand to other diseases.


