# Construction Sites and Parking Tickets in Toronto

## Overview

This repository studies whether street blocks in Toronto receive more parking tickets once construction starts on them. About 32 million parking tickets issued between 2010 and 2025 (excluding 2020 and 2021) are placed on the city's street network, and blocks where a new building or demolition started are compared with matched nearby blocks that had no construction, using a difference-in-differences design. All data come from Open Data Toronto.

## File structure

- `data/simulated_data` contains the simulated tickets, construction sites and block-by-month panel.
- `data/raw_data` contains the data as downloaded from Open Data Toronto: parking tickets (one Parquet file per year), building permits, address points, the street centreline, former municipality boundaries and green spaces.
- `data/analysis_data` contains the cleaned data and the results of the analysis. The cleaned tickets (`data/analysis_data/parking_tickets`, about 600 MB) are not included in the repository; they are rebuilt by `scripts/02-clean_data.R`.
- `models` contains the fitted difference-in-differences models.
- `other` contains the LLM chat history (`llm_usage`) and related literature (`literature`).
- `paper` contains the Quarto document, the bibliography and the PDF of the paper.
- `scripts` contains the R scripts used to simulate, download, clean, test and analyse the data.

## Reproducing the paper

Open `toronto_open_data_paper.Rproj` and run the scripts in order:

| Script | What it does | Approximate time |
|---|---|---|
| `00-simulate_data.R` | Simulates the three datasets | < 1 minute |
| `01-download_data.R` | Downloads all data from Open Data Toronto | 5-10 minutes |
| `02-clean_data.R` | Cleans tickets and permits and places them on street blocks | 5 minutes |
| `03-test_simulated_data.R` | Tests the simulated data | < 1 minute |
| `04-test_analysis_data.R` | Tests the cleaned data and the analysis results (run after 06) | 1 minute |
| `05-prepare_paper_data.R` | Prepares the map layers and summary tables used in the paper | 3 minutes |
| `06-construction_analysis.R` | Matches construction blocks with nearby blocks and estimates the difference-in-differences | 5 minutes |

Then render `paper/paper.qmd` to PDF. The paper reads only the saved files in `data/analysis_data`; it does not download anything.

`opendatatoronto` checks for an internet connection before downloading. On some networks this check fails even though the portal is reachable, and the download stops with "`opendatatoronto` does not work offline". Running the script on another network avoids this.

The R packages used are `tidyverse`, `opendatatoronto`, `arrow`, `sf`, `jsonlite`, `fixest`, `testthat`, `here`, `hexbin` and `tinytable`.

## Statement on LLM usage

Claude Code (Anthropic) was used throughout this project. It wrote the R scripts (simulation, download, cleaning, tests and analysis) and the Quarto code for the figures and tables, and it improved the given text for parts of the paper. It was also used to check grammar in text written by the author. The research questions, the choice of analyses and the final text were decided by the author. The entire chat history is available in `other/llm_usage/usage.txt`.
