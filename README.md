# Legitimation Strategies of Transnational Private Institutions

Replication code for:

> Bjorkholt, Solveig. (2026). "Legitimation Strategies of Transnational Private Institutions: Evidence From the International Organization for Standardization." *Regulation & Governance*. DOI: [10.1111/rego.70123](https://onlinelibrary.wiley.com/doi/full/10.1111/rego.70123)

Replication data available at: [Harvard Dataverse](https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/E43V2B)

## Overview

This project investigates how the International Organization for Standardization (ISO) deploys different **legitimation strategies** depending on the type of standard being produced. It distinguishes between:

- **Technocratic legitimation**: justifying standards by appealing to expertise, efficiency, scientific knowledge, and innovation.
- **Democratic legitimation**: justifying standards by appealing to participation, representation, equity, sustainability, and stakeholder engagement.

The paper tests whether **physical standards** (technical specifications, interoperability) are associated with technocratic legitimation, while **societal standards** (safety, quality, social responsibility) are associated with democratic legitimation. The analysis operates on two dimensions:

- **Input legitimation**: characteristics of who participates in standards development (R&D capacity of member countries; diversity of committee membership).
- **Output legitimation**: the language used in ISO news articles to promote standards (dictionary-based text analysis and GPT-coded legitimation statements).

## Repository Structure

```
.
├── README.md
├── .gitignore
└── scripts/
    ├── 00_scraping/           # Data collection
    │   ├── News_Selenium_Scrape.R   # Scrapes ISO news articles from iso.org
    │   └── parse_scopus.R           # Parses Scopus API JSON into tidy data
    │
    ├── 01_classification/     # LLM-based coding/classification
    │   ├── 1_1_GPT_coding_liaison.R         # Classifies liaison organizations (GPT-4)
    │   ├── 1_2_GPT_coding_standards.R       # Classifies standards as physical/societal (OpenAI)
    │   ├── 1_2_GPT_coding_standards_new.R   # Updated version using Responses API
    │   └── 1_3_GPT_coding_legitimacy.R      # Codes news sentences as democratic/technocratic (GPT-4-mini)
    │
    ├── 02_data_preparation/   # Data merging and feature engineering
    │   └── 2_Make_data.R              # Merges standards DB with classifications,
    │                                  # computes cumulative physical/societal counts
    │
    ├── 03_models/             # Statistical models (core analysis)
    │   ├── 3_1_Models_input_technocratic.R        # Input legitimation: R&D indicators ~ standard type
    │   ├── 3_2_Models_input_democratic.R          # Input legitimation: diversity indices ~ standard type
    │   ├── 4_0_Models_output_data_generation.R    # Prepares news text + legitimation coding for output models
    │   ├── 4_1_Models_output_technocratic.R       # Output legitimation: technocratic dictionary shares ~ standard type
    │   ├── 4_2_Models_output_democratic.R         # Output legitimation: democratic dictionary shares ~ standard type
    │   └── 4_3_Dictionary_stats.R                 # Summary statistics for text dictionaries
    │
    └── 04_results/            # Tables, figures, and validation
        ├── 5_1_Regression_tables.R            # Main regression tables (LaTeX)
        ├── 5_2_Coef_plot_standards.R          # Main coefficient plot (Figure 2)
        ├── 5_Coef_plot_standards.R            # Simplified coefficient plot (earlier version)
        ├── 5_3_Benjamin_Hochberg_adjustment.R # Benjamini-Hochberg FDR correction
        ├── 6_Descriptives.R                   # Descriptive statistics tables
        ├── 7_GPT_examples.R                   # Example table of GPT-classified standards
        ├── 8_GPT_validation.R                 # Validation: GPT coding vs. SDGs and TC sectors
        └── Figure1.R                          # Figure 1: physical vs. societal standards over time
```

## Analysis Pipeline

The scripts are designed to run sequentially by number prefix. The results scripts (`5_*`) automatically `source()` the model scripts they depend on.

```
00_scraping/         Data collection (ISO news, Scopus papers)
       |
01_classification/   LLM classification of organizations, standards, and legitimation
       |
02_data_preparation/ Merge and compute features
       |
03_models/           Fixed-effects panel regressions (feols)
       |
04_results/          Tables, coefficient plots, descriptive statistics
```

### Models

All models use the `fixest` package with:
- **Fixed effects**: Year + Technical Committee
- **Clustered standard errors**: by Committee and Year
- **Coverage**: 2004--2022 (input) / 1999--2022 (output)

| Model Family | DV | IV | Script |
|---|---|---|---|
| Technocratic Input | R&D/GDP, Researchers, High-tech exports, Uni-industry collab., Articles, Business GERD, Knowledge employment | Physical / Societal standard count | `3_1` |
| Democratic Input | Rae Index, Shannon H (regional & sectoral diversity) | Physical / Societal standard count | `3_2` |
| Technocratic Output | Share of technocratic words (from research papers, patents dictionaries) + GPT-coded technocratic statements | Physical / Societal standard indicator | `4_1` |
| Democratic Output | Share of democratic words (from UNGA debates, CSR dictionaries) + GPT-coded democratic statements | Physical / Societal standard indicator | `4_2` |

## Data Requirements

The scripts expect data in a directory outside the repository (referenced via relative paths like `../../../Legitimacy/Data/`). You can configure this by modifying the paths at the top of each script. The key data files are:

| File | Description |
|---|---|
| `iso_standards.sqlite` | ISO standards database (standards metadata, committee membership, liaison organizations) |
| `news.rds` | Scraped ISO news articles |
| `legitimation_output6.rds` | GPT-coded legitimation labels for news sentences |
| `standards2.rds` / `standards3.rds` | Processed standard type classifications |
| `organizations_tagged.rds` | GPT-classified liaison organizations |
| `scopus_results.rds` | Scopus research paper abstracts |
| `rndgdp.csv`, `researchers.csv`, `WIPO-GII.xlsx` | Country-level R&D and innovation indicators |
| `TXT/` directory | UNGA General Debate transcripts (for democratic dictionary) |

Replication data is available from the [Harvard Dataverse](https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/E43V2B).

## Software Requirements

R (>= 4.0) with the following packages:

- **Data wrangling**: `tidyverse`, `DBI`, `RSQLite`, `readxl`, `readtext`
- **Text analysis**: `tidytext`, `quanteda`, `SnowballC`, `lexicon`
- **Modeling**: `fixest`, `broom`
- **Tables and figures**: `modelsummary`, `kableExtra`, `ggplot2`, `forcats`
- **API access**: `httr`, `jsonlite`
- **Web scraping**: `RSelenium` (requires Docker with `selenium/standalone-chrome`)
- **Package management**: `pacman`

Install all at once:

```r
install.packages(c(
  "tidyverse", "DBI", "RSQLite", "readxl", "readtext",
  "tidytext", "quanteda", "SnowballC", "lexicon",
  "fixest", "broom",
  "modelsummary", "kableExtra", "forcats",
  "httr", "jsonlite",
  "RSelenium", "pacman"
))
```

## API Keys

The classification scripts require API keys set as environment variables:

```r
# In your .Renviron file (never committed to git):
OPENAI_API_KEY=sk-...
SCOPUS_API_KEY=...
```

The classification scripts (`01_classification/`) are provided for transparency and reproducibility documentation. Their outputs (the classified data) are included in the replication dataset on Harvard Dataverse, so re-running the LLM classification is not required to reproduce the paper's results.

## How to Reproduce

1. Download the replication data from [Harvard Dataverse](https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/E43V2B).
2. Place the data files so that the relative path `../../../Legitimacy/Data/` points to the data directory (or modify the paths in the scripts).
3. Set your working directory to the repository root.
4. Run the results scripts (e.g., `source("scripts/04_results/5_1_Regression_tables.R")`) -- these will automatically source upstream model scripts.

## Citation

```bibtex
@article{bjorkholt2026legitimation,
  title={Legitimation Strategies of Transnational Private Institutions: Evidence From the International Organization for Standardization},
  author={Bj{\o}rkholt, Solveig},
  journal={Regulation \& Governance},
  year={2026},
  doi={10.1111/rego.70123}
}
```

## Related Work

Bjorkholt, S. (2025). "Presenting the Standat Database on International Standards: Improving Data Accessibility on Marginal Topics." *Political Science Research and Methods*, 1--19.
