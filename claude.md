# ISP Analysis Project

**Author:** Ryan Batchelor
**Last Updated:** 23 November 2025

## Project Overview

This project analyzes Australia's Integrated System Plan (ISP) data, focusing on electricity generation and storage capacity, output, and scenario comparisons. The project includes an ETL pipeline for processing ISP data and a Shiny dashboard for interactive visualization.

## Project Structure

```
isp-analysis/
├── raw-data/           # Raw data downloads from ISP sources
├── data/               # Processed .rda data files (loaded by Shiny app)
├── prep/               # Data preparation scripts (dropdowns, etc.)
├── processing/         # ETL scripts for processing ISP data
│   ├── 00-utils.R
│   ├── 01-load-isp-generation-capacity.R
│   ├── 02-load-isp-generation-output.R
│   ├── 03-storage-capacity.R
│   ├── 04-storage_output.R
│   ├── 05-retirements.R
│   └── 06-generator-information.R
├── R/                  # Reusable functions and utilities
│   ├── chart_generation_output.R
│   ├── chart_generator_capacity.R
│   ├── extract-isp-generation-storage-data.R
│   └── run-all-r-scripts.R
├── explore/            # Ad-hoc exploration and analysis scripts
├── shiny-webtool/      # Shiny dashboard application
│   ├── app.r          # Main Shiny app (single-file structure)
│   └── brand/         # Brand configuration (YAML theming)
├── setup.r            # Package loading and environment setup
└── driver.r           # Main execution script
```

## Key Technologies

### R Packages
- **Data manipulation:** tidyverse, janitor, lubridate
- **Visualization:** ggplot2, ggpattern, plotly, patchwork, scales
- **Shiny:** shiny, shinyWidgets, bslib, thematic
- **Data import:** readxl, rvest (web scraping)
- **Web automation:** chromote
- **Other:** yaml, glue, forcats

### Shiny Framework
- Uses modern `bslib` theming with brand YAML configuration
- Single-file app structure (`app.r` with ui/server/shinyApp)
- Multi-page navigation with `page_navbar()`
- Responsive cards with `card()` and full-screen capability

## Data Pipeline Workflow

1. **Setup** (`setup.r`): Load packages and configure environment
2. **Processing** (`processing/` scripts): ETL pipeline
   - Scripts numbered 00-06 for sequential execution
   - Output saved as .rda files in `data/` directory
3. **Shiny App** (`shiny-webtool/app.r`):
   - Loads all .rda files from `data/`
   - Sources all functions from `R/`
   - Displays interactive visualizations

## Shiny App Structure

The app has four main sections:
1. **Generation capacity & output** - Generator capacity and output analysis
2. **Storage capacity & output** - Energy storage analysis
3. **Compare scenarios** - Scenario comparison tools
4. **Settings** - App configuration

### Current State
- App skeleton is in place with navigation structure
- Transitioning from old multi-file structure (ui.r, server.r, global.r) to single-file app.r
- UI panels are placeholders awaiting implementation
- Brand theming configured via `brand/_brand.yml`

## Coding Conventions

### File Naming
- Processing scripts: Numbered with leading zeros (e.g., `01-load-isp-generation-capacity.R`)
- Functions: Descriptive snake_case names (e.g., `chart_generator_capacity.R`)
- Temporary/backup files: Prefixed with `bak_`

### Code Style
- Use tidyverse conventions (pipes `%>%` or `|>`)
- Section headers with comment blocks using `# =====`
- Epsilon value (`1E-5`) for floating-point comparisons

### Data Files
- Processed data saved as `.rda` files in `data/` directory
- Raw data downloaded to `raw-data/` directory

## Common Tasks

### Running the Shiny App
```r
# From project root
shiny::runApp("shiny-webtool")
```

### Running ETL Pipeline
```r
source("setup.r")
source("R/run-all-r-scripts.R")
# Or run individual processing scripts as needed
```

### Adding New Visualizations
1. Create chart function in `R/chart_*.R`
2. Source in app.r (automatically loaded from `R/` directory)
3. Add to appropriate UI panel and wire up in server function

## Git Workflow

- Main branch: `main`
- Current branch: `shiny-dash-update` (app restructuring)
- Recent work: Full review of ETL scripts, adding net additions tab

## Notes

- The project analyzes ISP (Integrated System Plan) data for Australian energy scenarios
- Data is scraped/downloaded from ISP sources using rvest and chromote
- Epsilon value set to `1E-5` for comparing floating-point values
- Generator information stored separately in `raw-data/generator_information/`
