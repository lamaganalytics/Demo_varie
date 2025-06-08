# Shiny Morphological KPI Demo

This is a simple **Shiny** web application that simulates and visualizes morphological KPIs for quality monitoring in a production context.

## ✨ Features

- Simulates production data with key morphological measurements.
- Displays two control charts using **XmR methodology**:
  - **Morphological_KPI_1**: .
  - **Morphological_KPI_2**: .
- Allows selection of **quality grade** (`Grade1` or `Grade2`) with different thresholds.
- Adjustable time window for simulation (from 1 to 24 hours).

## 📦 Requirements

- R (version ≥ 4.0)
- R packages:
  - `shiny`
  - `shinyTime`
  - `ggplot2`
  - `dplyr`
  - `tibble`
  - `lubridate`
  - `ggQC`

## 🚀 How to Run

Clone the repository and run the app from RStudio or R terminal:

```bash
git clone https://github.com/lamaganalytics/shiny-morphological-kpi-demo.git
