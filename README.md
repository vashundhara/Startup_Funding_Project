# Startup Funding Project

## Overview
This project analyzes startup funding data and visualizes key trends using **R**. It also includes a **Shiny dashboard** for interactive exploration of funding by city, year, and industry.

**Key features:**
- Total funding trends per year
- Top 10 funded cities
- Top 10 funded industries
- Funding by investment type
- Top 10 funded startups
- Interactive Shiny dashboard

---

## Folder Structure

Startup_Funding_Project/
├── data/ # CSV dataset
│ └── your_data.csv
├── images/ # Plots and visualizations
├── shiny_app.R # Full R code (analysis + Shiny app)
├── .gitignore # Ignored files
└── README.md # Project overview


---

## Installation & Requirements

1. Install **R** and **RStudio** (if not already installed).  
2. Install required packages:

```
install.packages(c("shiny", "dplyr", "ggplot2"))


Ensure your dataset your_data.csv is in the data/ folder.

How to Run
1. Run Analysis Plots

Open shiny_app.R in RStudio.

Run the script to generate all plots (saved automatically in images/).

2. Launch Shiny Dashboard

Open shiny_app.R in RStudio.

Click Run App in the top-right corner.

Use the sidebar to select City and Year range to explore funding trends and top industries interactively.Visualizations
Total Funding per Year

Top 10 Cities by Funding

Top 10 Industries by Funding

Funding by Investment Type

Top 10 Funded Startups




Author

Vasundhara Gour

---

