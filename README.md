# Eduvos Graduate Tech Trends Dashboard

## Overview
Interactive dashboard analyzing technology tool adoption and career outcomes of Eduvos graduates. Built with R Shiny and Plotly, this tool allows users to explore data through dynamic visualizations and filters.

## Features
- **Interactive Filters**: Campus and Study Field selection
- **Real-time Updates**: Visualizations respond to filter changes
- **Multi-format Visuals**: Bar charts, dot plots, and pie charts for tool analysis
- **Career Insights**: Employment rates, industry distribution, and role popularity
- **Data Transparency**: Raw data viewer with export capabilities

## Getting Started
1. **Prerequisites**:
   - R (version 4.0+)
   - RStudio (recommended)

2. **Installation**:
   ```r
   install.packages(c("shiny", "shinydashboard", "plotly", "DT", "dplyr", "tidyr", "RColorBrewer"))
   
3 .Data Preparation :
  Create a data folder in your working directory
  Place cleaned_data.csv in the data folder
  Ensure your file structure looks like:

4. Usage Instructions
**Sidebar Controls**
    -Campus Filter : Select one or multiple campuses
    -Study Field Filter : Select one or multiple fields of study
    -Filters automatically update all visualizations
   
**General Overview Tab**
   - Campus distribution bar chart
   - Study field distribution pie chart
   - Total respondents counter
   - Top 3 campuses comparison
    
**Employment Trends Tab**
   - Employment status breakdown
   - Industry distribution analysis
   - Role popularity visualization
    
**Tech Tools Tab**
   - For each tool category (Programming Languages, Databases, etc.):
   - Use the dropdown to select specific tools
   - Choose visualization type (Bar/Dot/Pie)
   - Hover over charts for detailed tooltips
   - Click legend items to show/hide categories
    
**Raw Data Tab**
 - View full dataset
 - Export data using buttons: CSV, Excel, PDF, etc.

**Technical Details**
  - Built with: shiny, plotly, dplyr, tidyr
  - Color schemes: RColorBrewer palettes
  - Data processing: Uses tech_summary() function to aggregate tool usage
  - Responsive design: Works in modern browsers
