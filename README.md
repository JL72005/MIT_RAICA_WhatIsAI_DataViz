# MIT_RAICA_WhatIsAI_DataViz
two data visualizations based on student responses to the question "What is AI"?
- Note: these files are R-shiny applications that are already deployed on the web through these links:
  * https://jl140miteducation.shinyapps.io/RAICA_DataViz_v7/
  * second one coming soon!
## Files
### data_viz2_v1.R
This is the second R-Shiny File for the data set that contains 151 responses from students. The dashboard composed of two graphs:
  * a rectangular distribution that showcases the counts and percentages of each tag
  * all 150 responses plotted as circles on a 5 x 10 grid that can be filtered for by tag and by tag count. The bigger the circle is, the more tags the observations had. Hover over the circles to see the tags and the response.

### v7final.R
This is the other data visualization that plots seven student responses to the question over five administrations. The dashboard is composed of two parts: 
  * a grid that is split into rectangles.
     * each rectangle is a student response
     * each of the circles in the rectangles is a tag given to the response
     * hover over the dots to see the tags and the response.
  * a heatmap of the frequency of tags across the administration levels.

### updated_qualitative_coding_with_extra_columns
This file contains a link to the spreadsheet of the data for the data_viz2_v1.R file. Must request access to it to download (to run the R file) if not already given. 

### tag_freq.csv 
This csv file contains a list of all of the tags, the counts for each of them, and the percentage each tag takes up out of the total tags given for all 150 observations (351) 
