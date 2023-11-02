# Sea Surface Temperature

Title: Sea surface temperature

Date: 1981 - 2016

Description: This little pictures depicts the increasing minimum and maximum sea surface temperatures between 1981 and 2016, highlighting that temperatures that used to a maximum are now considered the minimum.

## Data

The data relates to Sea Surface Temperature, and contains monthly means of global sea surface temperature from 1981 to 2016. The data is given in Celsius. The data can be found on [GitHub](https://raw.githubusercontent.com/littlepictures/datasets/main/sst/monthly_global_sst_mean.csv). For more information on this dataset see: [catalogue.ceda.ac.uk/uuid/62c0f97b1eac4e0197a674870afe1ee6](https://catalogue.ceda.ac.uk/uuid/62c0f97b1eac4e0197a674870afe1ee6).

## Methods

The data was processed using R version 4.3.0. Column names were tidied and date columns converted to date objects. For each year in the data set, the minimum and maximum monthly sea surface temperatures were extracted. These values were then plotted as a ribbon plot using {ggplot2} to show how the minimum and maximum values have changed over time. The plot was then exported as a PDF to allow further editing in Inkscape. A gradient colour scheme was applied to the ribbon area in Inkscape, with red showing higher temperatures and blue showing lower temperatures. Diverging gradient shading is not currently available for ribbon plots in R. A background colour was added when exporting to PDF.

The R script can be found at [github.com/nrennie/data-viz-projects/blob/main/ESA%20Little%20Pictures/esa-little-pictures.R](https://github.com/nrennie/data-viz-projects/blob/main/ESA%20Little%20Pictures/esa-little-pictures.R).

The SVG file edited in Inkscape can be found at [github.com/nrennie/data-viz-projects/blob/main/ESA%20Little%20Pictures/esa-little-pictures.svg](https://github.com/nrennie/data-viz-projects/blob/main/ESA%20Little%20Pictures/esa-little-pictures.svg).