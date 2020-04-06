# BioAnalyst

![Alt text](www/Wei2Go_logo.png?raw=true "Title")

A public shiny app that is used to do basic data analysis for biological data.
1. Metadata table, filtering by conditions, stats(hist for continuous variable and pie plot for discrete variable), NA filling, regression analysis, corelation analysis and Chi-square test between 2 categorical variable.
2. Feature table, including features about gene, microbiome, metabolites. NA stats by sample or by feature, filter samples or features by NA proportion, NA filling(constant, row mean, col mean), data transform(log, log10,log2,...). Besides, PCA by variable in metadata table, complex heatmap, differential analysis.
3. Machine learning, including data from metadata table and feature table, or you can just upload another tables. Spliting dataset, model configuration(cv, metric, ...), model choice and specification. The results will show AUC plot and confusion matrix.

The application is accesable on shiny.io, https://w4356y.shinyapps.io/BioAnalyst/
## Instalation
You can install the package with the following command：
```
devtools::install_github("w4356y/BioAnalyst")
```

## Example
An example of running this app:
```
library(BioAnalyst)
runApp()
```