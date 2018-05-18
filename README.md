# elemSource
R Package for sourcing elements

### Warning: Currently in development:
Only works with obsidian data calibrated by Bruker software with data formatted 
with the artifact/source in the first column the 10 elements from the calibration in 
the next columns, the source (site name or obsidian source) in the next column, and
the type in the next column (can only be 'Source' or 'Artifact).

## Example

#### Install
Two packages must be installed prior to installing elemSource.
One package is optional, but must be installed if using the shiny apps.
64 bit Java must be installed for the rJava package to function (https://www.java.com/en/download/manual.jsp).

```{r echo = False}
install.packages("devtools")
library(devtools)
install_github("vqv/ggbiplot")
install.packages('plotly') # optional !Do not install development version; It causes an issue with ggbiplot!
install_github('bischrob/elemSource')
library(elemSource)
```

#### Get data

```{r echo = False}
df <- selectData()
```
#### Source data

```{r echo = False}
df <- elemSource(df)
```
#### Run plots

```{r echo = False}
windows(12,12) # creates pop up window for easier viewing of the scatterPlots function
scatterPlots(df) # runs all possible combinations of plots for quick viewing
allBiplots(df) # saves all combinations of biplots to Figures folder and subfolder with today's date
plotPCA(df) # saves principle components plot
runBiplotApp(df) # launches shiny app for biplots
runTriplotApp(df) # launches shiny app for triplots (ternary graphs)
```
