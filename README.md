# scenTool
scenTool allows to explore and visualize time series of modelling results

scenTool can read CSV files compatible with the Modelling Intercomparison Format (MIF). See testdata.csv for an example.

scenTool 0.1 has the following features:
- read CSV/MIF files
- interactive selection of Model(s), Scenario(s), Region(s), Variable(s) and Year(s)
- visualize the selected time series data with ggplot2 and plotly
- plot options: line, bar or area plot; bar and area plots can be stacked;
- customizable figure format: the dimensions Model, Scenario, Region and Variable can be assigend to colors, horizontal boxes or vertical boxes
- show interactive data table with column filters and search feature
- show statistics about the data set
- option to download plot as PDF and corresponding data set as CSV/MIF file

Technical notes:
- the data management is based on data tables, which is much faster (about 10x for large data sets) than data frames
- the tools has been sucessfully tested with CSV/MIF files of about 125 MB

Ideas/future plans:
- add option to save data selection and plot format
- add pre-defined plot types
- add more statistical indicators
- add uncertainty bands and error bars
- connect to database to retrieve data?

