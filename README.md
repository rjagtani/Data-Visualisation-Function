# Data-Visualisation-Function - dataviz
A function which takes the dataset as an argument and generates publication quality univariate (for each variable) and bivariate plots (for each variable pair : Discrete-Discrete,Continuous-Continuous,Discrete-Continuos,Time-Continuous) using ggplot2. Provides option to generate plots for user selected variables as well.

Types of Univariate Plots : 

1. Continuous : Histogram, Density plot & Boxplot
2. Discrete : Barplot

Types of Bivariate Plots : 
1. Continuous-Continuous : Scatter plot with trend line, Correlation matrix & heatmap
2. Discrete-Continuous : Boxplot, Violin Plot
3. Discrete-Discrete : Jitter Plot
4. Time-Continuous : Line plot,Scatter plot

Arguments : 

1. df - Dataframe containing variables
2. exclude_cols - Character vector of column names to exclude;optional
3. datetime_cols - Character vector of columns to be considered as Date variables;optional
4. datetime_format - format if date variables are present;optional
5. corr - whether correlation matrix is required for continuous-continuos variables; default is TRUE
6. selected_plots - whether user will provide selection of variables for which plots will be generated; if T a dialog box will open asking user to enter a CSV with variables in the first column as X axis and variables in the second column as y axis. Default is FALSE
7. required_plots - selected variables as dataframe;to be used if user wants to provide selection of variables using a dataframe;optional
8. univariate - Whether univariate plots are required; default is TRUE

Demo Function Call : 
df1=read.csv('superstores.csv')
dataviz(df=df1,exclude_cols=c('Country','Order.ID','Customer.ID','Customer.Name','Product.ID','Product.Name','Row.ID','Postal.Code'),datetime_format = '%m/%d/%Y',corr=T,required_plots = NULL,univariate = T,selected_plots=F)

Demo Output provided in repository;Directory : Plots


