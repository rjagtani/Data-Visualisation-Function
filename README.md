# Data-Visualisation-Function
# Data-Visualisation-Function A function which takes the dataset as an argument and generates univariate and bivariate plots for each variable.


Example : 

dataviz(df=df1,exclude_cols = c('PassengerId','Ticket','Name'),categorical_cols = c('Survived','Pclass'),datetime_format = '%m/%d/%Y',directory = 'C:\\Users\\Desktop',corr=FALSE)
