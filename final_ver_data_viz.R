### Dependencies 
 
library(ggplot2)
library(reshape2)

#### Univariate plots

univariate_histogram=function(data,x,dir=getwd())
{
  a=ggplot(data,aes(get(x))) 
  a + geom_histogram(bins=20,color='black',fill='dodgerblue4') + xlab(x) + ylab('Frequency')
  ggsave(paste0(dir,'/Plots/Univariate/Histograms/',x,'.jpeg'))
}

univariate_density=function(data,x,dir=getwd())
{
  a=ggplot(data,aes(get(x))) 
  a + geom_density(color='black',fill='dodgerblue4') + xlab(x) + ylab('Density')
  ggsave(paste0(dir,'/Plots/Univariate/Density/',x,'.jpeg'))
}

univariate_boxplot=function(data,x,dir=getwd())
{
  a=ggplot(data,aes(y=get(x),x="")) 
  a + geom_boxplot(fill='dodgerblue4',color='black') + xlab('') + ylab(x)
  ggsave(paste0(dir,'/Plots/Univariate/Boxplots/',x,'.jpeg'))
}  

univariate_barplot=function(data,x,dir=getwd())
{
  a=ggplot(data,aes(get(x))) 
  a + geom_bar(color='black',fill='dodgerblue4') + xlab(x) + ylab('Frequency')
  ggsave(paste0(dir,'/Plots/Univariate/Barplots/',x,'.jpeg'))
}

### Bivariate plots 



bivariate_scatter=function(data,x,y,dir=getwd()) {
  b=ggplot(data,aes(get(x),get(y)))
  b + geom_smooth(method='lm') + geom_point(color='dodgerblue4') + xlab(x) + ylab(y)
  ggsave(paste0(dir,'/Plots/Bivariate/Continuous-Continuous/Scatter/',x,'/',x,'_',y,'.jpeg'))
}

bivariate_jitter=function(data,x,y,dir=getwd()) {
  b=ggplot(data,aes(get(x),get(y))) 
  b + geom_jitter(color='dodgerblue4') + xlab(x) + ylab(y)
  ggsave(paste0(dir,'/Plots/Bivariate/Discrete-Discrete/',x,'/',x,'_',y,'.jpeg'))  
}

bivariate_boxplot=function(data,x,y,dir=getwd()) {
  b=ggplot(data,aes(x=get(x),y=get(y)))
  b + geom_boxplot(fill='dodgerblue4',color='black') + xlab(x) + ylab(y)
  ggsave(paste0(dir,'/Plots/Bivariate/Discrete-Continuous/Boxplot/',x,'/',x,'_',y,'.jpeg'))
}


bivariate_violin=function(data,x,y,dir=getwd()) {
  b=ggplot(data,aes(x=get(x),y=get(y)))
  b + geom_violin(fill='dodgerblue4',color='black') + xlab(x) + ylab(y)
  ggsave(paste0(dir,'/Plots/Bivariate/Discrete-Continuous/Violin_Plot/',x,'/',x,'_',y,'.jpeg'))
}

timeseries_line=function(data,x,y,date_range,dir=getwd())
{
  ggplot(data,aes(get(x),get(y))) + scale_x_date(date_breaks = paste0(date_range,' days')) + geom_line(color='dodgerblue4') + xlab(x) + ylab(y)
  ggsave(paste0(dir,'/Plots/Bivariate/Time Series/Line/',x,'/',x,'_',y,'.jpeg'))
}

timeseries_scatter=function(data,x,y,date_range,dir=getwd())
{
  ggplot(data,aes(get(x),get(y))) + scale_x_date(date_breaks = paste0(date_range,' days')) + geom_point(color='dodgerblue4') + xlab(x) + ylab(y)
  ggsave(paste0(dir,'/Plots/Bivariate/Time Series/Scatter/',x,'/',x,'_',y,'.jpeg'))
}


correlation_heatmap=function(data,numerical_columns,dir=getwd())
{
  num_data=data[,numerical_columns]
  cormat=round(cor(num_data,use='pairwise.complete.obs'),2)
  melted_cormat <- melt(cormat)
  head(melted_cormat)
  ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
    geom_tile(color = "white")+
    scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                         midpoint = 0, limit = c(-1,1), space = "Lab", 
                         name="Pearson\nCorrelation") + xlab('Variable 2') + ylab('Variable 1')
  write.csv(cormat,paste0(dir,'/Plots/Bivariate/Continuous-Continuous/Correlation/corr_matrix.csv'),row.names = T)
  ggsave(paste0(dir,'/Plots/Bivariate/Continuous-Continuous/Correlation/corr_matrix.jpeg'))
}


##### Final Function

dataviz=function(df,exclude_cols=NULL,numerical_cols=NULL,categorical_cols=NULL,datetime_cols=NULL,datetime_format=NULL,max.cat=5,directory=getwd(),corr=TRUE)
{
  df=as.data.frame(df)
  if(length(numerical_cols)>0)
  {
    df[,numerical_cols]=lapply(as.data.frame(df[,numerical_cols]),function(x) as.numeric(as.character(x)))
  }
  if(length(categorical_cols)>0)
  {
    df[,categorical_cols]=lapply(as.data.frame(df[,categorical_cols]),as.character)
  }
  if(length(datetime_cols)>0 & !is.null(datetime_format))
  {
    df[,datetime_cols]=lapply(as.df.frame(df[,datetime_cols]),function(x) as.Date(as.character(x),format=datetime_format))
  }
  if(!is.null(datetime_format))
  {
    
    check_date_col=lapply(df,function(x) try(as.Date(x[!is.na(x)],format=datetime_format),silent = T))
    date_cols=names(unlist(lapply(check_date_col,function(x) if(class(x)!='try-error'){if(sum(!is.na(x))>0) {1}})))
    date_cols=date_cols[!date_cols %in% datetime_cols]
    df[,date_cols]=lapply(as.data.frame(df[,date_cols]),function(x) as.Date(as.character(x),format=datetime_format))
  } 
  
  #### Scatter plots 
  df[,exclude_cols]=NULL
  length_unique=function(x) length(unique(x[!is.na(x)]))
  num_cols=names(df)[which(unlist(lapply(df,class)) %in% c('numeric','integer'))]
  cat_cols=names(df)[which(unlist(lapply(df,class)) %in% c('factor','character'))]
  date_cols=names(df)[which(unlist(lapply(df,class))=='Date')]
  
  ### univariate - numeric 
  dir.create(paste0(directory,'/Plots'))
  dir.create(paste0(directory,'/Plots/Univariate'))
  dir.create(paste0(directory,'/Plots/Bivariate'))
  
  if(length(num_cols)>0)
  { 
    dir.create(paste0(directory,'/Plots/Univariate/Histograms'))
    dir.create(paste0(directory,'/Plots/Univariate/Density'))
    dir.create(paste0(directory,'/Plots/Univariate/Boxplots'))
    dir.create(paste0(directory,'/Plots/Bivariate/Continuous-Continuous'))
    dir.create(paste0(directory,'/Plots/Bivariate/Continuous-Continuous/Scatter'))
    
    
    ### Correlation matrix
    if(corr){
      dir.create(paste0(directory,'/Plots/Bivariate/Continuous-Continuous/Correlation'))
      correlation_heatmap(data=df,numerical_columns=num_cols,dir=directory)
    }
    
    for(i in 1:length(num_cols))
    {
      univariate_histogram(data=df,x=num_cols[i],dir=directory)
      univariate_boxplot(data=df,x=num_cols[i],dir=directory)
      univariate_density(data=df,x=num_cols[i],dir=directory)
      other_num_cols=num_cols[num_cols!=num_cols[i]]
      if(length(other_num_cols)>0)
      { 
        dir.create(paste0(directory,'/Plots/Bivariate/Continuous-Continuous/Scatter/',num_cols[i]))
        for(j in 1:length(other_num_cols))
        { 
          bivariate_scatter(data=df,x=num_cols[i],y=other_num_cols[j],dir=directory)
        }
      }
    }
  }
  
  if(length(cat_cols)>0)
  { 
    dir.create(paste0(directory,'/Plots/Univariate/Barplots'))
    dir.create(paste0(directory,'/Plots/Bivariate/Discrete-Discrete'))
    dir.create(paste0(directory,'/Plots/Bivariate/Discrete-Continuous'))
    dir.create(paste0(directory,'/Plots/Bivariate/Discrete-Continuous/Boxplot'))
    dir.create(paste0(directory,'/Plots/Bivariate/Discrete-Continuous/Violin_Plot'))
    df[,cat_cols]=lapply(df[,cat_cols],as.character)
    for(i in 1:length(cat_cols))
    { 
      max_cat=min(max.cat,length_unique(df[,cat_cols[i]]))
      if(length_unique(df[,cat_cols[i]])>=max.cat)
      {
        freq_dist=as.data.frame(table(df[,cat_cols[i]]))
        freq_dist = freq_dist[order(-freq_dist$Freq),]
        top_n_cat=as.character(freq_dist$Var1[1:(max_cat-1)])
        df[,cat_cols[i]]=ifelse(df[,cat_cols[i]] %in% top_n_cat,df[,cat_cols[i]],'Others')
      }
    }  
    for(i in 1:length(cat_cols))  
    {
      df[,cat_cols[i]]=as.factor(df[,cat_cols[i]])
      univariate_barplot(data=df,x=cat_cols[i],dir=directory)
      other_cat_cols=cat_cols[cat_cols!=cat_cols[i]]
      if(length(other_cat_cols)>0)
      { 
        dir.create(paste0(directory,'/Plots/Bivariate/Discrete-Discrete/',cat_cols[i]))
        for(j in 1:length(other_cat_cols))
        { 
          bivariate_jitter(data=df,x=cat_cols[i],y=other_cat_cols[j],dir=directory)
        }
      }
      if(length(num_cols)>0)
      {
        dir.create(paste0(directory,'/Plots/Bivariate/Discrete-Continuous/Boxplot/',cat_cols[i]))
        dir.create(paste0(directory,'/Plots/Bivariate/Discrete-Continuous/Violin_Plot/',cat_cols[i]))
        for(k in 1:length(num_cols))
        {
          bivariate_boxplot(data=df,x=cat_cols[i],y=num_cols[k],dir=directory)
          bivariate_violin(data=df,x=cat_cols[i],y=num_cols[k],dir=directory)
        }
      }
    } 
  }
  
  if(length(date_cols)>0)
  {
    dir.create(paste0(directory,'/Plots/Bivariate/Time Series'))
    dir.create(paste0(directory,'/Plots/Bivariate/Time Series/Scatter'))
    dir.create(paste0(directory,'/Plots/Bivariate/Time Series/Line'))
    for(i in 1:length(date_cols))
    {
      dir.create(paste0(directory,'/Plots/Bivariate/Time Series/Scatter/',date_cols[i]))
      dir.create(paste0(directory,'/Plots/Bivariate/Time Series/Line/',date_cols[i]))
      range_date=difftime(max(df[,date_cols[i]],na.rm = T),min(df[,date_cols[i]],na.rm=T),units='days')
      range_date_1=as.character(floor(range_date/5))
      for(j in 1:length(num_cols))
      {
        timeseries_line(data=df,x=date_cols[i],y=num_cols[j],date_range=range_date_1,dir=directory)
        timeseries_scatter(data=df,x=date_cols[i],y=num_cols[j],date_range=range_date_1,dir=directory)
      }
    }
  }
}



