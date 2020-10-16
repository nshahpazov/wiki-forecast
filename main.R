library('ggplot2') # visualization
library('ggthemes') # visualization
library('scales') # visualization
library('grid') # visualisation
library('gridExtra') # visualisation
library('corrplot') # visualisation
library('ggrepel') # visualisation
library('RColorBrewer') # visualisation
library('data.table') # data manipulation
library('dplyr') # data manipulation
library('readr') # data input
library('tibble') # data wrangling
library('tidyr') # data wrangling
library('lazyeval') # data wrangling
library('broom') # data wrangling
library('stringr') # string manipulation
library('purrr') # string manipulation
library('forcats') # factor manipulation
library('lubridate') # date and time
library('forecast') # time series analysis
library('prophet') # time series analysis
library('scater') # time series analysis

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  require(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}


train = read.csv("./train_1.csv")
c(ncol(train), nrow(train))
rev(dim(train))
train %>% colnames() %>% head(5) # return the first 5 colnames of the train dataset

# select first 5 rows of the Page column
train %>% select(Page) %>% head(5)

# check for missing data
sum(is.na(train))/(prod(dim(train))) # about 8% of the data is missing - na
# There are about 8% of missing values in this data set, which is not trivial. 
# We will neeed to take them into account in our analysis.

# To make the training data easier to handle we split it into two part: the article information (from the Page column) and the time series data (tdates) from the date columns. We briefly separate the article information into data from wikipedia, wikimedia, and mediawiki due to the different formatting of the Page names. After that, we rejoin all article information into a common data set (tpages).
tdates <- train %>% select(-Page)
foo <- train %>% select(Page) %>% rownames_to_column()
# row_names_to_column will create a separate column instead of having rownames which
# are sometimes (they say) hard to manipulate with

# filter those where in the page name is the word mediawiki
mediawiki <- foo %>% filter(str_detect(Page, "mediawiki"))
wikimedia <- foo %>% filter(str_detect(Page, "wikimedia"))
wikipedia <- foo %>% filter(str_detect(Page, "wikipedia"))  %>%
                     filter(!str_detect(Page, "wikimedia")) %>%
                     filter(!str_detect(Page, "mediawiki"))

# separate wikipedia in different columns
wikipedia <-  wikipedia %>% 
  separate(Page, into=c("foo", "bar"), sep=".wikipedia.org_") %>%
  separate(foo, into=c("article", "locale"), sep=-3) %>%
  separate(bar, into=c("access", "agent"), sep="_") %>% 
  mutate(locale=str_sub(locale, 2,3)) # remove the underscore
  
wikimedia <- wikimedia %>%
  separate(Page, into=c("article", "bar"), sep="_commons.wikimedia.org_") %>%
  separate(bar, into=c("access", "agent"), sep="_") %>%
  add_column(locale="wikimed")

mediawiki <- mediawiki %>%
  separate(Page, into=c("article", "bar"), sep="_www.mediawiki.org_") %>%
  separate(bar, into=c("access", "agent"), sep="_") %>%
  add_column(locale="mediawik")


# join back all to tpages
JOIN_COLUMNS = c("rowname", "article", "locale", "access", "agent")
tpages <- wikipedia %>%
  full_join(wikimedia, by=JOIN_COLUMNS) %>%
  full_join(mediawiki, by=JOIN_COLUMNS)

# its all in here
sample_n(tpages, size=5)  

tpages %>% filter(str_detect(article, "The_Beatles")) %>%
  filter(access == "all-access") %>%
  filter(agent == "all-agents")

extract_ts <- function (rownr) {
  tdates %>% 
    rownames_to_column() %>%
    filter(rowname == as.character(rownr)) %>%
    gather(dates, value, -rowname) %>% # moves column to rows without rowname, duplicating values
    spread(rowname, value) %>% # this will give columns for different rows 
    mutate(dates=ymd(str_sub(dates, 2, -1))) %>%
    rename(views = as.character(rownr))
}

# a lot of duplication of code 
extract_ts_nrm <- function (rownr) {
  extract_ts(rownr = rownr) %>% mutate(views=views/mean(views))
}
extract_ts_nrm("1")

plot_rownr <- function (rownr) {
  art <- tpages %>% filter(rowname == rownr) %>% .$article
  loc <- tpages %>% filter(rowname == rownr) %>% .$locale
  acc <- tpages %>% filter(rowname == rownr) %>% .$access
  extract_ts(rownr) %>%
    ggplot(aes(dates, views)) + # create the plot on the data
    geom_line() + # plot it in lines
    geom_smooth(method="loess", color="blue", span=1/5) + # local regression smooting
    labs(title=str_c(art, " - ", loc, " - ", acc)) # title on top
}

plot_rownr_log <- function (rownr) {
  plot_rownr(rownr) + scale_y_log10() + labs(y="log views")
}

plot_rownr_zoom <- function(rownr, start, end){
  art <- tpages %>% filter(rowname == rownr) %>% .$article
  loc <- tpages %>% filter(rowname == rownr) %>% .$locale
  acc <- tpages %>% filter(rowname == rownr) %>% .$access
  extract_ts(rownr) %>%
    filter(dates > ymd(start) & dates <= ymd(end)) %>%
    ggplot(aes(dates, views)) +
    geom_line() +
    # geom_smooth(method = "loess", color = "blue", span = 1/5) +
    # coord_cartesian(xlim = ymd(c(start,end))) +  
    labs(title = str_c(art, " - ", loc, " - ", acc))
}

plot_rownr(11214)

# In addition, with the help of the extractor tool we define a function that re-connects the Page information to the corresponding time series and plots this curve according to our specification on article name, access type, and agent for all the available languages:
plot_names <- function(article_name, access_type, agent_type){

  pick <- tpages %>% filter(str_detect(article, article_name)) %>%
    filter(access == access_type) %>%
    filter(agent == agent_type)

  pick_rowname = pick %>% .$rowname
  pick_locale = pick %>% .$locale
  
  tdat <- extract_ts(pick_rowname[1]) %>% # they have the same name so itdoesnt namtter
    mutate(loc = pick_locale[1])
  
  for (i in seq(2, length(pick))) {
    foo <- extract_ts(pick_rowname[i]) %>%
      mutate(loc=pick_locale[i]) # not sure what this does
    tdat <- bind_rows(tdat, foo)
  }
  
  plt <- tdat %>%
    ggplot(aes(dates, views, color=loc)) + 
    geom_line() + 
    labs(title=str_c(article_name, " - ", access_type, " - ", agent_type))
  
  print(plt)
}

plot_names_nrm <- function(article_name, access_type, agent_type){
  
  pick <- tpages %>% filter(str_detect(article, article_name)) %>%
    filter(access == access_type) %>%
    filter(agent == agent_type)

  pick_rowname = pick %>% .$rowname
  pick_locale = pick %>% .$locale

  # use the extract by rowname ts function
  tdat <- extract_ts_nrm(pick_rowname[1]) %>% # they have the same name so itdoesnt namtter
    mutate(loc = pick_locale[1])

  # use the same function for each found pick and attach locale as a col
  for (i in seq(2, length(pick))) {
    foo <- extract_ts_nrm(pick_rowname[i]) %>%
      mutate(loc=pick_locale[i])
    tdat <- bind_rows(tdat, foo)
  }

  plt <- tdat %>%
    ggplot(aes(dates, views, color=loc)) + 
    geom_line() + 
    labs(title=str_c(article_name, " - ", access_type, " - ", agent_type)) +
    scale_y_log10() + labs(y="log views")
  
  print(plt)
}

# args(gather)

plot_names("Beatles", "all-access", "all-agents")

# 3. Summary Parameter Extraction
# Before diving into the time series data let’s have a look how the different meta-parameters are distributed:


p1 <- tpages %>%
  ggplot(aes(agent)) + geom_bar(fill="magenta")

p2 <- tpages %>%
  ggplot(aes(access)) + geom_bar(fill="magenta")

p3 <- tpages %>%
  ggplot(aes(locale, fill=locale)) + geom_bar() + theme(legend.position = "none")

layout <- matrix(c(1, 2, 3, 3), 2, 2, byrow = TRUE)
multiplot(p1, p2, p3, layout=layout)



# 3.2. Basic time series parameters
params_ts1 <- function (rownr) {
  foo <- tdates %>%
    filter_((interp(~x == row_number(), .values=list(x=rownr)))) %>% # todo: understand that
    rownames_to_column() %>%
    gather(dates, value, -rowname) %>%
    spread(rowname, value) %>%
    mutate(dates=ymd(str_sub(dates, 2, -1)),
           views=as.integer(`1`))
  
  slope <- ifelse(is.na(mean(foo$views)), 0, summary(lm(views ~ dates, data=foo))$coef[2])
  slope_err <- ifelse(is.na(mean(foo$views)), 0, summary(lm(views ~ dates, data=foo))$coef[4])
  
  bar <- tibble(
    rowname = rownr,
    min_view  = min(foo$views),
    max_view = max(foo$views),
    mean_view = mean(foo$views),
    med_view = median(foo$views),
    sd_view = sd(foo$views),
    slope = slope / slope_err
  )
  return(bar)
}

set.seed(4321)
foo <- sample_n(tpages, 5500) # sample the pages
rows <- foo$rowname # get first

pcols <- c("rowname", "min_view", "max_view", "mean_view", "med_view", "sd_view", "slope")

params <- params_ts1(rows[1])

for (i in seq(2, nrow(foo))) {
  params <- full_join(params, params_ts1(rows[i]), by = pcols)
}

# this is error prone
params <- params %>%
  filter(!is.na(mean_view)) %>%
  mutate(rowname = as.character(rowname))


# 3.3 Overview visualizations
p1 <- params %>%
  ggplot(aes(mean_view)) + geom_histogram(fill = "magenta", bins=50) + scale_x_log10()
p2 <- params %>%
  ggplot(aes(max_view)) + geom_histogram(fill = "magenta", bins=50) + scale_x_log10()
p3 <- params %>%
  ggplot(aes(sd_view/mean_view)) + geom_histogram(fill = "magenta", bins=50) + scale_x_log10()

p4 <- params %>%
  ggplot(aes(slope)) + geom_histogram(fill = "magenta", bins=30) + scale_x_continuous()


layout = matrix(c(1, 2, 3, 4), 2, 2, byrow=TRUE)
multiplot(p1, p2, p3, p4, layout=layout)

par_page <- left_join(params, tpages, by="rowname")

p1 <- par_page %>%
  ggplot(aes(mean_view, fill=locale)) + 
  geom_density(position = "stack") +
  scale_x_log10(limits=c(1,1e4)) +
  theme(legend.position = "none")

p2 <- par_page %>%
  ggplot(aes(max_view, fill=locale)) + 
  geom_density(position = "stack") +
  scale_x_log10(limits=c(1,1e6)) +
  theme(legend.position = "none")

p3 <- par_page %>%
  ggplot(aes(sd_view, fill=locale)) + 
  geom_density(position = "stack") +
  scale_x_log10(limits=c(1,1e5)) +
  theme(legend.position = "none")

p4 <- par_page %>%
  ggplot(aes(slope, fill=locale)) + 
  geom_density(position = "stack") +
  scale_x_continuous(limits=c(-10,10))


multiplot(p1,p2,p3,p4, layout=layout)

# Next we examine the 2d histogram
# todo: understand that plot
params %>%
  ggplot(aes(max_view - mean_view, mean_view)) + 
  geom_bin2d(bins = c(50, 50)) +
  scale_x_log10() +
  scale_y_log10() +
  labs(x="max views above mean", y="mean views")


limx <- c(max(params$max_view)/35, max(params$max_view))
limy <- c(max(params$mean_view)/35, max(params$mean_view))
par_page %>%
  ggplot(aes(max_view-mean_view, mean_view)) +
  geom_point(size = 2, color = "red") +
  scale_x_log10(limits = limx) +
  scale_y_log10(limits = limy) +
  labs(x = "maximum views above mean", y = "mean views") +
  geom_label_repel(aes(label = str_c(article, " (",rowname,")")), alpha = 0.5)


limx <- c(max(params$max_view)/35, max(params$max_view))
limy <- c(max(params$mean_view)/35, max(params$mean_view))
par_page %>%
  ggplot(aes(max_view-mean_view, mean_view)) +
  geom_point(size = 2, color = "red") +
  scale_x_log10(limits = limx) +
  scale_y_log10(limits = limy) +
  labs(x = "maximum views above mean", y = "mean views") +
  geom_label_repel(aes(label = str_c(article, " (",rowname,")")), alpha = 0.5)


# Individual observations with extreme params
# Based on the overview parameters we can focus our attention on those articles for which the time series parameters are at the extremes of the parameter space.

# 4.1 Large linear slope

# sort 
# the select  just reorders the columns
params %>% arrange(desc(slope)) %>% head(5) %>% select(rowname, slope, everything())


p1 <- plot_rownr(41275)
p2 <- plot_rownr(83456)
p3 <- plot_rownr(136949)
p4 <- plot_rownr(79180)

multiplot(p1,p2,p3,p4, layout = layout)

plot_names_nrm("Twenty_One_Pilots", "all-access", "all-agents")


params %>% arrange(slope) %>% head(5) %>% select(rowname, slope, everything())


# 5. short term variablility

p1 <- plot_rownr_zoom(10404, "2016-10-01", "2016-12-01")
p2 <- plot_rownr_zoom(9775, "2015-09-01", "2015-11-01")
p3 <- plot_rownr_zoom(139120, "2016-10-01", "2016-12-01")
p4 <- plot_rownr_zoom(110658, "2016-07-01", "2016-09-01")

layout <- matrix(c(1,2,3,4),2,2,byrow=TRUE)
multiplot(p1, p2, p3, p4, layout=layout)


rownr <- 10404
start <- "2016-10-01"
end <- "2016-12-01"
foo1 <- extract_ts(rownr) %>%
  filter(dates > ymd(start) & dates < ymd(end)) %>%
  mutate(dates = wday(dates, label = TRUE)) %>%
  group_by(dates) %>%
  summarise(wday_views = mean(views)) %>%
  mutate(wday_views = wday_views/mean(wday_views)) %>%
  mutate(id = factor(rownr))

rownr <- 9775
start <- "2015-09-01"
end <- "2015-11-01"
foo2 <- extract_ts(rownr) %>%
  filter(dates > ymd(start) & dates < ymd(end)) %>%
  mutate(dates = wday(dates, label = TRUE)) %>%
  group_by(dates) %>%
  summarise(wday_views = mean(views)) %>%
  mutate(wday_views = wday_views/mean(wday_views)) %>%
  mutate(id = factor(rownr))

rownr <- 139120
start <- "2016-10-01"
end <- "2016-12-01"
foo3 <- extract_ts(rownr) %>%
  filter(dates > ymd(start) & dates < ymd(end)) %>%
  mutate(dates = wday(dates, label = TRUE)) %>%
  group_by(dates) %>%
  summarise(wday_views = mean(views)) %>%
  mutate(wday_views = wday_views/mean(wday_views)) %>%
  mutate(id = factor(rownr))

rownr <- 110658
extract_ts(110658)
start <- "2016-07-01"
end <- "2016-09-01"
foo4 <- extract_ts(rownr) %>%
  filter(dates > ymd(start) & dates < ymd(end)) %>%
  mutate(dates = wday(dates, label = TRUE)) %>%
  group_by(dates) %>%
  summarise(wday_views = mean(views)) %>%
  mutate(wday_views = wday_views/mean(wday_views)) %>%
  mutate(id = factor(rownr))

foo <- bind_rows(foo1,foo2,foo3,foo4)

foo %>%
  ggplot(aes(dates, wday_views, color = id)) +
  geom_jitter(size = 4, width = 0.1) +
  labs(x = "Day of the week", y = "Relative average views")

# 6. Forecasting (finally)
plot_auto_arima_rownr <- function (rownr) {
  
  pageviews <- extract_ts(rownr) %>%
    rownames_to_column() %>%
    mutate(rowname = as.integer(rowname))
  
  # testing length
  pred_length = 60
  # testing range
  pred_range = c(nrow(pageviews)-pred_length - + 1, nrow(pageviews))
  
  # training data set head first before pred length
  pre_views = pageviews %>% head(nrow(pageviews) - pred_length)
  post_views = pageviews %>% tail(pred_length)
  
  arima.fit <- auto.arima(tsclean(ts(pre_views$views, frequency = 7)),
                          
                          d = 1, D = 1, stepwise = FALSE, approximation = FALSE)
  
  fc_views <- arima.fit %>% forecast(h = pred_length, level = c(50,95))
  
  autoplot(fc_views) + geom_line(aes(rowname/7, views), data=post_views, color="grey40") +
    labs(x = "Time [weeks]", y = "views vs auto.arima predictions")
}


p1 <- plot_auto_arima_rownr(70772)

p2 <- plot_auto_arima_rownr(108341)

p3 <- plot_auto_arima_rownr(95856)

p4 <- plot_auto_arima_rownr(139120)



layout <- matrix(c(1,2,3,4),2,2,byrow=TRUE)

multiplot(p1, p2, p3, p4, layout=layout)


# using prophet
rownr <- 139120

pageviews <- extract_ts(rownr) %>%
  rename(y = views,
         ds = dates)

pred_len <- 60
pred_range <- c(nrow(pageviews)-pred_len+1, nrow(pageviews))
pre_views <- pageviews %>% head(nrow(pageviews)-pred_len)
post_views <- pageviews %>% tail(pred_len)

proph <- prophet(pre_views, changepoint.prior.scale=0.5, yearly.seasonality=TRUE, daily.seasonality = FALSE)
future <- make_future_dataframe(proph, periods = pred_len)
fcast_future <- predict(proph, future)

plot(proph, fcast_future)
prophet_plot_components(proph, fcast)
