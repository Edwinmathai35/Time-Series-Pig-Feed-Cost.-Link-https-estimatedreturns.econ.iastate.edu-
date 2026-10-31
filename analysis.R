# Explore monthly pig-feed cost trends, components, seasonality and forecasts.
# Run from this folder: Rscript analysis.R
# Required packages: readr, dplyr, tidyr, ggplot2, forecast.
# 1. IMPORT: read_csv reads the supplied audited university benchmark.
library(readr); library(dplyr); library(tidyr); library(ggplot2); library(forecast)
d <- read_csv('data.csv', show_col_types=FALSE) |> mutate(sale_month=as.Date(sale_month)) |> arrange(sale_month)
# 2. VALIDATE: fail on missing observations, duplicates, or broken monthly spacing.
stopifnot(nrow(d)==153, !anyNA(d), !anyDuplicated(d$sale_month))
stopifnot(identical(d$sale_month, seq(min(d$sale_month), max(d$sale_month), by='month')))
components <- c('corn_usd_head','soybean_meal_usd_head','ddgs_usd_head','complete_feed_other_usd_head','feed_processing_usd_head')
stopifnot(max(abs(rowSums(d[components])-d$total_feed_usd_head))<1e-8)
# 3. RESHAPE: create a monthly series and a long component table.
y <- ts(d$total_feed_usd_head, start=c(2014,1), frequency=12)
long <- pivot_longer(d, all_of(components), names_to='component', values_to='cost')
# 4. DESCRIBE AND PLOT: nominal costs; no inflation adjustment or causal claim.
ggsave('R_01_feed_cost.png',ggplot(d,aes(sale_month,total_feed_usd_head))+geom_line()+labs(x=NULL,y='Nominal USD per head',title='Estimated pig-feed cost | Iowa')+theme_minimal(),width=10,height=5)
ggsave('R_02_components.png',ggplot(long,aes(sale_month,cost,fill=component))+geom_area()+labs(x=NULL,y='Nominal USD per head')+theme_minimal(),width=10,height=5)
# R STL uses its own implementation; decomposition need not be numerically
# identical to Python statsmodels STL. Both use a 12-month seasonal cycle.
dec <- stl(y,s.window=13, robust=TRUE)
png('R_03_decomposition.png',width=1200,height=900);plot(dec);dev.off()
# 5. BACKTEST: expanding training window; final 24 months; one-step forecasts.
# R ETS(AAA) is the corresponding additive model class. Its likelihood and
# initialization differ from Python Holt-Winters. Compare outputs independently;
# numerical equivalence has NOT been established.
pred <- list(); k <- 1
for(i in (length(y)-23):length(y)) {
 train <- ts(as.numeric(y[1:(i-1)]),start=c(2014,1),frequency=12)
 values <- c('Seasonal naive'=as.numeric(y[i-12]),ETS=as.numeric(forecast(ets(train,model='AAA',damped=FALSE),h=1)$mean))
 for(m in names(values)) {pred[[k]]<-data.frame(month=d$sale_month[i],model=m,actual=as.numeric(y[i]),prediction=values[[m]]);k<-k+1}
}
a <- bind_rows(pred)
metrics <- a |> group_by(model) |> summarise(MAE_USD_head=mean(abs(prediction-actual)),RMSE_USD_head=sqrt(mean((prediction-actual)^2))) |> arrange(MAE_USD_head)
write_csv(a,'R_backtest_predictions.csv');write_csv(metrics,'R_forecast_accuracy.csv')
# 6. FORECAST AND EXPORT: select by held-out MAE; refit using all observed months.
# Selection on these 24 months means accuracy is a model-comparison result,
# not an independent post-selection estimate of future performance.
best <- metrics$model[1]
f <- if(best=='ETS') forecast(ets(y,model='AAA',damped=FALSE),h=3) else snaive(y,h=3)
write_csv(data.frame(month=seq(max(d$sale_month),by='month',length.out=4)[-1],forecast_USD_head=as.numeric(f$mean),model=best),'R_forecast.csv')
png('R_04_forecast.png',width=1200,height=650);plot(f,main='Pig-feed cost forecast',ylab='Nominal USD per head');dev.off()
print(metrics);print(f)
