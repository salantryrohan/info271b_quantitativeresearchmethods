load("/Users/rohansalantry/Downloads/Countries.Rdata")
class(Countries)




Countries$percincr = ((Countries$internet_users_2011 - Countries$internet_users_2010)/(Countries$internet_users_2010))*100 
View(Countries)

mean_perc_incr = mean(Countries$percincr[!is.na(Countries$percincr)])
mean_perc_incr

median_perc_incr = median(Countries$percincr[!is.na(Countries$percincr)])
median_perc_incr

sd_perc_incr = sd(Countries$percincr[!is.na(Countries$percincr)])
sd_perc_incr

maxvar = max(Countries$percincr[!is.na(Countries$percincr)])
maxvar

maxvarindex = which(Countries$percincr == maxvar)
country_with_max_perc = Countries$Country[maxvarindex]
country_with_max_perc
# myanmar

meangdp = mean(Countries$gdp2012[!is.na(Countries$gdp2012)])
meangdp

Countries$high_gdp = Countries$gdp2012 > meangdp

num_gdp_below_mean = length(which(Countries$high_gdp == FALSE))

num_gdp_above_mean = length(which(Countries$high_gdp == TRUE))


# below mean = 149, above mean = 25. Most data is less than the mean. this implies the distribution is positively skewed 
# (has a long right tail which drags the mean to the right)
Countries$region_recoded = factor(Countries$region,labels=c("AF","AM","AS","EU","OC"))
View(dummies)



# Part 2
# Source - http://data.worldbank.org/indicator/SP.RUR.TOTL.ZS
# this contains percentage rural population country wise recorded from 1953 to 2011. I chose to take 2011 data and merge it with our existing data set. 
# it would be interesting to see the relationship of percentage of rural population for the country to gdp and internet usage. 

# after downloading the file, unzip the folder and rename the data file to population.csv
#  remove first two lines , obstruct reading of csv since csv has first line as column header. first two lines in the files given by the world bank 
# comments about the dataset. we found these errors after trying to load the file in R and getting exceptions

worldbankdf = read.csv('/Users/rohansalantry/Documents/ruralpopulation.csv')
View(worldbankdf)
worldbankdf_mod = data.frame(Country = worldbankdf$Country.Name, percrural2012 = worldbankdf$X2012)
# inner join on Country (both column names intentionally kept the same)
Countries = merge(Countries,worldbankdf_mod, by="Country")





# part 3

# a

hist(Countries$percrural2012)
# variable is normally distruted (approximately). 

summary(Countries$percrural2012)

# the summary shows mean, median, first and third quartile. 

# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 0.00   24.27   43.41   42.88   63.03   88.79       4 

# we also calculated std deviation = 24.13. Mean ~ Median. 3 rd quartile value is less than one sd from mean. 


sd(Countries$percrural2012[!is.na(Countries$percrural2012)])


# b 

plot(Countries$internet_users_2011,Countries$fertility_rate, ylab="fertility rate", xlab="internet user percentages")



Countries$log_fertility = log(Countries$fertility_rate)

plot(Countries$internet_users_2011,Countries$log_fertility, ylab="fertility rate in log", xlab="internet user percentages")

# LOG does not make relationship better. it scatters the scatter for values which are lower but improves clustering for values that are higher.



# c

plot(Countries$internet_users_2011, Countries$percrural2012, ylabel = 'perc rural populatino', xlabel = 'internetuers')

# negative correlation between internet users and percentage population. As the percentage of internet users increase , the 
# percentage of rural population is decreasing. 












