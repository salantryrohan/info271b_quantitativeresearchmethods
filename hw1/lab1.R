
# PART 1 - variable manipulation 

# load the Countries dataframe from the saved file 
load("/Users/rohansalantry/Downloads/Countries.Rdata")
class(Countries)

# a) create perc incr variable for capturing increase in internet users and calculate summary statistics
# calculate percentage increase from 2010 to 2011
Countries$percincr = ((Countries$internet_users_2011 - Countries$internet_users_2010)/(Countries$internet_users_2010))*100 
View(Countries)

# mean percentage increase
mean_perc_incr = mean(Countries$percincr[!is.na(Countries$percincr)])
mean_perc_incr

# median percentage increase
median_perc_incr = median(Countries$percincr[!is.na(Countries$percincr)])
median_perc_incr

# b) find the country with the highest percentage of increase in internet users

# find max perc increase 
maxvar = max(Countries$percincr[!is.na(Countries$percincr)])
maxvar 
# find index of row with the max value
maxvarindex = which(Countries$percincr == maxvar) 
# find country using the index we calculated
country_with_max_perc = Countries$Country[maxvarindex] 
country_with_max_perc


#c ) calculate mean gdp and find number of countries below and above mean gdp
# calculate mean gdp
meangdp = mean(Countries$gdp2012[!is.na(Countries$gdp2012)])
meangdp

#calculate mediangdp
mediangdp = median(Countries$gdp2012[!is.na(Countries$gdp2012)])
mediangdp

# create a boolean column with TRUE for countries with gdp above mean gdp , FALSE otherwise
Countries$high_gdp = Countries$gdp2012 > meangdp

# calculate number of countries below mean gdp
num_gdp_below_mean = length(which(Countries$high_gdp == FALSE))
num_gdp_below_mean

# calculate number of countries above mean gdp
num_gdp_above_mean = length(which(Countries$high_gdp == TRUE))
num_gdp_above_mean

# draw histogram for country gdp data, bins forced to a higher value to observe data to a higher precision
hist(Countries$gdp2012, nclass = 500)

#d ) mean gdp of countires below and above the mean gdp 

# logical vector to find countries higher than mean gdp
log_vec1 = c((Countries$gdp2012 > meangdp) & !is.na(Countries$gdp2012))

#separate out dataset with higher than mean gdp
mean_intrnet_gdp1 = Countries$percincr[log_vec1]

length(mean_intrnet_gdp1)
# mean percentage internet users increase for countries above mean gdp
mean_percincr_higher = mean(mean_intrnet_gdp1[!is.na(mean_intrnet_gdp1)])
mean_percincr_higher

# logical vector to find countries lower than mean gdp
log_vec2 = c((Countries$gdp2012 < meangdp) & !is.na(Countries$gdp2012))

#separate out dataset with lower than mean gdp
mean_intrnet_gdp2 = Countries$percincr[log_vec2]
length(mean_intrnet_gdp2)

# mean percentage internet users increase for countries below mean gdp
mean_percincr_lower = mean(mean_intrnet_gdp2[!is.na(mean_intrnet_gdp2)])
mean_percincr_lower

#e) recode regions into factors

Countries$region_recoded = factor(Countries$region,labels=c("AF","AM","AS","EU","OC"))
View(Countries)


# PART 2 - Importing external data
# Source - http://data.worldbank.org/indicator/SP.RUR.TOTL.ZS
# this contains percentage rural population country wise recorded from 1953 to 2011. I chose to take 2011 data and merge it with our existing data set. 
# it would be interesting to see the relationship of percentage of rural population for the country to gdp and internet usage. 

# after downloading the file, unzip the folder and rename the data file to population.csv
#  remove first two lines , obstruct reading of csv since csv has first line as column header. first two lines in the files given by the world bank 
# comments about the dataset. we found these errors after trying to load the file in R and getting exceptions


worldbankdf = read.csv('/Users/rohansalantry/Documents/fall13/271b/ruralpopulation.csv')
View(worldbankdf)
worldbankdf_mod = data.frame(Country = worldbankdf$Country.Name, percrural2012 = worldbankdf$X2012)
# inner join on Country (both column names intentionally kept the same)
Countries = merge(Countries,worldbankdf_mod, by="Country")
View(Countries)

# Part 3 - Graphs
# a - plot histogram of percentage rural population dataset 

# plot histogram , forced classes = 100 for better precision in observation
hist(Countries$percrural2012,nclass=100)
Countries$logpercrural = log(Countries$percrural2012)
# histogram with log data
hist(Countries$logpercrural,nclass=100)

# calculate summary statistics 
summary(Countries$percrural2012)
sd(Countries$percrural2012[!is.na(Countries$percrural2012)])




# b scatter plot between percent internet users and fertility rate

# scatter without transformation function 
plot(Countries$internet_users_2011,Countries$fertility_rate, ylab="fertility rate", xlab="internet user percentages")


Countries$log_fertility = log(Countries$fertility_rate)

# scatter with transformation function applied (log function) 
plot(Countries$internet_users_2011,Countries$log_fertility, ylab="fertility rate in log", xlab="internet user percentages")

# find correlation to find the degree of linear relationship with and without the transformation function applied
corr.test(Countries$internet_users_2011,Countries$fertility_rate)
corr.test(Countries$internet_users_2011,Countries$log_fertility)

# LOG does not make relationship better. it scatters the scatter for values which are lower but improves clustering for values that are higher.



# c
# scatter of perc rural population and infant mortality 

plot(Countries$percrural2012,Countries$infant.mortality, ylab="infant mortality", xlab="perc rural population")
corr.test(Countries$percrural2012,Countries$infant.mortality)

# summary statistics 
dfruralmortality = data.frame(percrural = Countries$percrural2012, mortality = Countries$infant.mortality)
View(dfruralmortality)
summary(dfruralmortality)
