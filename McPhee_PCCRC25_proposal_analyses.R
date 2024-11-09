#Analyses for PCCRC 2025 proposal
#Author: Megan McPhee
#Date: 5 Nov 2024


#Data sources:
#  I retrieved NPGO from NPGO index downloaded from https://o3d.org/npgo/data/NPGO.txt on 30 October 2024.

# Pink salmon and steelhead data were kindly provided by the Vosbigian et al. (2024) via their manuscript;
# https://figshare.com/articles/dataset/Data_and_scripts_for_Cycles_in_adult_steelhead_length_suggest_interspecific_competition_in_the_North_Pacific_Ocean_/23233604)

library (tidyverse)
npgo <- read.csv ("NPGO.csv")
pink <- read.csv ("data/PinkAbundReturn_all.csv")
sth <- read.csv ("data/adultsteelheadlowergranite_2008_2019.csv")


#What NPGO index to use?
# Debertin et al. 2017 used November-March average, as this best describes the winter conditions that set up the food web in spring and summer:
# Debertin, A., Irvine, J.R., Holt, C.A., Oka, G., Trudel, M., 2017. Marine growth patterns of southern British Columbia chum salmon 
# explained by interactions between density-dependent competition and changing climate. 
# Canadian Journal of Fish and Aquatic Sciences 74, 1077–1087.

npgo.index <- npgo %>% filter (MONTH > 10 | MONTH < 4) %>% #select Nov-Mar
  mutate (index.year = case_when(
    MONTH > 10 ~ YEAR+1,
    MONTH < 4 ~ YEAR
  )) %>% #index year to January
  group_by (index.year) %>% summarize (npgoNM = mean (NPGO)) %>% #create mean of monthly values
  filter (index.year > 1950 & index.year < 2024) #trim the ends

#Limit this example analysis to a single genetic stock (Grande Ronde) and fish returning after 1 full year at sea

steelhead1<- sth %>% filter (SaltwaterAge == 1 & Genstock == "GRROND") %>% select (-c(X, CollectionDate))
pink.1 <- pink %>% select (c(Year, EKam, NorthAmerica))

# With the NPGO data create 'lag1' and 'lag2' variables that align the npgo index to the relevent steelhead return years.
# E.g., an ocean-age 1 steelhead returning in 2010 - our hypothesized common cause is npgo in 2009 (t-1) and in 2008 (t-2). 
# so index year would be npgo year + 1 (for lag1) and npgo year + 2 (for lag2), where index year corresponds to the year when 
# the adult steelhead returned.

npgo.lag1 <- npgo.index %>% mutate (lag1 = index.year + 1)
npgo.lag2 <- npgo.index %>% mutate (lag2 = index.year + 2)


# Merge steelhead, pink salmon, and NPGO data into a single dataframe:

temp.join <- left_join (steelhead1, pink.1, by = join_by (LastOceanYear == Year))
temp.join2 <- left_join (temp.join, npgo.lag1, by = join_by (LastOceanYear == lag1)) %>% select (-index.year)
pccrc.df <- left_join (temp.join2, npgo.lag2, by = join_by (LastOceanYear == lag2)) %>% select (-index.year)
names(pccrc.df)[8] <- "npgoL1"
names(pccrc.df)[9] <- "npgoL2"

rm (temp.join); rm (temp.join2)

# Verify: for LastOceanYear = 2010, npgoL1 should = 0.764 and npgoL2 should = 0.922
pccrc.df %>% filter (LastOceanYear == 2010)


# Linear regressions:
# m1: Naive model: PANA --> SS1
m1 <- lm (ForkLengthmm ~ NorthAmerica, data = pccrc.df)
summary (m1)


# m2: condition on NPGO lag1 and lag2:
m2 <- lm (ForkLengthmm ~ NorthAmerica + npgoL1 + npgoL2, data = pccrc.df)
summary (m2)

# "Counterfactual" - there should be no correlation between steelhead size after 1 year at sea and abundance of pink salmon from E Kamchatka
# m3 - no NPGO
# m4 - condition on NPGO
m3 <- lm (ForkLengthmm ~ EKam, data = pccrc.df)
summary (m3)

m4 <- lm (ForkLengthmm ~ EKam + npgoL1 + npgoL2, data = pccrc.df)
summary (m4)
















