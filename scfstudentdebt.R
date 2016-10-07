#### Set up the file ####
      
      #set working directory
      setwd( "I:/User/Williams/Publications/529Data/rstuff/" )

      #Load external packages
      library(mitools)	# allows analysis of multiply-imputed survey data
      library(survey)		# load survey package (analyzes complex design surveys)
      library(downloader)	# downloads and then runs the source() function on scripts from github
      library(foreign) 	# load foreign package (converts data files into R)
      library(Hmisc) 		# load Hmisc package (loads a simple wtd.quantile function)
      library(dplyr)    #always needed
      
      # turn off scientific notation in most output
      options( scipen = 20 )
      
      # load two svyttest functions (one to conduct a df-adjusted t-test and one to conduct a multiply-imputed t-test)
      source("scf.survey.R")
      
      # load the 201 survey of consumer finances into memory
      load( "scf2013.rda" )
      
      #create variable with list of implicates
      implicates <- c("imp1","imp2","imp3","imp4","imp5")
      
#### Delete unneeded variables, set up data set ####
      
      #list of variables to keep.
      vars.to.keep <- c( 'y1' , 
                         'yy1' , 
                         'wgt' , 
                         'one' , 
                         'networth', 
                         'income', 
                         'five',
                         "x7801", #Do you (and your family living here) owe any money or have any loans for educational expenses?
                         "x7802", #How Many Such loans?
                         "x6693", #Originally reported value of X7802 
                         "x7805", #Loan 1 Original Amount
                         "x7806", #Paying on Loan 1 Now?
                         "x7173", #Payments on Loan 1 deferred?
                         "x7815", #How much are the Payments on Loan 1
                         "x7816", #Loan 1 Payment Frequency Per ____
                         "x7817", #Loan 1 typical payment
                         "x7818", #loan 1 typical payment freqncy
                         "x7822", #loan 1 interest rate
                         "x7824", #loan 1 reemaining owed
                         "x7847", #loan 2 reemaining owed
                         "x7870", #loan 3 reemaining owed
                         "x7924", #loan 4 reemaining owed
                         "x7947", #loan 5 reemaining owed
                         "x7970", #loan 6 reemaining owed
                         "x7179", #How much in total is owed on all the remaining loans
                         "x8840", #Original value of remaining total for Rs who did not provide complete information
                         )
      
      #keep only the variables we care about
      imp1 <- imp1[ , vars.to.keep ]
      imp2 <- imp2[ , vars.to.keep ]
      imp3 <- imp3[ , vars.to.keep ]
      imp4 <- imp4[ , vars.to.keep ]
      imp5 <- imp5[ , vars.to.keep ]
      
      #rename numbered variables from SCF codebook
      for (implicate in implicates) {
            
            temp <- get(implicate)
            
            names(temp)[8:22] <- c(
                                    "haveaccts",
                                    "numb.accts",
                                    "original.numb.accts",
                                    "acct.1.balance",
                                    "acct.2.balance",
                                    "acct.3.balance",
                                    "acct.4.balance",
                                    "acct.5.balance",
                                    "acct.6.balance",
                                    "acct.1.type",
                                    "acct.2.type",
                                    "acct.3.type",
                                    "acct.4.type",
                                    "acct.5.type",
                                    "acct.6.type"
                                    
            )
            
            assign(implicate, temp)
            
            remove(temp)
      }
      
      
      # construct an imputed replicate-weighted survey design object
      # build a new replicate-weighted survey design object,
      # but unlike most replicate-weighted designs, this object includes the
      # five multiply-imputed data tables - imp1 through imp5
      scf.design <- 
            svrepdesign( 
                  
                  # use the main weight within each of the imp# objects
                  weights = ~wgt , 
                  
                  # use the 999 replicate weights stored in the separate replicate weights file
                  repweights = rw[ , -1 ] , 
                  
                  # read the data directly from the five implicates
                  data = imputationList( list( imp1 , imp2 , imp3 , imp4 , imp5 ) ) , 
                  
                  scale = 1 ,
                  
                  rscales = rep( 1 / 998 , 999 ) ,
                  
                  # use the mean of the replicate statistics as the center
                  # when calculating the variance, as opposed to the main weight's statistic
                  mse = TRUE ,
                  
                  type = "other" ,
                  
                  combined.weights = TRUE
            )
      
      # clear up RAM
      gc()
      
#### Group each record in to an income quantile ####   

      #calculate income quantiles
      for (quantile in c(.2,.4,.6,.8,.9)) {
            
            # calculate income from each quantile
            temp <- scf.MIcombine( with( scf.design , svyquantile( ~income, quantiles = quantile)))
            
            # tore the value of the quantile
            temp <- coef(temp)
            
            # create quantile table, add values
            if (!exists("quantile_table")) {
                  quantile_table <- data.frame(quantile = c(quantile), income = temp)
            } else {
                  quantile_table <- rbind(quantile_table, c(quantile, temp))
            }
            remove(temp)
      }
      
      # Categorize each record as belonging to an income group.
      for (implicate in implicates) {
            
            #grab implicate
            temp <- get(implicate)
            
            #assign incomes below .1 percentile to group 1
            temp[temp$income < quantile_table[1,2],"inc.group"] <- 1
            
            #assign incomes at each threshold to respective groups
            for (i in 2:5) {
                  temp[temp$income < quantile_table[i,2] & temp$income >= quantile_table[(i-1),2] ,"inc.group"] <- i
            }
            
            #assign incomes above .9 percentile to group 6
            temp[temp$income >= quantile_table[5,2],"inc.group"] <- 6
            
            #assign to data set as a whole
            assign(implicate,temp)
            
            #remove temporary data set.
            remove(temp)
      }
                  
 #### Calculate value of 529 and Coverdell Accounts ####
      
      #run the calculation
      for (implicate in implicates) {
            
            temp <- get(implicate)
            
            # sum the value of all education savings (account type 2) or 529 plans (account type 3)
            temp <- mutate(temp, acct.1.edu = ifelse(acct.1.type == 3 | acct.1.type == 2, acct.1.balance, 0)) %>%
                  mutate(acct.2.edu = ifelse(acct.2.type == 3 | acct.2.type == 2, acct.2.balance, 0)) %>%
                  mutate(acct.3.edu = ifelse(acct.3.type == 3 | acct.3.type == 2, acct.3.balance, 0)) %>%
                  mutate(acct.4.edu = ifelse(acct.4.type == 3 | acct.4.type == 2, acct.4.balance, 0)) %>%
                  mutate(acct.5.edu = ifelse(acct.5.type == 3 | acct.5.type == 2, acct.5.balance, 0)) %>%
                  mutate(acct.6.edu = ifelse(acct.6.type == 3 | acct.6.type == 2, acct.6.balance, 0)) %>%
                  mutate(edu_savings = acct.1.edu + acct.2.edu + acct.3.edu + acct.4.edu + acct.5.edu + acct.6.edu)
      
            #multply assets by 5 to get proper weighting given the multiple imputation structure
            temp$edu_savings_five <- temp$edu_savings*5
            
            assign(implicate, temp)
            
      }
      
      #create variable that is equal to the "five" column if individual has edu savings
      for (implicate in implicates) {
            temp <- get(implicate)
            temp <- mutate(temp, has.edu = ifelse(edu_savings>0, five, 0))
            assign(implicate,temp)
      }
      
      
      # recreate the scf design object with newly coded variables
      scf.design <- 
            svrepdesign( 
                  
                  # use the main weight within each of the imp# objects
                  weights = ~wgt , 
                  
                  # use the 999 replicate weights stored in the separate replicate weights file
                  repweights = rw[ , -1 ] , 
                  
                  # read the data directly from the five implicates
                  data = imputationList( list( imp1 , imp2 , imp3 , imp4 , imp5 ) ) , 
                  
                  scale = 1 ,
                  
                  rscales = rep( 1 / 998 , 999 ) ,
                  
                  # use the mean of the replicate statistics as the center
                  # when calculating the variance, as opposed to the main weight's statistic
                  mse = TRUE ,
                  
                  type = "other" ,
                  
                  combined.weights = TRUE
            ) 
      
##########################################
## Analysis is below; above is cleaning ##
##########################################            
      
#### Double check everything lines up with FRB data ####
      
      #generate mean income for whole sample (compare to FRB numbers--should equal 86.6k)
      meanincomes <- mean(
            c(
                  weighted.mean( imp1$income , imp1$wgt ) ,
                  weighted.mean( imp2$income , imp2$wgt ) ,
                  weighted.mean( imp3$income , imp3$wgt ) ,
                  weighted.mean( imp4$income , imp4$wgt ) ,
                  weighted.mean( imp5$income , imp5$wgt )
            )
      )
      meanincomes # <- should equal 86.6k
      
      #generate median income for whole sample (compare to FRB numbers--should equal 46.7k)
      medianincomes <- mean(
            c(
                  wtd.quantile( imp1$income , imp1$wgt , 0.5 ) ,
                  wtd.quantile( imp2$income , imp2$wgt , 0.5 ) ,
                  wtd.quantile( imp3$income , imp3$wgt , 0.5 ) ,
                  wtd.quantile( imp4$income , imp4$wgt , 0.5 ) ,
                  wtd.quantile( imp5$income , imp5$wgt , 0.5 )            
            )
      )
      
      medianincomes # <- should equal 46.7k
      
      #### Try running it using anthony damico's code #### 
      
      # Check mean income
      ( mean_income <- scf.MIcombine( with( scf.design , svymean( ~income ) ) ) )  # <- should equal 86.6k
      
      # Check median income 
      ( median_income <- scf.MIcombine( with( scf.design , svyquantile( ~income, quantiles = .5)))) # <- should equal 46.7k  
      
#### Get mean incomes for income group ####
            
      #create data frame for results
      group.income.stats <- data.frame(group = c(1:6))
      
      for (i in 1:6) {
            
            #get weighted median for income group in each implicate
            temp_median <- c(
                  wtd.quantile( filter(imp1,inc.group == i)$income, filter(imp1,inc.group == i)$wgt, .5),
                  wtd.quantile( filter(imp2,inc.group == i)$income, filter(imp2,inc.group == i)$wgt, .5),
                  wtd.quantile( filter(imp3,inc.group == i)$income, filter(imp3,inc.group == i)$wgt, .5),
                  wtd.quantile( filter(imp4,inc.group == i)$income, filter(imp4,inc.group == i)$wgt, .5),
                  wtd.quantile( filter(imp5,inc.group == i)$income, filter(imp5,inc.group == i)$wgt, .5)
            )
            
            #add median to table
            group.income.stats[group.income.stats$group==i,"median.income"] <- mean(temp_median)
            remove(temp_median)
            
            #get weighted mean for income group in each implicate
            temp_means <- c(
                        weighted.mean( filter(imp1,inc.group == i)$income, filter(imp1,inc.group == i)$wgt),
                        weighted.mean( filter(imp2,inc.group == i)$income, filter(imp2,inc.group == i)$wgt),
                        weighted.mean( filter(imp3,inc.group == i)$income, filter(imp3,inc.group == i)$wgt),
                        weighted.mean( filter(imp4,inc.group == i)$income, filter(imp4,inc.group == i)$wgt),
                        weighted.mean( filter(imp5,inc.group == i)$income, filter(imp5,inc.group == i)$wgt)
                  )
            
            #add mean to table.
            group.income.stats[group.income.stats$group==i,"mean.income"] <- mean(temp_means)
            remove(temp_means)
            
      }

##### calculate mean education savings by income group ####

      #create data frame to store results.
      mean_savings <- data.frame(income_group = c(1:6), 
                                 imp1 = NA,
                                 imp2 = NA,
                                 imp3 = NA,
                                 imp4 = NA,
                                 imp5 = NA)
      
      #calculate mean savings for each income group and in each implicate
      for (implicate in implicates) {
            
            temp <- get(implicate)
            
            
            for (i in 1:6) {
                  temp2 <- filter(temp, inc.group == i)
                  mean_savings[i,implicate] = weighted.mean( temp2$edu_savings , temp2$wgt )
                  remove(temp2)
            }

            remove(temp)
      }
      
      #average across data frames 
      mean_savings <- mutate(mean_savings, average_savings = ((imp1+imp2+imp3+imp4+imp5)/5))
      
      
      #Compare with anthony damico's script
      mean_savings_2 <- scf.MIcombine( with( scf.design , svyby( ~edu_savings , ~inc.group , svymean ) ) )
      
      #merge in to results
      group.income.stats[c("mean.edu.savings", "mean.savings.se")] <- c(coef(mean_savings_2), SE(mean_savings_2))
      
      
##### calculate total education savings by income group ####
      
      #calculate using damico's script
      total.edu.savings <- scf.MIcombine( with( scf.design , svyby( ~edu_savings_five , ~inc.group , svytotal ) ) )
      
      #add to table 
      group.income.stats[c("total.edu.savings")] <- c(coef(total.edu.savings))


##### Calculate number of households with assets ####

      
      #create variable for "total households"
      total.households <- scf.MIcombine( with( scf.design , svyby( ~five , ~inc.group , svytotal ) ) )
      group.income.stats$total.households <- coef(total.households)
      
      #create variable for total households with edu savings 
      hh.with.edu.sav <- scf.MIcombine( with( scf.design , svyby( ~has.edu , ~inc.group , svytotal ) ) )
      group.income.stats$hh.with.edu <- coef(hh.with.edu.sav)
      
#### calculate percentages ####
      
      #calculate share of total savings
      group.income.stats[c("share.of.edu.savings")] <- c(group.income.stats$total.edu.savings)/sum(group.income.stats$total.edu.savings)
      
      #calculate percent with a savings acct.
      group.income.stats[c("percent.with.edu.savings")] <- c(group.income.stats$hh.with.edu)/c(group.income.stats$total.households)

### add "total" results to the table ####
      
      group.income.stats[7,1] <- "Total"
      group.income.stats[7,2:10] <- c( 
            coef(median_income), # median.income
            coef(mean_income), # mean.income
            coef(scf.MIcombine( with( scf.design , svymean( ~edu_savings ) ) ) ), # mean educational savings
            SE(scf.MIcombine( with( scf.design , svymean( ~edu_savings ) ) ) ),
            coef(scf.MIcombine( with( scf.design , svytotal( ~edu_savings_five ) ) ) ), #total educational savings
            coef(scf.MIcombine( with( scf.design , svytotal( ~five ) ) ) ), #total households
            coef(scf.MIcombine( with( scf.design , svytotal( ~has.edu ) ) ) ), #households with edu savings
            0, #share of edu savings, placeholder value
            0  # percent with edu savings, placeholder value
      )
      
      #calculate share of edu savings (partially a sanity check; should equal 1)
      group.income.stats[7,"share.of.edu.savings"] <- group.income.stats[7,"total.edu.savings"]/sum(group.income.stats[1:6,"total.edu.savings"])
      
      
      #caluculate total share of households with edu savings.
      group.income.stats[7,"percent.with.edu.savings"] <- group.income.stats[7,"hh.with.edu"]/group.income.stats[7,"total.households"]
      
#### print results to console ####
      
      #rename "group" column
      group.income.stats$group <- c(
                                    "1st Quintile (0 - 20%)",
                                    "2nd Quintile (20% - 40%)", 
                                    "3rd Quintile (40% - 60%)",
                                    "4th Quintile (60% - 80%)",
                                    "9th Decile (80% - 90%)",
                                    "10th Decile (90% - 100%)",
                                    "Total"
      )
      
      #Round long numbers
      group.income.stats[,2:8] <- round(group.income.stats[,2:8], 0)
      group.income.stats[,9:10] <- round(group.income.stats[,9:10], 3)
      
      #print to console
      group.income.stats
      
      #write to CSV
      write.csv(group.income.stats,file = "I:/User/Williams/Publications/529Data/rstuff/collegsavingsSCF/SCFCollegeSavingsResults.csv")
      