# MAT 5182 / STAT 5702 Assignment 1 ShinyApp server
# Jessica Perkins
# February 3, 2021

library(shiny)
library(stats)
library(tidyverse)

#Data to pull in from https://covid-19.ontario.ca/data for casesbyAge and from 
#https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=1710000501 for popnCounts
casesbyAge = read.csv("casesByAge.csv")
popnCounts = read.csv("popncounts.csv", 
                      col.names = c("age.group", "count"), 
                      stringsAsFactors = FALSE)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    
    ####### Selecting individuals to vaccinate based on age group          #######
    ####### older individuals get vaccinated first (e.g. all individuals   #######
    ####### in 80+ category get vaccinated, then the 70-79 category, etc.) #######
    
    stats.return = reactive ({
        n = 50000   # number of individuals in sample
        weeks = 31    # number of weeks/iterations we follow them for, 31 weeks brings us from week of March 1st to end of September
        vacc.weekly = n/weeks   # weekly vaccine capacity (to be user-determined or maybe bring in actual capacity data?)
        
        #determine vaccine efficacy depending on user choice in ui
        vacc.eff = input$vacc.eff/100
        
        #table with each age group's death rate of infected individuals in Ontario
        group.death.rt = data.frame(age.group = casesbyAge$category, 
                                    death.rate = casesbyAge$Deaths/casesbyAge$Cumulative)
        
        #sample our individuals proportionally by age group from our Ontario dataset,
        #setting everyone to be not vaccinated, not infected, not recovered, and not dead
        #and they have an invalid infected.week
        indiv.data.set.2 = data.frame(age.group = sample(popnCounts$age.group, 
                                                         n, 
                                                         replace=TRUE, 
                                                         prob = popnCounts$count/sum(popnCounts$count)), 
                                      vaccinated = 0, 
                                      infected = 0, 
                                      recovered = 0, 
                                      death = 0, 
                                      infected.week = -1)
        
        starter.2 = sample(c(1:n), input$inf.start, replace = FALSE)    # randomly select specified people to infect (user-determined)
        indiv.data.set.2[starter.2, 3] = 1       # set starter infected person to infected
        indiv.data.set.2[starter.2, 6] = 1       # set first infected person's infected.week to starter week
        
        #originally pulled value of 0.9 from https://covid-19.ontario.ca/data    
        #Re value -> estimate of the avg number of people 1 person with covid will infect 
        #(to be user-determined). assuming this is over the course of 2 weeks, we need 
        #to divide by two to get the average # of people 1 person with covid will infect 
        #per week. Could be user-determined but I chose that one will infect an average 
        #of 1 person every 2 weeks
        avg.spread = input$spread/2
        
        #create historical matrix, this is what we'll return at the end
        stats.we.keep.2 = matrix(NA, 
                                 nrow = weeks, 
                                 ncol = 5, 
                                 dimnames = list(NULL, c("week.num", 
                                                         "num.vaccinated", 
                                                         "num.curr.infected", 
                                                         "num.recovered", 
                                                         "num.deaths")))
        
        #function to determine who will be vaccinated each week
        new.vaccinated.2 = function(indiv.data.set.2, vacc.weekly, age.groups) {
            remaining.vacc = round(vacc.weekly)
            
            #vaccinating by age group, age.groups matrix is indexed so that we look at
            #the 80+ age group first, 70-79 next, etc.
            #this is because we want to vaccinate all the older individuals first
            for(i in 1:length(casesbyAge$category)) {
                
                #only those who are not already vaccinated, not dead, and not infected may be vaccinated
                eligible.population = subset(indiv.data.set.2, 
                                             indiv.data.set.2$vaccinated == 0 & 
                                                 indiv.data.set.2$infected == 0 & 
                                                 indiv.data.set.2$death == 0 & 
                                                 indiv.data.set.2$age.group == age.groups[i,1])
                
                if (nrow(eligible.population) > 0){
                    #sample from the eligible population to randomly select the individuals to be 
                    #vaccinated, with a size of either eligible population
                    #or of available vaccines for the week, whatever is smallest
                    newly.vaccinated = eligible.population[sample(nrow(eligible.population), 
                                                                  size = min(nrow(eligible.population), 
                                                                             remaining.vacc), 
                                                                  replace = FALSE),]
                    
                    newly.vaccinated$vaccinated = 1   #set the randomly selected individuals to be vaccinated
                    
                    #gives the row id/numbers of everyone who is being vaccinated at this step
                    id.newly.vaccinated = as.numeric(rownames(newly.vaccinated))  
                    
                    #put these vaccinations into our data set dataframe
                    indiv.data.set.2[id.newly.vaccinated, 2] = 1
                    #subtract vaccinations we just gave out from remaining week's capacity
                    remaining.vacc = round(remaining.vacc - length(id.newly.vaccinated))
                }
                if(remaining.vacc <= 0) break()
            }
            return(indiv.data.set.2) #return updated data
        }
        
        #iterate over all weeks
        for(curr.week in 1:weeks) {
            #put in current week data to historical matrix
            stats.we.keep.2[curr.week,] = c(curr.week, 
                                            sum(indiv.data.set.2$vaccinated), 
                                            sum(indiv.data.set.2$infected), 
                                            sum(indiv.data.set.2$recovered), 
                                            sum(indiv.data.set.2$death))
            
            #to determine # of new infected from current # infected and Re spread average
            num.new.infected = rpois(1, avg.spread*sum(indiv.data.set.2$infected))
            
            #probabilities of infection for those vaccinated and not vaccinated respectively
            p.infection.vac = (1-vacc.eff)*num.new.infected/length(
                indiv.data.set.2$infected[indiv.data.set.2$infected == 0])
            p.infection.novac = num.new.infected/length(
                indiv.data.set.2$infected[indiv.data.set.2$infected == 0])    
            
            #randomly select newly recovered individuals from those who were infected & 
            #also do not "unrecover" those who were already recovered
            new.recovered = indiv.data.set.2$death == 0 & 
                (indiv.data.set.2$recovered == 1 | 
                     ( indiv.data.set.2$infected == 1 & 
                           indiv.data.set.2$infected.week <= (curr.week-2) ))
            indiv.data.set.2$recovered = as.integer(new.recovered)   # convert TRUE/FALSE into 1/0 respectively
            
            #call vaccination function from above
            indiv.data.set.2 = new.vaccinated.2(indiv.data.set.2, 
                                                vacc.weekly, 
                                                age.groups = cbind(rev(casesbyAge$category), 
                                                                   1:length(casesbyAge$category)))
            
            #randomly select the newly infected individuals for the week, based on their
            #previous states of death, recovered, infected, vaccinated
            new.infected = indiv.data.set.2$death == 0 & 
                indiv.data.set.2$recovered == 0 & 
                (indiv.data.set.2$infected == 1 |    
                     (indiv.data.set.2$vaccinated == 0 & 
                          (runif(n) < p.infection.novac)) | 
                     (indiv.data.set.2$vaccinated == 1 &  
                          (runif(n) < p.infection.vac)))
            #set infected week to current week
            indiv.data.set.2$infected.week[indiv.data.set.2$infected == 0 & 
                                               new.infected == TRUE] = curr.week  
            indiv.data.set.2$infected = as.integer(new.infected)     # convert TRUE/FALSE into 1/0 respectively
            
            #randomly select the newly dead individuals, based on their age group's death rate/probability
            #age group death rate/probability is calculated from Ontario population data
            new.deaths =  indiv.data.set.2$death == 1 | 
                (indiv.data.set.2$infected == 1 & 
                     (((indiv.data.set.2$age.group == "Under 20" & 
                            runif(n) < group.death.rt$death.rate[group.death.rt$age.group == "Under 20"]) | 
                           (indiv.data.set.2$age.group == "20-29" & 
                                runif(n) < group.death.rt$death.rate[group.death.rt$age.group == "20-29"]) | 
                           (indiv.data.set.2$age.group == "30-39" & 
                                runif(n) < group.death.rt$death.rate[group.death.rt$age.group == "30-39"]) | 
                           (indiv.data.set.2$age.group == "40-49" & 
                                runif(n) < group.death.rt$death.rate[group.death.rt$age.group == "40-49"]) | 
                           (indiv.data.set.2$age.group == "50-59" & 
                                runif(n) < group.death.rt$death.rate[group.death.rt$age.group == "50-59"]) | 
                           (indiv.data.set.2$age.group == "60-69" & 
                                runif(n) < group.death.rt$death.rate[group.death.rt$age.group == "60-69"]) | 
                           (indiv.data.set.2$age.group == "70-79" & 
                                runif(n) < group.death.rt$death.rate[group.death.rt$age.group == "70-79"]) | 
                           (indiv.data.set.2$age.group == "80+" & 
                                runif(n) < group.death.rt$death.rate[group.death.rt$age.group == "80+"])
                     )))
            indiv.data.set.2$death = as.integer(new.deaths)  
        }      
        stats.we.keep.2
    })
    
    #outputs for current value selections
    output$infected.start = renderText({input$inf.start})
    output$re.choice = renderText({input$spread})
    output$efficacy.choice = renderText({input$vacc.eff})
    
    #plot of deaths over time (week number) as well as # of vaccines along the top x-axis
    output$deathsPlot = renderPlot({
        stats.return = stats.return()
        ggplot(data = as.data.frame(stats.return)) + 
            geom_line(aes(x = week.num, y = num.deaths), size = 1, col = "red") + 
            ggtitle("Cumulative Death Count by Week") + 
            xlab("Week Number") + 
            ylab("Cumulative Number of Deaths") +
            scale_x_continuous(sec.axis = sec_axis(trans = ~ . * 50000/31, 
                                                   name = "# of Vaccinations Administered")) +
            theme(plot.title = element_text(size = 20),
                  axis.title = element_text(size = 12),
                  axis.text = element_text(size = 12))
    })
    
    #plot of active cases over time (week num) as well as # of vaccines along top x-axis
    output$casesPlot = renderPlot({
        stats.return = stats.return()
        ggplot(data = as.data.frame(stats.return), 
               aes(x = week.num, y = num.curr.infected)) + 
            geom_line(size = 1, col = "blue") + 
            ggtitle("Active Case Count by Week") + 
            xlab("Week Number") + 
            ylab("Number of Active Cases") +
            scale_x_continuous(sec.axis = sec_axis(trans = ~ . * 50000/31, 
                                                   name = "# of Vaccinations Administered")) +
            theme(plot.title = element_text(size = 20),
                  axis.title = element_text(size = 12),
                  axis.text = element_text(size = 12))
        
    })
    
    #table of data from completed simulation
    output$stats = renderPrint({
        stats.return = stats.return()
        stats.return
        
    })
    
    #correlation of number of deaths and number of vaccines
    output$deathVaccCorr = renderPrint({
        stats.return = stats.return()
        
        cor(c(0, 
              as.data.frame(stats.return)$num.deaths[2:length(as.data.frame(stats.return)$num.deaths)]-
                  as.data.frame(stats.return)$num.deaths[1:length(as.data.frame(stats.return)$num.deaths)-1]), 
            as.data.frame(stats.return)$num.vaccinated)
        
    })    
    
    #linear regression for number of deaths and number of vaccines
    output$deathVaccRegress = renderPrint({
        stats.return = stats.return()
        number.of.vaccinated = as.data.frame(stats.return)$num.vaccinated
        number.of.deaths = c(0, 
                             as.data.frame(stats.return)$num.deaths[2:length(as.data.frame(stats.return)$num.deaths)]-
                                 as.data.frame(stats.return)$num.deaths[1:length(as.data.frame(stats.return)$num.deaths)-1])
        
        model = lm(number.of.deaths ~ number.of.vaccinated)
        model
        
    })
    
})
