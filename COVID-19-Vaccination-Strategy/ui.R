# MAT 5182 / STAT 5702 Assignment 1 ShinyApp ui
# Jessica Perkins
# February 3, 2021

library(shiny)

# Define UI for application
shinyUI(fluidPage(
    
    # Application title
    titlePanel("COVID-19 Vaccination Strategy - Prioritizing Older Age Groups in Ontario, Canada to Minimize Deaths"),
    
    sidebarLayout(
        sidebarPanel(
            sliderInput(
                inputId = "inf.start",
                "Number of infected people to start with:",
                min = 25,
                max = 100,
                value = 70),
            
            sliderInput(
                inputId = "spread",
                "Effective reproduction Number (Re): on average, how many individuals an 
              infected person will infect over the course of their infection:",
                min = 0.7,
                max = 3,
                value = 1),
            
            #let user choose vaccine efficacy based on min of lower bound of Moderna and Pfizer
            #and max of upper bound of Moderna and Pfizer
            #default selection is the average of their estimated efficacies
            sliderInput(
                inputId = "vacc.eff",
                "Vaccine Efficacy",
                min = 89.3,
                max = 97.3,
                value = 94.4,
                post = "%"),
            
            submitButton(
                text = "Apply Changes")
        ),
        
        
        # Show a plot of the generated distribution
        mainPanel(
            verticalLayout(
                navbarPage(title = "Tab Selection",
                           tabPanel("Cases & Deaths Over Time (Plots)", 
                                    verticalLayout(
                                        strong("Current selection:"),
                                        "For a sample size of 50,000 individuals, we have:",
                                        strong(textOutput("infected.start")), "individuals infected, and each of
                                  these people will on average infect an estimated number of ", 
                                        strong(textOutput("re.choice")), "other individual(s). Once individuals begin 
                                  receiving the vaccine, they will be ", strong(textOutput("efficacy.choice")),
                                        "% less likely to be infected.",
                                        br(),
                                        br(),
                                        plotOutput("deathsPlot"),
                                        br(),
                                        "Using the default settings of 70 people infected to start with, Re = 1, and 
                                  a vaccine efficacy of 94.4%, the sample has minimal deaths by the end of the simulation (usually <15), and
                                  the deaths quickly slow down at around the 5,000 - 15,000 mark of administered vaccines (roughly week 5). By 
                                  the time we hit 10,000 vaccines, roughly week 7, the deaths have usually peaked.", br(),
                                        "To comment on an extreme example of the application settings, if Re is set to 3 with the other settings
                                  remaining the same, we see many more deaths (usually 400-500) and the deaths do not begin to taper off until
                                  around 20,000 vaccines administered, or week 13.",
                                        br(),
                                        plotOutput("casesPlot"),
                                        br(),
                                        "Similar to the Cumulative Deaths by week graph, with the default settings of 70 infected to start,
                                  Re = 1, and a vaccine efficacy of 94.4%, the case count peaks between 5,000 - 10,000 vaccines administered, and
                                  starts a sharp trend downwards. By the time we hit 20,000 vaccines administered (40% of the sample),
                                  the active case count is already approaching 0.", br(),
                                        "To again comment on the extreme example of the settings, with Re = 3 and the other settings at their defaults,
                                  the active case count increases roughly exponentially until it peaks at around 15,000 vaccines administered, 
                                  or around week 10. After this point it quickly drops over the course of a few weeks.",
                                        br())),
                           tabPanel("Deaths vs. Vaccines (Regression)", 
                                    verticalLayout(
                                        h1("Relationship between Deaths & Vaccines"),
                                        verbatimTextOutput("deathVaccCorr"),
                                        "With the default settings of 70 individuals infected to start, a spread of Re = 1, and a vaccine
                                  efficacy of 94.4%, the correlation between the number of deaths and the number of vaccinated
                                  individuals ranges between -0.3 to -0.6.", br(),
                                        "Examining again the extreme case where the settings are all set to their defaults except for the
                                  spread (Re) which we set to 3, the correlation stays within the range of -0.3 to -0.6", br(),
                                        "The results of performing linear regression on the two variables can be found below:", br(),
                                        verbatimTextOutput("deathVaccRegress"),
                                        br()
                                        
                                    )),
                           tabPanel("Simulated Data Table",
                                    verticalLayout(
                                        h1("Results of the simulation, by week:"), br(),
                                        verbatimTextOutput("stats"))),
                           tabPanel("About", 
                                    h1("Goal"),
                                    "The goal of this ShinyApp is to showcase the 'best' COVID-19 Vaccination 
                       Strategy of giving vaccines to older age groups first, prioritizing the 
                       oldest group before moving onto the next. 'Best' in this 
                       application is defined as the strategy that minimizes deaths. The strategy/simulation
                       examines the public vaccination phase and assumes the vaccinations are complete (everyone
                       has been vaccinated) by the end of September 2021 (week 31)",
                                    hr(),
                                    h1("Assumptions"),
                                    "- all background information (therefore any proportions or probabilities)
                       are based off of data for population counts and COVID-19 cases for Ontario, 
                       Canada only. Data sources found in the section below.", br(),
                                    "- all individuals in the simulation are aged 16 or older so that they are able to get the vaccine (at ",
                                    a(href="http://www.health.gov.on.ca/en/pro/programs/publichealth/coronavirus/docs/vaccine/COVID-19_information_sheet_pfizer-biontech.pdf", 
                                      "health.gov.on.ca"),
                                    "it's stated only individuals 16 and older can receive the Pfizer vaccine, and 18 and older can receive the Moderna vaccine)", br(),
                                    "- only one vaccination is considered, but the efficacy bounds are based off of the actual bounds of both the
                       Pfizer and Moderna vaccines, and the default value (94.3% efficacy) is from the average of both vaccines'
                       expected efficacy.", br(),
                                    "- we assume that an individual is considered to be vaccinated after they have received both doses
                       of the vaccine, and the recommended amount of time after dose 2 has occurred for the vaccine to 
                       become fully effective. Until then, they are considered to not be vaccinated at all.", br(),
                                    "- because we are only examining the general public vaccination phase, I have assigned a constant
                       value to the number of vaccinations per week, found by dividing the sample size by the number of weeks. This is because
                       I assume that by the time Ontario is vaccinating the general public, we will have roughly constant vaccination capacity.", br(),
                                    "- no individuals are vaccinated, dead, or recovered at the beginning of the simulation", br(),
                                    "- an individual who is recovered cannot get re-infected", br(),
                                    "- an individual who is currently infected cannot get vaccinated", br(),
                                    "- an individual can only die from COVID-19, therefore they can only die while infected", br(),
                                    hr(),
                                    h1("Data Sources"),
                                    "The data used for any assumptions, rates, and decisions are found below:", br(),
                                    "-",
                                    a(href="https://covid-19.ontario.ca/data",
                                      " Ontario COVID-19 Data Webpage"),
                                    " used for determining the proportions of cases by age group, as well as finding lower and upper bounds
                       of the Re / effective reproduction number", br(),
                                    "-", 
                                    a(href="https://www150.statcan.gc.ca/t1/tbl1/en/tv.action?pid=1710000501",
                                      " Statistics Canada Webpage"), " for Ontario's population counts by age group", br(),
                                    "-", 
                                    a(href="https://covid-vaccine.canada.ca/info/pdf/pfizer-biontech-covid-19-vaccine-pm1-en.pdf",
                                      " Product Monograph - Pfizer-BioNTech"), " for the Pfizer-BioNTech vaccine's efficacy estimate and its confidence interval, found at the ",
                                    a(href="https://www.canada.ca/en/health-canada/services/drugs-health-products/covid19-industry/drugs-vaccines-treatments/authorization/list-drugs.html",
                                      " Government of Canada Webpage"), br(),
                                    "-",
                                    a(href="https://covid-vaccine.canada.ca/info/pdf/covid-19-vaccine-moderna-pm-en.pdf",
                                      " Product Monograph - Moderna"), " for the Moderna vaccine's efficacy estimate and its confidence interval, found at the ",
                                    a(href="https://www.canada.ca/en/health-canada/services/drugs-health-products/covid19-industry/drugs-vaccines-treatments/authorization/list-drugs.html",
                                      " Government of Canada Webpage"), br(),
                                    hr(),
                                    h1("Author"),
                                    "Jessica Perkins", br(), 
                                    "MSc student studying Statistics at the University of Ottawa", br(),
                                    a(href="https://github.com/js-perkins/COVID-19-Vaccination-Strategy", 
                                      "Github"), br(),
                                    a(href="https://www.linkedin.com/in/jessica-perkins-96222a157/", 
                                      "LinkedIn"), br(),br(), br()
                           )
                )
            )
        )
    )
))
