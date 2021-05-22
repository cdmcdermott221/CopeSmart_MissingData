#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
?tableOutput()
#

library(htmltools)
library(bslib)
library(shiny)
library(e1071)
library(sn)
library(ggplot2)
library(shinydashboard)

library(dplyr)
library(faux)
library(mice)
library(Amelia)
library(Rmisc)
library(shiny)
library(shinydashboard)
library(grid)
library(ggpubr)
library(shinyWidgets)

library(fresh)
# Create the theme
mytheme <- create_theme(
    adminlte_color(
        light_blue = "#004466"))



css <- '.nav-tabs>li>a {color: #004466 !important; font-weight: bold;}'


# Define UI for application that draws a histogram
ui <- dashboardPage(
    dashboardHeader(title = "Simulator Tool"),
    dashboardSidebar(
        sidebarMenu(
            menuItem("Home", tabName = "home", icon = icon("home")),
            menuItem("Case Study", tabName="casestudy", icon = icon("book-open")),
            menuItem("Intro to Missing Data", tabName = "md", icon = icon("align-left")),
            menuItem("Intro to Multiple Imputation", tabName = "mi", icon = icon("align-left")),
            menuItem("Intro to Sensitivity to Bias", tabName = "stb", icon = icon("align-left")),
            menuItem("Example Sensitivity to Bias", tabName = "exstb", icon = icon("binoculars")),
            menuItem("Enter Data", tabName = "data", icon = icon("calculator")),
            menuItem("Customise Sensitivity to Bias", tabName = "custstb", icon = icon("calculator")),
            menuItem("Customise Multiple Imputation", tabName = "custmi", icon = icon("calculator")),
            menuItem("Final Remarks", tabName = "final", icon = icon("align-left")))),
        
    dashboardBody(
        setBackgroundColor(color = "#f2f2f2", shinydashboard = T),
        use_theme(mytheme), # <-- use the theme
        tags$head(tags$style(HTML(".skin-blue .main-sidebar {
                              background-color: #004466; font-color: #333333;}"))), 
        
        
        
        tags$head(tags$style(HTML("
                            .small-box {background-color: #FFFFFF !important; border-radius: 2vh !important; border-style: solid; border-color: #00cc99 !important;}
                            .small-box h3 {font-size: 4vh !important; color: #333333 !important;}
                            .small-box h4 {font-size: 4vh !important; color: #333333 !important;}
                            .small-box h5 {font-size: 4vh !important; color: #333333 !important;}
                            .small-box h6 {font-size: 4vh !important; color: #333333 !important;}
                            .small-box p {color: #333333 !important;}"))),
        
        tags$head(tags$style(HTML("
                            .box {background-color: #FFFFFF !important;border-radius: 1vh !important; border-style: solid; border-color: #000000 !important;}
                            .box h3 {font-size: 2vh !important; color: #333333 !important;}
                            .box h4 {font-size: 2vh !important; color: #333333 !important;}
                            .box h5 {font-size: 2vh !important; color: #333333 !important;}
                            .box h6 {font-size: 2vh !important; color: #333333 !important;}
                            .box p {color: #333333 !important;}"))),

        tags$head(
            tags$style(type='text/css', 
                       ".nav-tabs {font-size: 17px; font-color: #333333;} 
                       .nav-tabs>li>p {color: #004466;}")),
        
         tags$head(
            tags$style(type='text/css', 
                       ".table {font-size: 17px; color: #333333;} ")),
        
        tags$head(tags$style(HTML(css))),
        
        tabItems(
            # First tab content
            tabItem(tabName = "home",
                    br(),
                    br(),
                    br(),
                    br(),
                    br(),
                    br(),
                    h1(align = 'center', tags$sup(style="font-size: 40px", strong("Sensitivity to Bias Simulation Tool for Missing Data"))),
                    h4(align = 'center', "Created by Courtney McDermott"),
                    br(),
                    fluidRow(
                        column(10, offset = 1,
                               wellPanel(style = "background-color: #fff; border-color: #2c3e50 !important; border-width: thick; margin-top: 7px;",
                               p(align='center', "This website is a tutorial-led simulation tool that walks users through the steps to perform a 
                                 'sensitivity to bias analysis' on variables with missing data.",style = "font-size: 150%;"),
                               br(),
                               p(align = 'center', "The examples in this tutorial focus on normally-distributed, and interpret the results.",style = "font-size: 150%;"),
                               br(),
                               p(align = 'center', "Finally, this tool allows users to input their own data and run their own sensitivity to bias 
                               analysis. Users will be taught how to investigate the potential biases that may be introduced from missing data; how to 
                               interpret the results of a sensitivity to bias analysis; and how to investigate the performance of multiple imputation 
                                 when data are missing not randomly.",style = "font-size: 150%;"))))
                     ),
            
            tabItem(tabName = "casestudy",
                    br(),
                    h1(align='center', tags$sup(style="font-size: 40px", strong("Example Case Study"))),
             
                    fluidRow(
                        column(10, offset = 1,
                               wellPanel(style = "background-color: #fff; border-color: #2c3e50 !important; border-width: thick; margin-top: 7px;",
                               p(align = 'center', "Let's pretend that a survey was conducted on a sample of 1000 adults from the Irish population. 
                               Data on the drinking habits of the participants (18 years and older) were collected. The purpose of this study was to 
                               determine the average amount of alcohol consumed per week, to then decide if stricter alcohol-related policies should 
                               be introduced to reduce the amount of alcohol consumed by the population. The policy-makers decided ahead of time 
                               that if the mean was larger than 95oz, they would introduce new policies; if it was under 95oz, they would not make 
                               any new changes. One question in the survey asked individuals how many ounces of alcohol they consumed, on average, 
                                 per week.",style = "font-size: 130%;"), 
                               br(),
                               p(align = 'center',"After the study finished, researchers determined that the mean number of ounces consumed was 
                               90oz, with a standard deviation of 20oz. However, they found that only 75% of the study participants answered that 
                               question. Alcohol consumption is a topic that could be seen as a 'sensitive' topic. Do you think that it is likely 
                                 that these data were missing randomly? And if not, can we trust the results we see from the analyses?", style = "
                                 font-size: 130%;"),
                               br(),
                               p(align='center', "We will attempt to answer these questions by investigating various possible missing data scenarios 
                               through a sensitivity to bias analysis. We will then determine if our results are 'robust' or 'sensitive' to bias, and 
                               how confident we can be in our results. Finally, we will investigate how multiple imputation may be biased in the 
                               presence of missing data, and 
                                 the degree to which an auxiliary variable can reduce this bias.", style = "font-size: 130%;"), width = 12)))
                    ),
   
            tabItem(tabName = "md",
                    br(),
                    h1(align='center', tags$sup(style="font-size: 40px", strong("Introduction to Missing Data"))),
                
                    fluidRow(
                        column(12,
                               wellPanel(style = "background-color: #fff; border-color: #2c3e50 !important; border-width: thick; margin-top: 7px;",
                                         fluidRow(
                                             column(10, offset=1,
                                             p(align = 'center', "Regardless of the reason, missing data results in blanks in the dataset, which in turn 
                                             results in a loss of information. There are three main patterns, or mechanisms, in which missing data arises. 
                                             The type of pattern impacts the analysis of the data and can impact the confidence that a researcher may have 
                                             in the results and conclusions. The following tabs will discuss and present the three missing data patterns 
                                               in the context of our case study.", style = "font-size: 130%;"))), 
                                         br(),
                                         tabsetPanel(type = "tabs",
                                                     tabPanel("How Does it Arise?",
                                                              fluidRow(
                                                                  column(6,
                                                                         br(),
                                                                         wellPanel(style = "background-color: #fff; border-color: #2c3e50 !important; border-width: thin; margin-top: 7px; height: 440px; 
                                                                            width: 600;", br(),
                                                                            p(align = 'justify', tags$sup(style="font-size: 16px", "Missing data can arise in biomedical studies in many ways. 
                                                                            For example, participants may choose to skip over a question on a survey and leave it blank. They may choose to 
                                                                            skip over more than one question, or to skip an entire section. Or maybe the participant found the question 
                                                                            confusing and gave a nonsensical answer. Perhaps the survey was long and the participant stopped answering questions 
                                                                                                          near the end of the survey due to survey fatigue.")),
                                                                            br(),
                                                                            p(align = 'justify',tags$sup(style="font-size: 16px", "In a clinical trial, missing data may arise from particpants 
                                                                            choosing to not show up to get their data collected. Similarly, a participant may choose to drop out of the study entirely.")),
                                                                            br(),
                                                                            p(align = 'justify',tags$sup(style="font-size: 16px", "For each of the three following missing data patterns, 150 individuals 
                                                                            were randomly selected from our case study of  alcohol consumption. The mean ounces of alcohol consumed for the full dataset 
                                                                            (if we knew the values of the missing data) was simulated to be 95, with a standard deviation of 20. A total of 15% of the 
                                                                                                         observations were selected to be missing, and are represented in pink. The observed values are 
                                                                                                         represented in blue.")))),
                                                                  column(6,
                                                                            br(),
                                                                            wellPanel(style = "background-color: #fff; border-color: #2c3e50 !important; border-width: thin; margin-top: 7px; height: 440px;
                                                                                      width: 600;",
                                                                                      img(src='puzzle2.png', align = "center", height = 400, width = 510))))),
                                                     tabPanel("Missing Completely At Random", 
                                                              fluidRow(
                                                                  column(5,
                                                                         br(),
                                                                         wellPanel(style = "background-color: #fff; border-color: #2c3e50 !important; border-width: thin; margin-top: 7px; height: 470px; 
                                                                            width: 600;",
                                                                            p(align = 'justify',"Data are said to be", strong("Missing Completely at Random (MCAR)"), "when the mechanism 
                                                                            driving the missingness is unrelated to the observed values or the missing values. Some examples of Missing 
                                                                            Completely at Random would be survey fatigue, not understanding the question, or a computer glitching and 
                                                                            skipping over a section. For our case study, data may be MCAR if individuals were in a rush and randomly 
                                                                            skipped over some of the questions in this survey in order to get to the end. Data may also be MCAR if the 
                                                                            participants were confused about how to determine ounces of alcohol consumed, and so skipped that question 
                                                                              because of confusion.", style = "font-size: 103%;"),
                                                                            br(),
                                                                            p(align = 'justify',"When data are Missing Completely at Random, the missing values will not be significantly 
                                                                            different from the observed values. The graph to the right shows 15% of values missing completely at random. 
                                                                            As we can see, the missing values have a similar spread as the observed values along the y-axis. When data 
                                                                            are MCAR, they are said to be 'ignorable' because it is unlikely to introduce bias into the analysis. However, 
                                                                            missing data will ", strong("always"), " result in a loss of information and loss of sample size. This is 
                                                                              likely to increase our confidence intervals, which may lead to a loss of power.", style = "font-size: 103%;"))),
                                                                  column(7, 
                                                                            br(),
                                                                            wellPanel(style = "background-color: #fff; border-color: #2c3e50 !important; border-width: thin; margin-top: 7px; 
                                                                                      height: 470px; width: 600;",plotOutput("missingmcar", height = "100%"))))),
                                                     tabPanel("Missing At Random",
                                                              fluidRow(
                                                                  column(4,
                                                                         br(),
                                                                  wellPanel(style = "background-color: #fff; border-color: #2c3e50 !important; border-width: thin; margin-top: 
                                                                            7px; height: 490px; width: 600;",
                                                                            p(align = 'justify',"Data are said to be", strong(" Missing At Random (MAR)"), " when the mechanism 
                                                                            for missingness is dependent upon an observed covariate. For example, older participants may be more 
                                                                            likely to skip over questions than younger participants; Women may be more likely to answer than men.",
                                                                            style = "font-size: 105%;"),
                                                                            br(),
                                                                            p(align = 'justify',"The graphs to the right again represent 150 individuals from our case study. 
                                                                            Exactly 50% (75 indivudals) of the sample were men, and 50% were women. The data were simulated 
                                                                            to be MAR dependent upon Gender. We can see that women had much fewer missing observations than 
                                                                            men. However, within each category of Gender, we can see that the data are still missing randomly. 
                                                                            As with MCAR, MAR is unlikely to introduce bias into the priary analysis, but again, we are still 
                                                                              losing information from the loss of sample size.", style = "font-size: 105%;"))),
                                                                  column(8,
                                                                         br(),
                                                                         wellPanel(style = "background-color: #fff; border-color: #2c3e50 !important; border-width: thin; margin-top: 
                                                                                   7px; height: 490px; width: 600;",
                                                                                   plotOutput("missingmar", height = "100%"))))),
                                                     tabPanel("Missing Not At Random",
                                                              fluidRow(
                                                                  column(5,
                                                                         br(),
                                                                         wellPanel(style = "background-color: #fff; border-color: #2c3e50 !important; border-width: thin; margin-top: 7px; 
                                                                            height: 495px; width: 600;",
                                                                            p(align = 'justify',"Data are", strong(" Missing Not At Random (MNAR) "), "when the mechanism for missingness 
                                                                            is dependent upon the values, had they been observed. In other words, if those that skipped a question were more 
                                                                            likely to have extreme (high/low) values, the data will be missing not randomly. Examples of this may be if 
                                                                            those who have worse outcomes are more likely to drop out of a study because they are not seeing the results 
                                                                            they want. For our case study, if individuals who consume much higher volumes of alcohol are more likely to 
                                                                              skip that question, the data will be MNAR."),
                                                                            br(),
                                                                            p(align = 'justify',"When data are Missing Not At Random, the risk of bias in the primary analyses is much 
                                                                            higher. If those who have higher values are the ones more likely to not answer, the mean of the data will 
                                                                            be biased downward. We can see this in the graph to the right. The 15% of values that are missing are 
                                                                            more likely to be located in the top half of the graph, or higher alcohol consumption values. In this 
                                                                            example, the mean of the overall data (observed + missing) is 95 oz. However, the mean of the missing 
                                                                            values is 104.1 oz and the mean of the observed data is 93.3 oz. We can see that, since the missing 
                                                                            values are larger, the mean of the observed values is lower than the true mean of the full data."))),
                                                                  column(7,
                                                                         br(),
                                                                         wellPanel(style = "background-color: #fff; border-color: #2c3e50 !important; border-width: thin; margin-top: 
                                                                            7px; height: 495px; width: 600;",
                                                                                   plotOutput("missingmnar", height="100%"))))),
                                                     
                                                     tabPanel("Missing Data Impact",
                                                              fluidRow(
                                                                  column(7,
                                                                         br(),
                                                                         wellPanel(style = "background-color: #fff; border-color: #2c3e50 !important; border-width: thin; margin-top: 7px; 
                                                                            height: 550px; width: 600;",
                                                                            h4(strong("Why Should We Care About Missing Data?")),
                                                                            p(align = 'justify', "As mentioned in the previous sections, missing data will always result in a loss of 
                                                                            information. This will mean a loss of sample size in our analyses, which can impact power. More importantly, 
                                                                            if data are Missing Not at Random, the analyses may produce biased estimates. The example in the previous 
                                                                            section demonstrated that the estimates of the observed data, after 15% of data were simulated to be Missing 
                                                                            Not at Random, were almost 2 ounces lower than the 'true' value of that data. Data that are MNAR can cause 
                                                                            the analyses to over- or under-estimate the results, which can have negative downstream effects."),
                                                                            br(),
                                                                            
                                                                            h4(strong("Impacts on Observational Studies")),
                                                                            
                                                                            p(align = 'justify', "Data from observational studies are frequently used to draw conclusions between health 
                                                                            factors, and health outcomes. The results may also be used to determine the prevalence of a health concern 
                                                                            in a population, which in turn could be used to inform policy decisions. If the results are biased due to 
                                                                            missing data, researchers and policy-makers may end up making inaccurate conclusions. Policies or other 
                                                                            decisions may be made on these inaccurate results and could be unnecessary and costly. For our case study, 
                                                                            the policy-makers have decided to introduce new policies to reduce unsafe alcohol consumption if the mean 
                                                                            of this variable is over 95 oz. In the example in this section, the true mean of this variable--had everyone 
                                                                            answered the question--was 95 oz. However, 15% didn't respond. In the MNAR scenario, individuals with higher 
                                                                            levels of alcohol consumption were more likely to skip that question, and so the new estimated mean of the 
                                                                            observed data was 93 oz. This would have major impacts on the conclusions, as researchers and policy-makers 
                                                                            would conclude that the mean is under the threshold of 95 oz and new measures should not be introduced. 
                                                                              Missing data can result in researchers missing the true effect and drawing the wrong conclusions!"))),
                                                                  column(5, 
                                                                            br(),
                                                                            wellPanel(style = "background-color: #fff; border-color: #2c3e50 !important; border-width: thin; margin-top: 7px; 
                                                                                      height: 550px; width: 450;",
                                                                                      img(src='missed.target.png', align = "center", height = 320, width = 420),
                                                                                      br(),
                                                                                      br(),
                                                                                      br(),
                                                                                      h4(strong("What Can We Do?")),
                                                                                      p(align = 'justify', "To reduce the potential negative impacts of missing data, we can implement a 
                                                                                      missing data method. The next section will introduce one of these methods, multiple imputation. 
                                                                                        However, other missing data methods exist that we could also implement."))))
                                    )))))),
            tabItem(tabName = "stb",
                    br(),
                    h1(align='center', tags$sup(style="font-size: 40px", strong("What is a Sensitivity to Bias Analysis?"))),
                  
                    wellPanel(style = "background-color: #fff; border-color: #2c3e50 !important; border-width: thick; margin-top: 7px; height: 9
                              50px; width: 600;",
                              fluidRow(
                                  column(10, offset = 1,
                                  p(align = 'center', "A sensitivity to bias analysis is an investigative tool to help  answer the question: 
                                  'If my data are actually Missing Not at Random, and I'm using a method that assumes the data are Missing at 
                                    Random--how extreme would the missing values have to be for my MAR-based method to generate biased results?'", 
                                    style = "font-size: 130%;"))),
                              br(),
                              br(),
                              tabsetPanel(type = "tabs",
                                          tabPanel("Why Perform a Sensitivity to Bias Analysis?",
                                          wellPanel(style = "background-color: #fff; border-color: #2c3e50 !important; border-width: thin; margin-top: 7px; 
                                                    height: 730px; width: 600;",
                                                    fluidRow(
                                                        column(6,
                                                               br(),
                                                               br(),
                                                               p(align = 'justify', "As described in the previous sections, missing data can have serious 
                                                               impacts on the primary analyses of the data, particularly if the data are MNAR. Two issues 
                                                               arise from this: The first is that most missing data methods, such as multiple imputation, 
                                                               operate under the Missing At Random assumption. So what happens if the missing data are 
                                                               in fact Missing Not at Random? The multiple imputation estimates will be biased. Similarly,
                                                               if you choose to ignore the missingness and analyse only the observed data, the estimates 
                                                               can be valid when data are MCAR or MAR, but the estimates will be biased if the data are MNAR. 
                                                               The second issue is that we will never know what those missing values were, and therefore 
                                                               we will never know with 100% certainty the pattern of missingness. Sometimes, it is highly 
                                                               unrealistic that the data will be MAR or MCAR. So if we know that MNAR data will bias the 
                                                                 results, but we are unable to determine the pattern of missingness, what can we do?",
                                                                 style = "font-size: 115%;"),
                                                               br(),
                                                               p(align = 'justify', "We can implement a missing data technique to try to recover those 
                                                               missing values; however,imputation can be biased if the data are MNAR. Therefore, it is 
                                                               impossible to know if our estimates from using imputation (or any other missing data method 
                                                               that assumes MAR, including analysing only the observed data), are accurate or biased. One 
                                                               thing we can do, however, is to test different assumptions about the distribution/values of 
                                                               the missing data and see if our conclusions from multiple imputation (or another MAR-based 
                                                               method) remain the same. In other words, we can run a", strong(" sensitivity to bias 
                                                               analysis"), ". This type of analysis will investigate how robust your estimates are from 
                                                                 departures to the MAR assumption.", style = "font-size: 115%;"), 
                                                               br(),
                                                               p(align = 'justify', "A sensitivity to bias analysis is an ", strong("investigative tool "), 
                                                               "into determining what values the missing data would have to be centred around for the 
                                                               conclusions to be significantly different from an analysis that assumes the data are Missing 
                                                               At Random.", style = "font-size: 115%;")),
                                                        column(6,
                                                               br(),
                                                               br(),
                                                               img(src='conclusion.png', align = "center", height = "90%", width = "100%"),
                                                               br(),
                                                               br(),
                                                               br(),
                                                               p(align = 'justify', "If the missing data are centred around a mean that is only slightly different 
                                                               from the observed data, and yet the conclusions are very different from a MAR-based analysis 
                                                               (like multiple imputation)--the MAR-based estimates would be very sensitive to bias. However, if 
                                                               the missing data are centred around a mean that is very extreme compared to the observed data, 
                                                               and yet the conclusions do not change significantly from a MAR-based analysis, the MAR-based 
                                                               estimates would be robust to bias. This can help us determine if we should trust the results 
                                                               generated from a method such as multiple imputation or analysing only the observed data, even 
                                                                 in the presence of MNAR data.", style = "font-size: 115%;"))))),
                                          
                                          tabPanel("Methods",
                                          wellPanel(style = "background-color: #fff; border-color: #2c3e50 !important; border-width: thin; margin-top: 
                                                    7px; height: 730px; width: 600;",
                                                    fluidRow(
                                                        br(),
                                                        column(5,
                                                               br(),
                                                               p(align = 'justify', "The sensitivity to bias analysis can help researchers determine if, in the presence 
                                                               of missing data, their results should be interpreted with caution or with confidence. This tool will
                                                                 help to answer the question: ", style = "font-size: 115%;", strong("'If my data are actually Missing 
                                                                 Not at Random, and I'm using a method that assumes the data are Missing at Random--how extreme 
                                                                 would the missing values have to be for my MAR-based method to generate biased results?'")),
                                                               br(),
                                                               p(align = 'justify', "The steps to perform this analysis are as follows:", style = "font-size: 115%;"),
                                                               br(),
                                                               p(align = 'justify', "1. Impute the missing values to be centred around (in other words, have a mean of) 
                                                               increasingly more extreme values. These values will be both higher and lower than the mean of the 
                                                                 observed data.", style = "font-size: 115%;"),
                                                               p(align = 'jusify',"2. Each set of imputed values will be combined with the observed data to create 
                                                                 a set of new, 'complete' variables.", style = "font-size: 115%;"),
                                                               p(align = 'justify', "3. Each new variable will re-analysed using the primary analysis.", 
                                                                 style = "font-size: 115%;"),
                                                               p(align = 'justify', "4. The results of each analysis will be compared to the results of 
                                                                 the MAR-based method, such as multiple imputation.", style = "font-size: 115%;"),
                                                               br(),
                                                               p(align = 'justify',"The user will investigate the range of values that the missing data 
                                                               can be centred around where a method like multiple imputation will not generate signficantly 
                                                                 different results from the 'true' estimations. ", style = "font-size: 115%;")),
                                                        br(),
                                                        column(7,
                                                        wellPanel(style = "background-color: #fff; border-color: #2c3e50 !important; border-width: thin; margin-top: 
                                                                  7px; height: 500px; width: 300;",
                                                                  br(),
                                                                  img(src='single.png', align = "center", height = "100%", width = "100%"),
                                                                  br()),
                                                        br(),
                                                        br(),
                                                        p(align = 'justify', "There are many values that a researcher can choose to for the missing data to be centred
                                                        around. One method to run a sensitivity to bias analysis could be to impute the values to be centred around 
                                                          the 10th, 20th, 30th, 40th, ....90th percentiles of the observed data.", style = "font-size: 115%;"))))),
                                          
                                          tabPanel("Quick Example",
                                          wellPanel(style = "background-color: #fff; border-color: #2c3e50 !important; border-width: thin; margin-top: 7px; 
                                                    height: 670px; width: 600;",
                                                    fluidRow(
                                                        column(5,
                                                               br(),
                                                               br(),
                                                               p(align = 'justify', "Let's say we have a variable with a mean of 50 and standard deviation of 16. Only 
                                                               75% of participants responded to this variable, so 25% of the data are missing. We think that it is 
                                                               likely that, if the data are in fact MNAR, that the individuals who chose not to respond had lower 
                                                               values on average. We decide to impute these missing values to be centred around the 20th percentile 
                                                               value (~28) and the 40th percentile value (~48) of the observed data. These imputed values are then 
                                                                 combined with the observed data to create two new variables. This is shown in the first figure 
                                                                 ('Missing Data Distributions').", style = "font-size: 115%;"),
                                                               br(),
                                                               p(align = 'justify', "The means of these two new variables are plotted in the second figure ('Total 
                                                               Data Distributions'), in addition to the distribution of the data if we had used a MAR-based missing 
                                                               data method such as multiple imputation. We  can see that, as the means of the missing values are 
                                                               located further away from the mean of the observed data, the distributions become wider, slightly tilted, 
                                                               and shifted to the left of the MAR-based distribution. At a first glance, it seems that, if the missing 
                                                               values were truly centred around the 20th or 40th percentiles, the overall distribution would not be 
                                                               largely impacted when 15% of observations are missing. It appears, visually, that these data are 
                                                                 fairly robust to bias.", style = "font-size: 115%;")),
                                                        column(7,
                                                               br(),
                                                               tabsetPanel(type = "tabs",
                                                                           tabPanel("Missing Data Distributions",
                                                                                    br(),
                                                                                    br(),
                                                                                    img(src='miss1040.png', align = "center", height = "85%", width = "100%")),
                                                                           
                                                                           tabPanel("Total Data Distributions",
                                                                                    br(),
                                                                                    img(src='total1040.png', align = "center", height = "85%", width = "100%")))))))))),
            
            tabItem(tabName = "mi",
                    br(),
                    h1(align = 'center', tags$sup(style="font-size: 40px", strong("Brief Introduction To Multiple Imputation"))),
                    fluidRow(
                        column(12,
                        wellPanel(style = "background-color: #fff; border-color: #2c3e50 !important; border-width: thick; margin-top: 7px; 
                                  height: 1070px; width: 600;",
                                  p(align = 'center', "A commonly-employed missing method is multiple imputation. This method works by attempting to 'guess' what 
                                  those missing values would have been. The imputed values are replaced for the missing values and the new data 
                                    (imputed + observed) are analysed with the primary analysis.", style = "font-size: 130%;"),
                                  br(),
                                  br(),
                                  tabsetPanel(type = "tabs",
                                              tabPanel("How Does it Work?", 
                                                       fluidRow(
                                                           column(6,
                                                           wellPanel(style = "background-color: #fff; border-color: #2c3e50 !important; border-width: thin; margin-top: 
                                                           7px; height: 530px; width: 600;",
                                                                     h4(strong("What is Multiple Imputation?")),
                                                                     p(align = 'justify',tags$sup(style="font-size: 15px", "Multiple imputation is a commonly-used 
                                                                     missing data method that has the ability to generate unbiased estimates from the primary analysis. 
                                                                     This method generates a guess for each missing value by using information from an imputation model. 
                                                                     In this imputation model, the user specifies variables that they believe will help this model 
                                                                     with estimating appropriate values for the missing data. Typically, these imputation model 
                                                                     variables will be strong predictors of the variable with missingness. Variables that are highly 
                                                                     correlated with either the variable of interest, or the mechanism of missingness, are called ", 
                                                                     strong("auxiliary variables"), ". These variables are very beneficial to the imputation model 
                                                                     as they help the program estimate more accurate values for the missing data."),
                                                                     br(),
                                                                     p(align = 'justify', tags$sup(style="font-size: 15px", "For example, let's say researchers were 
                                                                     collecting data on body fat percentage, and a selection of individuals opted out of this measurement. 
                                                                     A strong auxiliary variable may be if the researchers also collected VO2 max level, or some other 
                                                                     measurement of fitness. These two variables could be highly correlated with one another, and therefore 
                                                                     the VO2 max variable could help recover some of the lost information from the missingness in the body 
                                                                     fat percentage variable. For our case study, our researchers could ask a less sensitive alcohol consumption 
                                                                     question, such as 'How many days per week do you consume alcohol?'. If this question had low levels of
                                                                     missingness, this variable may act as a strong auxiliary variable to help recover the lost information
                                                                     from the primary alcohol consumption question. Auxiliary variables are particularly useful to the imputation 
                                                                                                   model when the data are Missing Not at Random."))))),
                                                           column(6,
                                                           wellPanel(style = "background-color: #fff; border-color: #2c3e50 !important; border-width: thin; margin-top: 7px; 
                                                           height: 530px; width: 600;",
                                                                     h4(strong("How is it Estimated?")),
                                                                     p(align = 'justify',tags$sup(style="font-size: 15px", "The imputation method will guess these missing 
                                                                     values by sampling from the posterior distribution of the observed data, using the variables from the 
                                                                     imputation model. The method chosen to impute the values will depend on the distribution of the data. 
                                                                     For more information, see X reference. After the imputation, the result is a full dataset of observed 
                                                                     and imputed values. This dataset is then analysed by whatever statistical model is of interest (i.e. t-test, 
                                                                                                  regression, correlation, etc).")),
                                                                     br(),
                                                                     h4(strong("Pooling the Results")),
                                                                     p(align = 'justify',tags$sup(style="font-size: 15px", "Described above is single imputation. However, 
                                                                     analysing these imputed datasets does is not correct, as the primary analysis does not take into account 
                                                                     the fact that these are just guesses and not the real values. To appropriately account for this 
                                                                     uncertainty, we run this imputation method multiple times, ending up with many datasets. This is called 
                                                                     multiple imputation. Each dataset is analysed using the primary analysis method, and then the results 
                                                                                                  are pooled using Rubin’s rules (link in third tab).")),
                                                                     br(),
                                                                     h4(strong("Implementing in a Statistical Program")),
                                                                     p(align = 'justify',tags$sup(style="font-size: 15px", "Multiple imputation can be run in many statistical 
                                                                     programmes, but for this tool it is run in R using the package “mice”. For more information, see Van 
                                                                     Buuren paper. References and helpful tutorials are located in the third tab."))))),
                                                       fluidRow(
                                                           column(6, offset=3,
                                                                  img(src='multiple.imputation.png', align = "center", height = "100%", width = "100%")))),
                                              tabPanel("Should I Use It?",
                                                       fluidRow(
                                                           column(12,
                                                                  br(),
                                                                  h4(align = 'center', strong("Should I Use Multiple Imputation?")),
                                                                  br(),
                                                                  p(align = 'center',tags$sup(style="font-size: 16px", "Multiple Imputation, like any missing data 
                                                                  method, has both advantages and disadvantages. It may not always be appropriate to employ multiple 
                                                                  imputation, as shown at the end of this tutorial. Their are many possible scenarios of missing 
                                                                  data in various biomedical study designs, so it is important that you consult the literature, 
                                                                  as well as any direction from this tool, to decide if you would like to implement multiple 
                                                                  imputation. Further information can be found in the references tab.")),
                                                                  br(),
                                                                  br())),
                                                                  wellPanel(style = "background-color: #fff; border-color: #2c3e50 !important; border-width: medium; margin-top: 
                                                                  7px; height: 530px; width: 600;",
                                                                            fluidRow(
                                                                                column(6,
                                                                                       tabsetPanel(type = "tabs",
                                                                                                   tabPanel("Pros",
                                                                                                            br(),
                                                                                                            br(),
                                                                                                            tags$ul(
                                                                                                                tags$li(align = 'justify', tags$sup(style="font-size: 16px","Multiple 
                                                                                                                                                    imputation is a flexible, and easy-to-use 
                                                                                                                                                    missing data tool.")),
                                                                                                                br(),
                                                                                                                tags$li(align = 'justify', tags$sup(style="font-size: 16px","There are many 
                                                                                                                tutorials that clearly explain how to perform multiple imputation, given any 
                                                                                                                data structure, study design, data type, or level of missing data.")),
                                                                                                                br(),
                                                                                                                tags$li(align = 'justify', tags$sup(style="font-size: 16px","If a user has a 
                                                                                                                dataset with multiple variables with missingness, the user can specify a 
                                                                                                                                                    different imputation method for each variable 
                                                                                                                                                    with missingness.")),
                                                                                                                br(),
                                                                                                                tags$li(align = 'justify', tags$sup(style="font-size: 16px","Similarly, the user 
                                                                                                                can specify a different imputation model with as many or as few predictors as needed.")),
                                                                                                                br(),
                                                                                                                tags$li(align = 'justify',tags$sup(style="font-size: 16px", "The imputation model does 
                                                                                                                not need to be the same as the primary analysis model, and many variables can be 
                                                                                                                                                   included in the imputation model at once.")),
                                                                                                                br(),
                                                                                                                tags$li(align = 'justify', tags$sup(style="font-size: 16px", "Auxiliary variables, or 
                                                                                                                those variables that are correlated with the outcome or mechanism of missingness, can 
                                                                                                                provide a reduction in bias if the data are missing not at random.")))),
                                                                                                   
                                                                                                   tabPanel("Cons",
                                                                                                            br(),
                                                                                                            br(),
                                                                                                            tags$ul(
                                                                                                            tags$li(align = 'justify', tags$sup(style="font-size: 16px","Because it uses a stochastic 
                                                                                                            process, the values imputed with be different each time you run an imputation. That means 
                                                                                                            that two different users could run the exact same imputation model on the exact same data 
                                                                                                            and will generate slightly different results from one another.")),
                                                                                                            br(),
                                                                                                            tags$li(align = 'justify', tags$sup(style="font-size: 16px","When data are MCAR or MAR, if 
                                                                                                            there are no useful predictors or auxiliary variables, multiple imputation may not perform 
                                                                                                                                                any better than just ignoring the missingness.")),
                                                                                                            br(),
                                                                                                            tags$li(align='justify', tags$sup(style="font-size: 16px","When the data are MNAR, multiple 
                                                                                                            imputation may generate biased results, especially in the absence of auxiliary variables."
                                                                                                                                              )))))), 
                                                                                column(6,
                                                                                       br(),
                                                                                       br(),
                                                                                       img(src='decision.png', align = "center", height = "100%", width = "100%"))))),
                                              
                                              tabPanel("References and Further Reading", 
                                                       fluidRow(
                                                           column(6, offset = 3,
                                                                  h4(strong("References")),
                                                                  br(),
                                                                  p("1.INSERT REFERENCES HERE!!!!"),
                                                                  p("2. Another Reference"),
                                                                  p("3. Another Reference"),
                                                                  p("4. Another Reference"),
                                                                  br(),
                                                                  br(),
                                                                  h4(strong("Additional Reading")),
                                                                  br(),
                                                                  p("1. Another Reference"),
                                                                  p("2. Another Reference"),
                                                                  p("3. Another Reference"))))))))),
            
            tabItem(tabName = "data",
                    br(),
                    h1(align = 'center', tags$sup(style="font-size: 40px", strong("Enter Your Data"))),
                    wellPanel(style = "background-color: #fff; border-color: #2c3e50 !important; border-width: thick; margin-top: 7px; 
                              height: 730px; width: 600;",
                              fluidRow(
                                  column(10, offset=1,
                                  p(align = 'center', "The tool presented in the next sections allows researchers to run a sensitivity to bias 
                                  analysis using simulated data that is modelled off of their own data. In this section, you can enter the 
                                  descriptive statistics of your data. If you don't have data yet, you can enter any values and see how it 
                                  changes the distributions. The data you input in this section will be used in subsequent sections.", 
                                    style = "font-size: 130%;"))),
                              br(),
                              fluidRow(
                                  column(4,
                                  wellPanel(style = "background-color: #fff; border-color: #2c3e50 !important; border-width: medium; margin-top: 
                                  7px; height: 550px; width: 600;",
                                            numericInput("min_val", "Enter Minimum Value of Variable", 0),
                                            numericInput("max_val", "Enter Maximum Value of Variable", 100),
                                            numericInput("mean_val", "Enter Mean of Variable", 50),
                                            numericInput("sd_val", "Enter Standard Deviation of Variable", 16),
                                            numericInput("skew_val", "Enter Skew of Variable (Possible Values: -1.5 to 1.5)", 0),
                                            numericInput("n_val", "Enter Total Sample Size (Observed + Missing)", 100),
                                            numericInput("miss_val", "Enter Percent Missing (0 to 100%)", 10))),
                                  column(8,
                                         tabsetPanel(type = "tabs",
                                                     tabPanel("Histogram", 
                                                              br(),
                                                              br(),
                                                              plotOutput("histPlot")),
                                                     
                                                     tabPanel("Density Plot",
                                                              br(),
                                                              br(),
                                                              plotOutput("distPlot"))))))),
            
            tabItem(tabName = "exstb",
                    br(),
                    h1(align = 'center', tags$sup(style="font-size: 40px", strong("Example Sensitivity to Bias Analysis"))),
                    wellPanel(style = "background-color: #fff; border-color: #2c3e50 !important; border-width: thick; margin-top: 7px; 
                    height: 830px; width: 600;",
                              fluidRow(
                                  column(10, offset = 1,
                                  p(align = 'center', "In this section, a sensitivity to bias analysis has been performed. The missing 
                                  data were imputed with means from the 10th, 20th, 30th,...90th percentile values of the observed data, 
                                  and the standard deviations were reduced so that all of the imputed data remained in the bounds of the 
                                    original data. Changing the percent missing will change the distributions.", style = "font-size: 130%;"))),
                              br(),
                              
                              tabsetPanel(type = "tabs",
                                          tabPanel("Case Study",
                                          wellPanel(style = "background-color: #fff; border-color: #2c3e50 !important; border-width: thin; margin-top: 7px; 
                                          height: 600px; width: 600;",
                                                    fluidRow(
                                                        column(5,
                                                               br(),
                                                               p(align = 'justify', "It is very possible that those who drink less alcohol on a weekly 
                                                               basis are also those who chose not to respond to this question. However, it is equally 
                                                               possible that those who consume more alcohol on a weekly basis are also those who chose 
                                                               not to respond to this question. As discussed in the 'Case Study' tab, the mean number of 
                                                               ounces consumed for the observed data, or those who chose to respond (75%), was 90oz 
                                                               (standard deviation = 20oz). The researchers decided to employ a MAR-based missing data 
                                                               method to try to compensate for those missing values, and it returned a similar estimate. 
                                                               However, 25% of individuals didn't respond. Can we trust the estimates that assume the 
                                                                 data are Missing at Random?",style = "font-size: 105%;"), 
                                                               br(),
                                                               br(),
                                                               p(align = 'justify', "This section presents an example of a sensitivity to bias analysis on our 
                                                               alcohol consumption data. As discussed in previous sections, there are many ways that you could 
                                                               determine which values to use to center the missing data around. For simplicity, in this example 
                                                               we have chosen to center the missing values around the 10th, 20th, 30th, 40th, 50th, 60th, 70th, 
                                                               80th, and 90th percentiles. It is probably unrealistic that the missing values are centred around 
                                                               the 10th or 90th (or even 20th or 80th) percentiles, but the purpose is to show a sensitivity to 
                                                               bias analysis with a wide range of possible missing data values. You can change the percent missing 
                                                                 to see how  this impacts the distributions.", style = "font-size: 105%;"),
                                                               br()),
                                                        column(7,
                                                               img(src='alc.png', align = "center", height = 430, width = 600),
                                                               br(),
                                                               br(),
                                                               br(),
                                                               p(align='justify', "Remember, we're looking at each of these new distributions as if they represent 
                                                               the real data had everyone responded. The purpose is to determine how extreme the missing values 
                                                               could be (or how extreme the MNAR mechanism could be) for a MAR-based missing data method to still 
                                                                 produce fairly unbiased results.", style = "font-size: 105%;"))))), 
                                          
                                          tabPanel("Missing Data Distributions",
                                                   br(),
                                                   wellPanel(style = "background-color: #fff; border-color: #2c3e50 !important; border-width: thin; margin-top: 7px; 
                                                   height: 550px; width: 600;",
                                                             fluidRow(
                                                                 column(5,
                                                                        br(),
                                                                        br(),
                                                                        numericInput("miss_val2", "Enter Percent Missing (0 to 100%)", 10),
                                                                        br(),
                                                                        p(align='justify', "This first plot shows the distributions of the imputed missing data. 
                                                                        The means of the missing data are centred around the 10th, 20th,...90th percentiles. 
                                                                        The means of each of these percentiles are approximately: 67, 75, 81, 86, 90, 94, 100, 
                                                                          107, and 115 oz. ", style = "font-size: 105%;"),
                                                                        br(),
                                                                        p(align='justify', "You can change the percent missing to see how this impacts the 
                                                                        distributions. The standard deviations were reduced to ensure that the missing data 
                                                                        distributions fit inside of the observed data distributions. The degree to which the 
                                                                        standard deviation was reduced was calculated using the absolute value of the distance 
                                                                        between the given percentile and the observed mean (or, the 50th percentile). For 
                                                                        instance, for the missing data distribution centred around the 40th percentile, the standard 
                                                                        deviation was reduced by 10%, since 50% - 40% = 10%. The missing data distribution centred 
                                                                        around the 60th percentile was similarly reduced by 10%, since the absolute value of 
                                                                          50% - 60% is also 10%.", style = "font-size: 105%;")),
                                                                 column(7,
                                                                        br(),
                                                                        wellPanel(style = "background-color: #fff; border-color: #2c3e50 !important; border-width: 
                                                                        thin; margin-top: 7px; height: 450px; width: 600;",
                                                                                  plotOutput("plotpercentiles")))))),
                                          
                                          tabPanel("New Datasets (Observed + Imputed)",
                                                   br(),
                                                   wellPanel(style = "background-color: #fff; border-color: #2c3e50 !important; border-width: thin; margin-top: 
                                                   7px; height: 550px; width: 600;",
                                                             fluidRow(
                                                                 column(4,
                                                                        br(),
                                                                        p(align='justify', "This second plot shows the distributions of the total data, or the 
                                                                        missing + observed data. The means of the missing data are centred around the 10th, 
                                                                          20th,...90th percentiles.", style = "font-size: 120%;"),
                                                                        br(),
                                                                        p(align = 'justify', "After you have tested a few values for the percent of missing data, 
                                                                        try the following values: 5%, 10%, 20%, and 40%. Look at how it changes the graph in the 
                                                                          following section as well.", style = "font-size: 120%;")),
                                                                 column(8,
                                                                 wellPanel(style = "background-color: #fff; border-color: #2c3e50 !important; border-width: thin; 
                                                                 margin-top: 7px; height: 450px; width: 600;",
                                                                           plotOutput("plotperctot")))))),
                                          
                                          tabPanel("Interpretations",
                                                   br(),
                                                   br(),
                                                   tabsetPanel(type="tabs",
                                                               tabPanel("What Do Our Results Mean?",
                                                                        br(),
                                                                        fluidRow(
                                                                            column(12,
                                                                            p(align = 'justify', "
                                                                            In these examples, we can take the '50th percentile' imputation as a representation 
                                                                            of a MAR-based estimate. The determination of a variable's 'sensitivity' to bias 
                                                                              will be dependent upon the user.", style = "font-size: 105%;"),
                                                                            br(),
                                                                            h4(strong("'Statistical' Significance")),
                                                                            p(align = 'justify', "The user could decide to define sensitivity as any values where 
                                                                            the confidence intervals of the MAR-based estimates do not overlap with the MNAR 
                                                                            confidence interval. If that were the case, for 5% missing, all of the imputed variables 
                                                                            have confidence intervals that overlap with the MAR estimate. For 10% missing, only the 
                                                                            10th percentile variable does not overlap with the MAR estimate. What this means is that, 
                                                                            for these low percentages missing, the data are very robust to bias. The missing values 
                                                                            could be centred around very extreme values and it would not significantly bias the estimation 
                                                                              from a MAR-based method.", style = "font-size: 105%;"),
                                                                            p(align = 'justify', "At 20% missing, the 10th, 20th, 80th, and 90th percentiles do not 
                                                                            overlap with the MAR estimate; while for 40% missing, ", em("only"), " the 40th and 
                                                                            60th percentile variables overlap with the MAR estimate. We can clearly see that, 
                                                                            as missingness increases, the range in which the MAR estimate will not be biased becomes 
                                                                            narrower and narrower. This means that the data become more sensitive, and less robust, 
                                                                            to bias. If 40% of these data were missing, the researchers would need to present these 
                                                                            results with a lot of caution, because if the missing values are centred around anything 
                                                                              more extreme than the 40th or 60th percentiles, the MAR-based method will be very biased.",
                                                                              style = "font-size: 105%;"),
                                                                            br(),
                                                                            h4(strong("'Clinical' Significance")),
                                                                            p(align = 'justify', "Another determination of a variable's 'sensitivity' to bias may be 
                                                                            to look at a more 'real-world' significance. For example, the policy-makers here have 
                                                                            decided that if the 95% confidence interval contains 95oz, they will implement new 
                                                                            alcohol-related policies. The observed data, as well as the MAR-based method (50th 
                                                                            percentile), estimated a mean of 90 oz. The conclusion from this would be that a new 
                                                                            policy should not be implemented. The policy-makers may decide that it is more meaningful 
                                                                            to look at the range of MNAR values where the conclusions change. In other words, at what 
                                                                              point in the missing data assumptions does the confidence interval contain 95oz?", 
                                                                              style = "font-size: 105%;"),
                                                                              p(align = 'justify', "When only 5% or 10% of data are missing, the conclusions remain 
                                                                              stable, regardless of the value that the missing data are centred around. However, 
                                                                              when 20% and 40% of data are missing, if the missing data are centred around the 80th 
                                                                              percentile and higher, the 95% confidence interval will contain 95oz. If the percent 
                                                                              missing were this high, it would be up to the researchers and policy makers to determine 
                                                                              if it is plausible or realistic that the misisng values are centred around 107oz. If 
                                                                              this were plausible, the policy-makers may choose to implement the new policies just 
                                                                                to be safe.", style = "font-size: 105%;")))),
                                                               
                                                               tabPanel("5% Missing",
                                                                        fluidRow(
                                                                            column(7,
                                                                            wellPanel(style = "background-color: #fff; border-color: #2c3e50 !important; border-width: 
                                                                            thin; margin-top: 7px; height: 420px; width: 600;",
                                                                                      br(),
                                                                                      tableOutput("tabletot5"))),
                                                                            column(5, 
                                                                            wellPanel(style = "background-color: #fff; border-color: #2c3e50 !important; border-width: 
                                                                            thin; margin-top: 7px; height: 420px; width: 600;",
                                                                                      img(src='5_10.png', align = "center", height = 370, width = 430)))),
                                                                        fluidRow(
                                                                            column(12,
                                                                            wellPanel(style = "background-color: #fff; border-color: #2c3e50 !important; border-width: 
                                                                            medium; margin-top: 7px; height: 100px; width: 600;",
                                                                            p(align = 'center', "The graph on the right shows the ", strong("clinical significance"), 
                                                                            " of the results when 5% of data are missing. The green shading indicates the range of 
                                                                            values that the missing data could be centred around without our conclusion changing 
                                                                            (>95 oz). We can see that, for 5% missing, none of our imputed values would impact the 
                                                                            conclusions.", style = "font-size: 115%;"))))),
                                                               
                                                               tabPanel("10% Missing",
                                                                        fluidRow(
                                                                            column(7,
                                                                            wellPanel(style = "background-color: #fff; border-color: #2c3e50 !important; border-width: 
                                                                            thin; margin-top: 7px; height: 420px; width: 600;",
                                                                                      tableOutput("tabletot10"))),
                                                                            column(5, 
                                                                            wellPanel(style = "background-color: #fff; border-color: #2c3e50 !important; border-width: 
                                                                            thin; margin-top: 7px; height: 420px; width: 600;",
                                                                                      img(src='5_10.png', align = "center", height = 370, width = 430)))),
                                                                        fluidRow(
                                                                            column(12,
                                                                            wellPanel(style = "background-color: #fff; border-color: #2c3e50 !important; border-width: 
                                                                            medium; margin-top: 7px; height: 100px; width: 600;",
                                                                            p(align = 'center', "The graph on the right shows the ", strong("clinical significance"), " 
                                                                            of the results when 10% of data are missing. The green shading indicates the range of 
                                                                            values that the missing data could be centred around without our conclusion changing 
                                                                              (>95 oz). We can see that, for 10% missing, none of our imputed values would impact 
                                                                              the conclusions.", style = "font-size: 115%;"))))),
                                                               tabPanel("20% Missing",
                                                                        fluidRow(
                                                                            column(7,
                                                                            wellPanel(style = "background-color: #fff; border-color: #2c3e50 !important; border-width: 
                                                                            thin; margin-top: 7px; height: 420px; width: 600;",
                                                                                      tableOutput("tabletot20"))),
                                                                            column(5, 
                                                                            wellPanel(style = "background-color: #fff; border-color: #2c3e50 !important; border-width: 
                                                                            thin; margin-top: 7px; height: 420px; width: 600;",
                                                                                      img(src='20.png', align = "center", height = 370, width = 430)))),
                                                                        fluidRow(
                                                                            column(12,
                                                                            wellPanel(style = "background-color: #fff; border-color: #2c3e50 !important; border-width: 
                                                                                      medium; margin-top: 7px; height: 100px; width: 600;",
                                                                                      p(align = 'center', "The graph on the right shows the ", strong("clinical 
                                                                                      significance"), " of the results when 20% of data are missing. The green shading 
                                                                                      indicates the range of values that the missing data could be centred around 
                                                                                      without our conclusion changing (>95 oz). We can see that, for 20% missing, 
                                                                                      if the missing values were centred around the 80th percentile or higher, 
                                                                                      our conclusions would change and new alcohol policies should be introduced.", 
                                                                                        style = "font-size: 115%;"))))),
                                                               
                                                               tabPanel("40% Missing",
                                                                        fluidRow(
                                                                            column(7,
                                                                            wellPanel(style = "background-color: #fff; border-color: #2c3e50 !important; border-width: 
                                                                            thin; margin-top: 7px; height: 420px; width: 600;",
                                                                                      tableOutput("tabletot40"))),
                                                                            column(5, 
                                                                            wellPanel(style = "background-color: #fff; border-color: #2c3e50 !important; border-width: 
                                                                            thin; margin-top: 7px; height: 420px; width: 600;",
                                                                                      img(src='40.png', align = "center", height = 370, width = 430)))),
                                                                        fluidRow(
                                                                            column(12,
                                                                            wellPanel(style = "background-color: #fff; border-color: #2c3e50 !important; border-width:
                                                                                      medium; margin-top: 7px; height: 100px; width: 600;",
                                                                                      p(align = 'center', "The graph on the right shows the ", strong("clinical significance"), 
                                                                                      " of the results when 40% of data are missing. The green shading indicates the range of 
                                                                                      values that the missing data could be centred around without our conclusion changing 
                                                                                      (>95 oz). We can see that, for 40% missing, if the missing values were centred around 
                                                                                      the 70th percentile or higher, our conclusions would change and new alcohol policies 
                                                                                      should be introduced.", style = "font-size: 115%;")))))
                               ))))),
            
            tabItem(tabName = "custstb",
                    h1(align = 'center', tags$sup(style="font-size: 40px", strong("Run Your Own Sensitivity to Bias Analysis"))),
                    br(),
                    wellPanel(style = "background-color: #fff; border-color: #2c3e50 !important; border-width: 
                    thick; margin-top: 7px; height: 1100px; width: 600;",
                              fluidRow(column(10, offset=1,
                              p(align = 'center', "Now that you have seen an example, it is time for you to perform your own sensitivity to bias 
                              analysis on your own data! In this section, you can enter up to 10 means for the missing data distributions. The 
                                standard deviations will be calculated as described in the previous section. The values chosen for the missing 
                                data means should ideally be based on real, plausible values for the missing data.", style = "font-size: 130%;"))),
                              br(),
                              fluidRow(
                                  column(4,
                                  wellPanel(style = "background-color: #fff; border-color: #2c3e50 !important; border-width: medium; margin-top: 
                                  7px; height: 900px; width: 600;",
                                            numericInput("miss_val3", "Enter Percent Missing (0 to 100%)", 10),
                                            br(),
                                            br(),
                                            numericInput("MD1", "Missing Data Mean 1", 30),
                                            numericInput("MD2", "Missing Data Mean 2", 50),
                                            numericInput("MD3", "Missing Data Mean 3", 70),
                                            numericInput("MD4", "Missing Data Mean 4", NA),
                                            numericInput("MD5", "Missing Data Mean 5", NA),
                                            numericInput("MD6", "Missing Data Mean 6", NA),
                                            numericInput("MD7", "Missing Data Mean 7", NA),
                                            numericInput("MD8", "Missing Data Mean 8", NA),
                                            numericInput("MD9", "Missing Data Mean 9", NA),
                                            numericInput("MD10", "Missing Data Mean 10", NA))),
                                  column(8,
                                         tabsetPanel( type = "tabs",
                                                      tabPanel("Graphs",
                                                               br(),
                                                               plotOutput("plotpercentilesCus"),
                                                               br(),
                                                               br(),
                                                               plotOutput("plotperctotCust")),
                                                      
                                                      tabPanel("Table",
                                                               fluidRow(
                                                                   column(12,
                                                                          h4(strong("95% Confidence Interval of the Mean for Each New Full Distribution")),
                                                                          br(),
                                                                          tableOutput("sum"))))))))),
            
            tabItem(tabName = "custmi",
                    br(),
                    h1(align = 'center', tags$sup(style="font-size: 40px", strong("Testing the Performance of Multiple Imputation"))),
                    wellPanel(style = "background-color: #fff; border-color: #2c3e50 !important; border-width: thick; margin-top: 7px; 
                              height: 1100px; width: 600;",
                              fluidRow(
                                  column(12,
                                  p(align = 'center', "In this section, you will investigate two things: 1. How missing data can bias the estimations from both 
                                    ignoring the missing data and employing multiple imputation; and 2. How an auxiliary variable included in the imputation 
                                    model can reduce that bias. As a reminder, an auxiliary variable is a variable from the dataset that is correlated with 
                                    either the variable of interest or the mechanism of missingness. If your missing data are in fact missing not at random, 
                                    an auxiliary variable can reduce the bias in the multiple imputation methods. As the strength of the auxiliary variable 
                                    increases, the reduction in bias increases.", style = "font-size: 115%;"),
                                  br(),
                                  p(align = 'center', "Below, you can enter the percent missing, the mean of the missing data (try out some values that you 
                                     think will bias the data!), the mean of the auxiliary variable, and the strength of the correlation between the auxiliary variable 
                                     and your variable of interest. Play around with the inputs to see how the bias is impacted in the two multiple imputation 
                                     estimates, as well as the estimation from just the observed data. The 'Full Data' is the true estimate of the mean of your 
                                     variable (observed + missing data centred around the mean that you input), had all the participants answered the 
                                    question.", style = "font-size: 115%;")),
                                  br()),
                              br(),
                              fluidRow(
                                  column(4,
                                  wellPanel(style = "background-color: #fff; border-color: #2c3e50 !important; border-width: medium; margin-top: 7px; 
                                        height: 450px; width: 600;",
                                        numericInput("miss_val4", "Enter Percent Missing (0 to 100%)", 25),
                                        numericInput("meanMD_val", "Enter Mean of the Missing Data", value = 50),
                                        numericInput("aux_val", "Enter Mean of Auxiliary Variable, If Collected One (Default is Mean of 0, 
                                                     Standard Deviation of 1)", 0),
                                        numericInput("aux_sd", "Enter Standard Deviation of Auxiliary Variable", 1),
                                        sliderInput("slider_aux", "Choose Strength of Correlation Between Auxiliary Variable and Main Variable", 
                                                    min = -0.9, max = 0.9,  value = 0.8, step=0.1))),
                                  column(8,
                                  wellPanel(style = "background-color: #fff; border-color: #2c3e50 !important; border-width: medium; margin-top: 7px; 
                                        height: 450px; width: 600;",
                                        br(),
                                        plotOutput("miplot")))),
                              fluidRow(
                                  column(12,
                                         br(),
                                         p(align = 'center', "If you tested out a fairly MNAR value for the missing data to be centred around, you will have seen 
                                         how the estimations from both ignoring the missingness and employing multiple imputation became very biased. Adding the 
                                         auxiliary variable can help, but only if the correlation is above 0. When the correlation between the auxiliary variable 
                                           and the main variable is low, the benefit from the auxiliary variable is small.", style = "font-size: 115%;"),
                                         br(),
                                         p(align = 'center', "Another conclusion from these simulations is that, even with a correlation of 0.9 (or -0.9), if the data 
                                         are MNAR, the auxiliary variable will never be able to fully recover the missing information. The multiple imputation 
                                         estimates will always be biased when the missing data are strongly missing not at random, even with a strong auxiliary 
                                         variable. If you did not see this result, move the slider to 0.9 and enter an extreme value for the mean of the missing 
                                           data.", style = "font-size: 115%;"),
                                         br(),
                                         p(align = 'center', "The results of these simulations should give you an idea into how an auxiliary variable may reduce 
                                         the bias from the MAR-based estimations of multiple imputation or ignoring the missingness. If you notice, the multiple 
                                         imputation and ignoring the missing data will always produce an estimate that is very close to the mean of the observed 
                                         data. Again, this is because these two methods assume the data are Missing At Random. However, the auxiliary variable 
                                         has the ability to help the imputation model to better predict those missing values.", style = "font-size: 115%;"))))),
            
            tabItem(tabName="final",
                    br(),
                    h1(align = 'center', tags$sup(style="font-size: 40px", strong("Final Remarks"))),
                    wellPanel(style = "background-color: #fff; border-color: #2c3e50 !important; border-width: thick; margin-top: 7px; 
                              height: 670px; width: 600;",
                              tabsetPanel(type="tabs",
                                          tabPanel("Conclusions",
                                                   fluidRow(
                                                       column(6,
                                                              br(),
                                                              p(align = 'justify', "In this tutorial, you learned the three patterns in which missing data can arise, a 
                                                              method to compensate for missing data (multiple imputation), and how to run and interpret a sensitivity to 
                                                              bias analysis. You learned that missing data can bias the analysis of just the observed data, and it can 
                                                              bias the estimations from multiple imputation. Auxiliary variables can be very useful to reduce this bias, 
                                                              so researchers should make every effort to collect a variable that they think may be highly correlated to the 
                                                                variable of interest.", style = "font-size: 115%;"),
                                                              br(),
                                                              p(align = 'justify', "A sensitivity to bias analysis is an investigative tool to see how missing data may 
                                                              impact your results. It is a tool designed to either give researchers confidence that the results they see are real, 
                                                              or to make them wary that the results they see may be due to bias. The next step in this process would be to perform 
                                                              a ", strong("sensitivity analysis"), ". For this method, users could use the results from this sensitivity to bias 
                                                              analysis to select a few realistic values for the missing data to be centred around. A good sensitivity analysis 
                                                              will pool the results of the analyses that assume different missing data distributions, and will generate a bias-adjusted 
                                                                estimate. These results should then be presented as supplementary analyses in a paper or other research outputs.", 
                                                                style = "font-size: 115%;"),
                                                              br(),
                                                              p(align = 'justify', "For more information on missing data, missing data methods, and sensitivity analyses, please 
                                                              see the references in the next tab. Every researcher should be educating themselves on the issues that arise from 
                                                              missing data, the methods to handle missing data, and ways to prevent it from happening. This will help to reduce 
                                                                the potential bias from missing data so that we can be more confident basing decisions off of our results.", 
                                                                style = "font-size: 115%;")),
                                                       column(6,
                                                              br(),
                                                              br(),
                                                              wellPanel(style = "background-color: #fff; border-color: #2c3e50 !important; border-width: medium; margin-top: 5px;
                                                                    height: 500px; width: 600;",
                                                                    img(src='front.page.png', align = "center", height = 450, width = 510))))),
                                          
                                          tabPanel("References")))))))
    
    





server <- function(input, output) {

    
    output$distPlot <- renderPlot({
        set.seed(123456)
        x = ifelse(input$skew_val < -1, 4,
                   ifelse(input$skew_val <.5, 1,
                          ifelse(input$skew_val <0, .5,
                                 ifelse(input$skew_val==0, 0,
                                        ifelse(input$skew_val < 0.51, .5,
                                               ifelse(input$skew_val <1.1, 1,
                                                      ifelse(input$skew_val<1.6, 4, 0)))))))
        
        observed = input$n_val-(input$n_val*(input$miss_val/100))
        
        cpST <- c(input$mean_val, input$sd_val, input$skew_val, x)
        dpST <- cp2dp(cpST, family="ST")
        sims <- rst(observed, dp = dpST)
        sims=as.data.frame(sims)
        

        z = ggplot(sims, aes(sims)) + geom_density(aes(y = ..count../sum(..count..)), adjust=2, color='dodgerblue', size=2) +
            labs(title="Density Plot of Observed Data",y="Frequency", x = "Variable of Interest")+ xlim(c(input$min_val, input$max_val))

        z = z + theme(panel.background = element_rect(fill = "white",
                                            colour = "white", size = .5),
            panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                            colour = "lightgrey"), 
            panel.grid.minor = element_line(size = 0.25, linetype = 'solid',
                                            colour = "lightgrey"))
        z = z + theme(plot.title = element_text(size = 20, face = "bold", hjust=0.5),
                      legend.title=element_text(size=11, face="bold"), 
                      legend.text=element_text(size=9),
                      axis.title.x=element_text(size=12, face="bold"),  # X axis title
                      axis.title.y=element_text(size=12, face="bold"),  # Y axis title
                      axis.text.x=element_text(size=10),  # X axis text
                      axis.text.y=element_text(size=10))
        
        z = z + theme(
            axis.ticks.y = element_blank(),
            axis.text.y = element_blank())
        
        z
    })
    
    
    
    output$histPlot <- renderPlot({
        set.seed(123456)
        x = ifelse(input$skew_val < -1, 4,
                   ifelse(input$skew_val <.5, 1,
                          ifelse(input$skew_val <0, .5,
                                 ifelse(input$skew_val==0, 0,
                                        ifelse(input$skew_val < 0.51, .5,
                                               ifelse(input$skew_val <1.1, 1,
                                                      ifelse(input$skew_val<1.6, 4, 0)))))))
        
        cpST <- c(input$mean_val, input$sd_val, input$skew_val, x)
        dpST <- cp2dp(cpST, family="ST")
        sims <- rst(input$n_val, dp = dpST)
        Outcome = sims
        
        par(cex=1.4, cex.axis = 1.3)
       
        hist(Outcome, col = "lightskyblue", main = "Histogram of Outcome Variable", ylab="Frequency", 
             xlab = "Outcome Variable")
    })
    
    
    
    output$plotpercentiles <- renderPlot({
        
        set.seed(123456)
        cpST <- c(90, 20, 0 , 0)
        dpST <- cp2dp(cpST, family="ST")
        sims <- rst(1000*(1-(input$miss_val2/100)), dp = dpST)

 
        xx = quantile(sims, probs=c(.1, .2, .3, .4, .5, .6, .7, .8, .9))
        xx = as.data.frame(xx)
        y = c(.6, .7, .8, .9, 1, .9, .8, .7, .6)
        nmiss = reactive({1000*(input$miss_val2/100)})
        
        sims2 = matrix(NA, nrow = nmiss(), ncol=9)
        
        for(i in 1:9){
            cpST <- c(xx[i,1], (20*y[i]), 0, 0)
            dpST <- cp2dp(cpST, family="ST")
            sims2[c(1:nmiss()),i] <- rst(nmiss(), dp = dpST)}
        
        
        sims2 = as.data.frame(sims2)
        sims=as.data.frame(sims)
        
        sims1 = data.frame("Ounces.Alcohol" = c(sims2$V1,sims2$V2, sims2$V3,sims2$V4, sims2$V5, sims2$V6, sims2$V7, sims2$V8, sims2$V9, sims$sims),
                           "Missing Data Distributions" = c(rep("10th percentile", times=length(sims2$V1)),
                                                            rep("20th percentile", times=length(sims2$V1)), 
                                                            rep("30th percentile", times=length(sims2$V1)),
                                                            rep("40th percentile", times=length(sims2$V1)), 
                                                            rep("50th percentile", times=length(sims2$V1)),
                                                            rep("60th percentile", times=length(sims2$V1)), 
                                                            rep("70th percentile", times=length(sims2$V1)),
                                                            rep("80th percentile", times=length(sims2$V1)), 
                                                            rep("90th percentile", times=length(sims2$V1)),
                                                            rep("Observed Data", times=length(sims$sims))))
        
        sims1$Missing.Data.Distributions = as.factor(sims1$Missing.Data.Distributions)
        
        z = ggplot(sims1, aes(Ounces.Alcohol, colour=Missing.Data.Distributions, group=Missing.Data.Distributions)) + geom_density(aes(y = ..count../sum(..count..)), 
                                                                                                                                   adjust=2, size = 1) +
            labs(title="Distributions of Missing Data",y="Frequency", x = "Ounces of Alcohol Consumed")+ xlim(c(0, 160))
        z = z + theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
                      legend.title=element_text(size=14, face="bold"), 
                      legend.text=element_text(size=12),
                      axis.title.x=element_text(size=14, face="bold"),  # X axis title
                      axis.title.y=element_text(size=14, face="bold"),  # Y axis title
                      axis.text.x=element_text(size=12),  # X axis text
                      axis.ticks.y = element_blank(),
                      axis.text.y = element_blank())
        
        z = z + scale_colour_discrete("Missing Data 
Distributions")
        
        z
        
    })
   
    
    
    output$plotperctot <- renderPlot({
        set.seed(123456)
        cpST <- c(90, 20, 0 , 0)
        dpST <- cp2dp(cpST, family="ST")
        sims <- rst(1000*(1-(input$miss_val2/100)), dp = dpST)
        
        xx = quantile(sims, probs=c(.1, .2, .3, .4, .5, .6, .7, .8, .9))
        xx = as.data.frame(xx)
        y = c(.6, .7, .8, .9, 1, .9, .8, .7, .6)
        nmiss = reactive({1000*(input$miss_val2/100)})
        
        sims2 = matrix(NA, nrow = nmiss(), ncol=9)
        
        for(i in 1:9){
            cpST <- c(xx[i,1], (20*y[i]), 0, 0)
            dpST <- cp2dp(cpST, family="ST")
            sims2[c(1:nmiss()),i] <- rst(nmiss(), dp = dpST)}
        
        
        sims2 = as.data.frame(sims2)
        sims=as.data.frame(sims)
        
        sims2[c((nmiss()+1):(nmiss()+length(sims$sims))),1] = sims$sims
        sims2[c((nmiss()+1):(nmiss()+length(sims$sims))),2] = sims$sims
        sims2[c((nmiss()+1):(nmiss()+length(sims$sims))),3] = sims$sims
        sims2[c((nmiss()+1):(nmiss()+length(sims$sims))),4] = sims$sims
        sims2[c((nmiss()+1):(nmiss()+length(sims$sims))),5] = sims$sims
        sims2[c((nmiss()+1):(nmiss()+length(sims$sims))),6] = sims$sims
        sims2[c((nmiss()+1):(nmiss()+length(sims$sims))),7] = sims$sims
        sims2[c((nmiss()+1):(nmiss()+length(sims$sims))),8] = sims$sims
        sims2[c((nmiss()+1):(nmiss()+length(sims$sims))),9] = sims$sims
        
        
        sims1 = data.frame("Ounces.Alcohol" = c(sims2$V1,sims2$V2, sims2$V3,sims2$V4, sims2$V5, sims2$V6, sims2$V7, sims2$V8, sims2$V9),
                           "Total Distributions" = c(rep("10th percentile", times=length(sims2$V1)),
                                                            rep("20th percentile", times=length(sims2$V1)), 
                                                            rep("30th percentile", times=length(sims2$V1)),
                                                            rep("40th percentile", times=length(sims2$V1)), 
                                                            rep("50th percentile", times=length(sims2$V1)),
                                                            rep("60th percentile", times=length(sims2$V1)), 
                                                            rep("70th percentile", times=length(sims2$V1)),
                                                            rep("80th percentile", times=length(sims2$V1)), 
                                                            rep("90th percentile", times=length(sims2$V1))))
        
        sims1$Total.Distributions = as.factor(sims1$Total.Distributions)
        
        z = ggplot(sims1, aes(Ounces.Alcohol, colour=Total.Distributions, group=Total.Distributions)) + geom_density(aes(y = ..count../sum(..count..)), 
                                                                                                                     adjust=2, size = .6) +
            labs(title="Distributions of Total (Observed + Imputed) Data",y="Frequency", x = "Ounces of Alcohol Consumed")+ xlim(c(0, 160))
        z = z + theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
                      legend.title=element_text(size=14, face="bold"), 
                      legend.text=element_text(size=12),
                      axis.title.x=element_text(size=14, face="bold"),  # X axis title
                      axis.title.y=element_text(size=14, face="bold"),  # Y axis title
                      axis.text.x=element_text(size=12),  # X axis text
                      axis.ticks.y = element_blank(),
                      axis.text.y = element_blank())
        
        z = z + scale_colour_discrete("Total Distributions")
        
        z
        
    }) 
    
    
    
    output$plotpercentilesCus <- renderPlot({
        set.seed(123456)
        
        x = ifelse(input$skew_val < -1, 4,
                   ifelse(input$skew_val < -0.5, 1,
                          ifelse(input$skew_val < 0, .5,
                                 ifelse(input$skew_val==0, 0,
                                        ifelse(input$skew_val < 0.51, .5,
                                               ifelse(input$skew_val <1.1, 1,
                                                      ifelse(input$skew_val<1.6, 4, 0)))))))
        
        
        cpST <- c(input$mean_val, input$sd_val, input$skew_val, x)
        dpST <- cp2dp(cpST, family="ST")
        sims <- rst((input$n_val-(input$n_val*(input$miss_val3/100))), dp = dpST)
        
        Fn <- ecdf(sims)
        
        xx = Fn(c(input$MD1, input$MD2, input$MD3, input$MD4, input$MD5, input$MD6, input$MD7, 
                  input$MD8, input$MD9, input$MD10))
        nEnter = length(xx)
        
        xx = ifelse(is.na(xx), 999999, xx)
        
        values_cust = c(input$MD1, input$MD2, input$MD3, input$MD4, input$MD5, input$MD6, input$MD7, 
                        input$MD8, input$MD9, input$MD10)
       
        values_cust = ifelse(is.na(values_cust), 999999, values_cust)

        y = NULL
        
        for(i in 1:nEnter){
            y[i] = ifelse(xx[i] < 0.5, (1-(.5-xx[i])), (1-(xx[i]-.5)))}

        nmiss = reactive({input$n_val*(input$miss_val3/100)})

        
        sims2 = matrix(NA, nrow = nmiss(), ncol=nEnter+1)
        
        for(i in 1:nEnter){
            newvalue = ifelse(values_cust[i]==999999, 0, values_cust[i])
            newy = ifelse(values_cust[i]==999999, 1, y[i])
            cpST <- c(newvalue, (input$sd_val*newy), input$skew_val, x)
            dpST <- cp2dp(cpST, family="ST")
            sims2[c(1:nmiss()),11] = rep(values_cust[i], nmiss())
            sims2[c(1:nmiss()),i] <- rst(nmiss(), dp = dpST)
            sims2[c(1:nmiss()),i] <- ifelse(sims2[c(1:nmiss()),11]==999999, NA, sims2[c(1:nmiss()),i]) }


        sims2 = as.data.frame(sims2)
        sims=as.data.frame(sims)
        
        sims1 = data.frame("Outcome" = c(sims2$V1,sims2$V2, sims2$V3,sims2$V4, sims2$V5, sims2$V6, sims2$V7, sims2$V8, sims2$V9, sims2$V10, sims$sims),
                           "Missing Data Distributions" = c(rep("Missing Data Mean 1", times=length(sims2$V1)),
                                                            rep("Missing Data Mean 2", times=length(sims2$V1)), 
                                                            rep("Missing Data Mean 3", times=length(sims2$V1)),
                                                            rep("Missing Data Mean 4", times=length(sims2$V1)), 
                                                            rep("Missing Data Mean 5", times=length(sims2$V1)),
                                                            rep("Missing Data Mean 6", times=length(sims2$V1)), 
                                                            rep("Missing Data Mean 7", times=length(sims2$V1)),
                                                            rep("Missing Data Mean 8", times=length(sims2$V1)), 
                                                            rep("Missing Data Mean 9", times=length(sims2$V1)),
                                                            rep("Missing Data Mean 10", times=length(sims2$V1)),
                                                            rep("Observed Data", times = length(sims$sims))))
        
        sims1$Missing.Data.Distributions = as.factor(sims1$Missing.Data.Distributions)
        sims1 = na.omit(sims1)
        sims1$Missing.Data.Distributions = as.factor(sims1$Missing.Data.Distributions)
        
        z = ggplot(sims1, aes(Outcome, colour=Missing.Data.Distributions, group=Missing.Data.Distributions)) + geom_density(aes(y = ..count../sum(..count..)), 
                                                                                                                            adjust=2, size = 1) +
            labs(title="Distributions of Missing Data",y="Frequency", x = "Outcome")+ xlim(c(input$min_val, input$max_val))
        z = z + theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
                      legend.title=element_text(size=14, face="bold"), 
                      legend.text=element_text(size=12),
                      axis.title.x=element_text(size=14, face="bold"),  # X axis title
                      axis.title.y=element_text(size=14, face="bold"),  # Y axis title
                      axis.text.x=element_text(size=12),  # X axis text
                      axis.ticks.y = element_blank(),
                      axis.text.y = element_blank())
        
        z = z + scale_colour_discrete("Missing Data Distributions")
        
        z
        
    })
    
    
    
    output$plotperctotCust <- renderPlot({
        
        set.seed(123456)
        
        x = ifelse(input$skew_val < -1, 4,
                   ifelse(input$skew_val < -0.5, 1,
                          ifelse(input$skew_val < 0, .5,
                                 ifelse(input$skew_val==0, 0,
                                        ifelse(input$skew_val < 0.51, .5,
                                               ifelse(input$skew_val <1.1, 1,
                                                      ifelse(input$skew_val<1.6, 4, 0)))))))
        
        
        cpST <- c(input$mean_val, input$sd_val, input$skew_val, x)
        dpST <- cp2dp(cpST, family="ST")
        sims <- rst((input$n_val-(input$n_val*(input$miss_val3/100))), dp = dpST)
        
        Fn <- ecdf(sims)
        
        xx = Fn(c(input$MD1, input$MD2, input$MD3, input$MD4, input$MD5, input$MD6, input$MD7, 
                  input$MD8, input$MD9, input$MD10))
        nEnter = length(xx)
        
        xx = ifelse(is.na(xx), 999999, xx)
        
        values_cust = c(input$MD1, input$MD2, input$MD3, input$MD4, input$MD5, input$MD6, input$MD7, 
                        input$MD8, input$MD9, input$MD10)
        
        values_cust = ifelse(is.na(values_cust), 999999, values_cust)
        
        y = NULL
        
        for(i in 1:nEnter){
            y[i] = ifelse(xx[i] < 0.5, (1-(.5-xx[i])), (1-(xx[i]-.5)))}
        
        nmiss = reactive({input$n_val*(input$miss_val3/100)})
        
        
        sims2 = matrix(NA, nrow = nmiss(), ncol=nEnter+1)
        
        for(i in 1:nEnter){
            newvalue = ifelse(values_cust[i]==999999, 0, values_cust[i])
            newy = ifelse(values_cust[i]==999999, 1, y[i])
            cpST <- c(newvalue, (input$sd_val*newy), input$skew_val, x)
            dpST <- cp2dp(cpST, family="ST")
            sims2[c(1:nmiss()),11] = rep(values_cust[i], nmiss())
            sims2[c(1:nmiss()),i] <- rst(nmiss(), dp = dpST)
            sims2[c(1:nmiss()),i] <- ifelse(sims2[c(1:nmiss()),11]==999999, NA, sims2[c(1:nmiss()),i]) }
        
        
        sims2 = as.data.frame(sims2)
        sims=as.data.frame(sims)
        
        sims2[c((nmiss()+1):(nmiss()+length(sims$sims))),1] = sims$sims
        sims2[c((nmiss()+1):(nmiss()+length(sims$sims))),2] = sims$sims
        sims2[c((nmiss()+1):(nmiss()+length(sims$sims))),3] = sims$sims
        sims2[c((nmiss()+1):(nmiss()+length(sims$sims))),4] = sims$sims
        sims2[c((nmiss()+1):(nmiss()+length(sims$sims))),5] = sims$sims
        sims2[c((nmiss()+1):(nmiss()+length(sims$sims))),6] = sims$sims
        sims2[c((nmiss()+1):(nmiss()+length(sims$sims))),7] = sims$sims
        sims2[c((nmiss()+1):(nmiss()+length(sims$sims))),8] = sims$sims
        sims2[c((nmiss()+1):(nmiss()+length(sims$sims))),9] = sims$sims
        sims2[c((nmiss()+1):(nmiss()+length(sims$sims))),10] = sims$sims
        
        
        for(i in 1:10){
            sims2[,12] = ifelse(is.na(sims2[1,i]),1, 2)
            sims2[,i] = ifelse(sims2[,12]==1, NA, sims2[,i])
        }
        
        sims2 = as.data.frame(sims2)
        sims=as.data.frame(sims)
        
        sims1 = data.frame("Outcome" = c(sims2$V1,sims2$V2, sims2$V3,sims2$V4, sims2$V5, sims2$V6, sims2$V7, sims2$V8, sims2$V9, sims2$V10, sims$sims),
                           "Total Distributions" = c(rep("Missing Data Mean 1 + Observed Data", times=length(sims2$V1)),
                                                     rep("Missing Data Mean 2 + Observed Data", times=length(sims2$V1)), 
                                                     rep("Missing Data Mean 3 + Observed Data", times=length(sims2$V1)),
                                                     rep("Missing Data Mean 4 + Observed Data", times=length(sims2$V1)), 
                                                     rep("Missing Data Mean 5 + Observed Data", times=length(sims2$V1)),
                                                     rep("Missing Data Mean 6 + Observed Data", times=length(sims2$V1)), 
                                                     rep("Missing Data Mean 7 + Observed Data", times=length(sims2$V1)),
                                                     rep("Missing Data Mean 8 + Observed Data", times=length(sims2$V1)), 
                                                     rep("Missing Data Mean 9 + Observed Data", times=length(sims2$V1)),
                                                     rep("Missing Data Mean 10 + Observed Data", times=length(sims2$V1)),
                                                     rep("Just Observed Data", times=length(sims$sims))))

        sims1$Total.Distributions = as.factor(sims1$Total.Distributions)
        sims1 = na.omit(sims1)
        
        
        
        z = ggplot(sims1, aes(Outcome, colour=Total.Distributions, group=Total.Distributions)) + geom_density(aes(y = ..count../sum(..count..)), adjust=2, size = 1) +
            labs(title="Distributions of Total (Observed + Imputed) Data",y="Frequency", x = "Variable of Interest")+ xlim(c(input$min_val, input$max_val))
        z = z + theme(plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
                      legend.title=element_text(size=14, face="bold"), 
                      legend.text=element_text(size=12),
                      axis.title.x=element_text(size=14, face="bold"),  # X axis title
                      axis.title.y=element_text(size=14, face="bold"),  # Y axis title
                      axis.text.x=element_text(size=12),  # X axis text
                      axis.ticks.y = element_blank(),
                      axis.text.y = element_blank())
        z = z + scale_colour_discrete("Total Distributions")
        
        z
        
    }) 
    

    output$sum <- renderTable({
        set.seed(123456)
        
        x = ifelse(input$skew_val < -1, 4,
                   ifelse(input$skew_val < -0.5, 1,
                          ifelse(input$skew_val < 0, .5,
                                 ifelse(input$skew_val==0, 0,
                                        ifelse(input$skew_val < 0.51, .5,
                                               ifelse(input$skew_val <1.1, 1,
                                                      ifelse(input$skew_val<1.6, 4, 0)))))))
        
        
        cpST <- c(input$mean_val, input$sd_val, input$skew_val, x)
        dpST <- cp2dp(cpST, family="ST")
        sims <- rst((input$n_val-(input$n_val*(input$miss_val3/100))), dp = dpST)
        
        Fn <- ecdf(sims)
        
        xx = Fn(c(input$MD1, input$MD2, input$MD3, input$MD4, input$MD5, input$MD6, input$MD7, 
                  input$MD8, input$MD9, input$MD10))
        nEnter = length(xx)
        
        xx = ifelse(is.na(xx), 999999, xx)
        
        values_cust = c(input$MD1, input$MD2, input$MD3, input$MD4, input$MD5, input$MD6, input$MD7, 
                        input$MD8, input$MD9, input$MD10)
        
        values_cust = ifelse(is.na(values_cust), 999999, values_cust)
        
        y = NULL
        
        for(i in 1:nEnter){
            y[i] = ifelse(xx[i] < 0.5, (1-(.5-xx[i])), (1-(xx[i]-.5)))}
        
        nmiss = reactive({input$n_val*(input$miss_val3/100)})
        
        
        sims2 = matrix(NA, nrow = nmiss(), ncol=nEnter+1)
        
        for(i in 1:nEnter){
            newvalue = ifelse(values_cust[i]==999999, 0, values_cust[i])
            newy = ifelse(values_cust[i]==999999, 1, y[i])
            cpST <- c(newvalue, (input$sd_val*newy), input$skew_val, x)
            dpST <- cp2dp(cpST, family="ST")
            sims2[c(1:nmiss()),11] = rep(values_cust[i], nmiss())
            sims2[c(1:nmiss()),i] <- rst(nmiss(), dp = dpST)
            sims2[c(1:nmiss()),i] <- ifelse(sims2[c(1:nmiss()),11]==999999, NA, sims2[c(1:nmiss()),i]) }
        
        
        sims2 = as.data.frame(sims2)
        sims=as.data.frame(sims)
        
        sims2[c((nmiss()+1):(nmiss()+length(sims$sims))),1] = sims$sims
        sims2[c((nmiss()+1):(nmiss()+length(sims$sims))),2] = sims$sims
        sims2[c((nmiss()+1):(nmiss()+length(sims$sims))),3] = sims$sims
        sims2[c((nmiss()+1):(nmiss()+length(sims$sims))),4] = sims$sims
        sims2[c((nmiss()+1):(nmiss()+length(sims$sims))),5] = sims$sims
        sims2[c((nmiss()+1):(nmiss()+length(sims$sims))),6] = sims$sims
        sims2[c((nmiss()+1):(nmiss()+length(sims$sims))),7] = sims$sims
        sims2[c((nmiss()+1):(nmiss()+length(sims$sims))),8] = sims$sims
        sims2[c((nmiss()+1):(nmiss()+length(sims$sims))),9] = sims$sims
        sims2[c((nmiss()+1):(nmiss()+length(sims$sims))),10] = sims$sims
        
        colnames(sims2) = c("Missing Data Mean 1 + Observed Data", "Missing Data Mean 2 + Observed Data","Missing Data Mean 3 + Observed Data",
                            "Missing Data Mean 4 + Observed Data","Missing Data Mean 5 + Observed Data","Missing Data Mean 6 + Observed Data",
                            "Missing Data Mean 7 + Observed Data","Missing Data Mean 8 + Observed Data","Missing Data Mean 9 + Observed Data",
                            "Missing Data Mean 10 + Observed Data")

        sims3 = matrix(NA, nrow = 10, ncol=3)
        
        for(i in 1:10){
            dd = CI(sims2[,i], ci=0.95)
            sims3[i, 1:3] = dd[c(3,2,1)]}
        
        colnames(sims3) = c("Lower Bound", "Mean", "Upper Bound")
        rownames(sims3) = colnames(sims2)[1:10]
        sims3 = as.data.frame(sims3)
        dd1 = CI(sims$sims, ci=0.95)
        sims3[11, 1:3] = dd1[c(3,2,1)]
        rownames(sims3)[11] = "Just Observed Data"
        sims3[1:11,4] = rownames(sims3)[1:11]
        sims3 = sims3[1:11, c(4,1,2,3)]
        sims3 = na.omit(sims3)
        sims3 = as.data.frame(sims3)
        colnames(sims3)[1] = " "
        
        sims3
        
    })
    
    
    
    
    output$miplot <- renderPlot({
        
        set.seed(123456)
        x = ifelse(input$skew_val < -1, 4,
                   ifelse(input$skew_val < -.5, 1,
                          ifelse(input$skew_val < 0, .5,
                                 ifelse(input$skew_val==0, 0,
                                        ifelse(input$skew_val < 0.51, .5,
                                               ifelse(input$skew_val <1.1, 1,
                                                      ifelse(input$skew_val<1.6, 4, 0)))))))
        
        
        observed = reactive({input$n_val - (input$n_val*(input$miss_val4/100))})
        
        cpST <- c(input$mean_val, input$sd_val, input$skew_val, x)
        dpST <- cp2dp(cpST, family="ST")
        sims <- rst(observed(), dp = dpST)
        
        #calculation of SD here.
        Fn <- ecdf(sims)
        xx = Fn(input$meanMD_val)
        
        y = ifelse(xx < 0.5, (1- (.5 - xx)), (1-(xx - .5)))
    
        cpST <- c(input$meanMD_val, input$sd_val*y, input$skew_val, x)
        dpST <- cp2dp(cpST, family="ST")
        sims2 <- rst((input$n_val - observed()), dp = dpST)
        
        sims2=as.data.frame(sims2)
        sims = as.data.frame(sims)
        
        simsTot = data.frame(sims = rep(NA, input$n_val))
        simsTot$sims = c(sims$sims, sims2$sims2)
        simsTot$Fact = c(rep(1, length(sims$sims)), rep(2, length(sims2$sims2)))
        
        
        meanfull = mean(simsTot$sims)
        sefull = sd(simsTot$sims)

        
        simsTot$Age = rnorm_pre(simsTot$sims, mu=input$aux_val, sd=input$aux_sd, r=input$slider_aux)
        
        testmiss = simsTot
        testmiss$sims = ifelse(testmiss$Fact==2, NA, testmiss$sims)
        
        ini1 = mice(testmiss, maxit=0)
        pred = ini1$pred
        
        cca_mean = mean(testmiss$sims, na.rm=T)
        cca_se = sd(testmiss$sims, na.rm=T)

        
        #Remove all filled from being imputed
        meth = ini1$meth

        pred["sims", ] = c(1,0, 0)
        meth[c("sims")] = "norm"
        imp = mice(testmiss, meth=meth, pred=pred, maxit=10, m = 10, pri=F)
        com_MCAR05 = complete(imp, "long")
        
        
        
        mimean = rep(0, 10)
        mise = rep(0, 10)
        
        for (j in 1:10){
            
            mimean[j] <- mean(com_MCAR05$sims[com_MCAR05$.imp==j])
            mise[j] <- sd(com_MCAR05$sims[com_MCAR05$.imp==j])
            
        }
        
        mimean = as.matrix(mimean)
        mise = as.matrix(mise)
        combined.results2 <- mi.meld(q = mimean, se = mise)

        
        miMeanpool = combined.results2$q.mi
        avSE = mean(mise)
        varImp = sd(mise)^2
        avSE = (avSE*(1+(1/10))) + varImp
        miSEpool = avSE
        
        
        
        ini1 = mice(testmiss, maxit=0)
        pred = ini1$pred
        #Remove all filled from being imputed
        meth = ini1$meth
        pred["sims",] = c(1,0,1)
        meth[c("sims")] = "norm"
        
        imp2 = mice(testmiss, meth=meth, pred=pred, maxit=10, m = 10, pri=F)
        com_sims= complete(imp2, "long")
        
        
        
        mimean = rep(0, 10)
        mise = rep(0, 10)
        
        for (j in 1:10){
            
            mimean[j] <- mean(com_sims$sims[com_sims$.imp==j])
            mise[j] <- sd(com_sims$sims[com_sims$.imp==j])
            
        }
        
        mimean = as.matrix(mimean)
        mise = as.matrix(mise)
        combined.results2 <- mi.meld(q = mimean, se = mise)
        
        miMeanPoolCV = combined.results2$q.mi
        avSE = mean(mise)
        varImp = sd(mise)^2
        avSE = (avSE*(1+(1/10))) + varImp
        miSEpoolCV = avSE
        

        sem1 = miSEpool/sqrt(input$n_val)
        
        mi_normlow = miMeanpool - sem1*1.96  
        mi_normhigh = miMeanpool + sem1*1.96 
        
        
        sem2 = miSEpoolCV/sqrt(input$n_val)
        
        mi_auxlow = miMeanPoolCV - sem2*1.96  
        mi_auxhigh = miMeanPoolCV + sem2*1.96 
        
        
        sem3 = sefull/sqrt(input$n_val)
        
        mi_fulllow = meanfull - sem3*1.96  
        mi_fullhigh = meanfull + sem3*1.96 
        
        sem4 = cca_se/sqrt(observed())
        
        cca_low = cca_mean - sem4*1.96
        cca_high = cca_mean + sem4*1.96
        
        
        HI_MI_test = data.frame(`MI Model` = c("Full Data", "Ignoring Missingness", "MI", "MI with Auxiliary Variable"), Lower = c(mi_fulllow, cca_low, mi_normlow, mi_auxlow),
                                Upper = c(mi_fullhigh, cca_high, mi_normhigh, mi_auxhigh), MI_0.9 = c(meanfull, cca_mean, miMeanpool, miMeanPoolCV))
        
        f <- ggplot(
            HI_MI_test, aes(x = MI.Model, y = MI_0.9, ymin = Lower, ymax = Upper, color=MI.Model)) +
            geom_point() 
        f = f + geom_errorbar(width = 0.2) +
            geom_point(size = 3) + geom_linerange(size=1) +
            labs(y="95% Confidence Interval of Variable", x = "Analysis Method",
                 title="Results of MI With and Without Auxiliary Variable")
        
        
        f = f + theme(legend.position="none")
        f = f + theme(plot.title=element_text(size=20, 
                                              face="bold", 
                                              hjust=0.5,
                                              vjust = 2),
                      axis.title.x=element_text(size=18, face="bold", vjust = -4),  # X axis title
                      axis.title.y=element_text(size=16, face="bold", vjust = 4),  # Y axis title
                      axis.text.x=element_text(size=15, face = "bold"),  # X axis text
                      axis.text.y=element_text(size=15))
        f = f + theme(plot.margin=unit(c(1,1,1.5,1.2),"cm"))
        f
      
        
    })
    
    
    
    output$missingmcar <- renderPlot({
        set.seed(123458)
        cpST <- c(95, 20, 0, 0)
        dpST <- cp2dp(cpST, family="ST")
        sims <- rst(150, dp = dpST)
        
        Del = rep(NA, 150)
        Del = sample(c(0,1), length(sims), prob = c(.15, .8), replace = T)   
        sims = as.data.frame(sims)
        
        sims$Missing.Status = Del
        
        sims$Missing.Status = ifelse(sims$Missing.Status==0, "Missing", "Not Missing")
        colnames(sims)[1] = "Alcohol Consumption (Oz)"
        sims$Missing.Status = as.factor(sims$Missing.Status)
        
        z = ggplot(sims, aes(y=`Alcohol Consumption (Oz)`, x = seq_along(sims$`Alcohol Consumption (Oz)`), 
                             group = Missing.Status, 
                             color = Missing.Status)) + geom_point()
        
        z = ggplot(sims, aes(y=`Alcohol Consumption (Oz)`, 
                             x = seq_along(sims$`Alcohol Consumption (Oz)`), group = Missing.Status, 
                             color = Missing.Status)) + geom_point(size=3) +
            labs(title="Alcohol Consumption Values, by Missing Status (MCAR)")
        
        z = z + theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5, vjust = 4),
                      legend.title=element_text(size=14, face="bold"), 
                      legend.text=element_text(size=12),
                      axis.title.x=element_blank(),  # X axis title
                      axis.title.y=element_text(size=14, face="bold"),  # Y axis title  # X axis text
                      axis.ticks.x = element_blank(),
                      axis.text.x = element_blank())
        
        z = z + theme(plot.margin=unit(c(1.5,0.5,.5,.5),"cm"))
        z
    
    })
    
    
    output$missingmar <- renderPlot({
        set.seed(123459)
        cpST <- c(95, 20, 0, 0)
        dpST <- cp2dp(cpST, family="ST")
        sims <- rst(150, dp = dpST)
        Gender = sample(c(0,1), length(sims), prob = c(.5, .5), replace = T) 
        sims = as.data.frame(sims)
        sims$Gender = Gender
        
        Del = rep(NA, 150)
        sims = sims[order(sims$Gender),]
        Del = c(sample(c(0,1), length(sims$Gender[sims$Gender==0]), prob = c(.05, .95), replace = T),
                sample(c(0,1), length(sims$Gender[sims$Gender==1]), prob = c(.25, .75), replace=T))
        
        sims$Missing.Status = Del
        
        sims$Missing.Status = ifelse(sims$Missing.Status==0, "Missing", "Not Missing")
        colnames(sims)[1] = "Alcohol Consumption (Oz)"
        sims$Missing.Status = as.factor(sims$Missing.Status)
        
        simsF = subset(sims, Gender==0)
        simsM = subset(sims, Gender==1)
        
        
        z = ggplot(simsF, aes(y=`Alcohol Consumption (Oz)`, 
                              x = seq_along(simsF$`Alcohol Consumption (Oz)`), group = Missing.Status, 
                              color = Missing.Status)) + geom_point(size=3) 
        
        z = z + theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
                      legend.title=element_text(size=14, face="bold"), 
                      legend.text=element_text(size=12),
                      axis.title.x=element_blank(),  # X axis title
                      axis.title.y=element_text(size=14, face="bold"),  # Y axis title  # X axis text
                      axis.ticks.x = element_blank(),
                      axis.text.x = element_blank())
        z = z + theme(plot.margin=unit(c(1.5,0.5,.5,.5),"cm"))
        
        
        b = ggplot(simsM, aes(y=`Alcohol Consumption (Oz)`, 
                              x = seq_along(simsM$`Alcohol Consumption (Oz)`), group = Missing.Status, 
                              color = Missing.Status)) + geom_point(size=3) 
       
        
        b = b + theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5),
                      legend.title=element_text(size=14, face="bold"), 
                      legend.text=element_text(size=12),
                      axis.title.x=element_blank(),  # X axis title
                      axis.title.y=element_text(size=14, face="bold"),  # Y axis title  # X axis text
                      axis.ticks.x = element_blank(),
                      axis.text.x = element_blank())
        
        b = b + theme(plot.margin=unit(c(1.5,0.5,.5,.5),"cm"))
        
    
        
        d = ggarrange(z, b,
                      common.legend=T,
                      legend="bottom",
                      labels = c("Women", "Men"), vjust=3.5,
                      ncol = 2, nrow = 1)
        
        
        annotate_figure(d,
                        top = text_grob("Alcohol Consumption Values, by Missing Status and Gender", color = "black", face = "bold", size = 18))
     
    })
    
   
     
    output$missingmnar <- renderPlot({
        set.seed(123461)
        cpST <- c(95, 20, 0, 0)
        dpST <- cp2dp(cpST, family="ST")
        sims <- rst(150, dp = dpST)
        
        Del = rep(NA, 150)
        
        mnar = rnorm_pre(sims[1:150], mu =0.15, sd=.1, r=0.9)
        mnar = ifelse(mnar < 0, 0, mnar)
        
        sims = as.data.frame(sims)
        mnar = mnar
        mnar2 = 1-mnar
        
        for (i in 1:length(sims$sims)){
            Del[i] = sample(c(0,1), 1, prob = c(mnar[i], mnar2[i]))}
        
        sims$Missing.Status = Del
        
        sims$Missing.Status = ifelse(sims$Missing.Status==0, "Missing", "Not Missing")
        colnames(sims)[1] = "Alcohol Consumption (Oz)"
        sims$Missing.Status = as.factor(sims$Missing.Status)

        
        z = ggplot(sims, aes(y=`Alcohol Consumption (Oz)`, 
                             x = seq_along(sims$`Alcohol Consumption (Oz)`), group = Missing.Status, 
                             color = Missing.Status)) + geom_point(size=3) +
            labs(title="Alcohol Consumption Values, by Missing Status (MNAR)")
        
        z = z + theme(plot.title = element_text(size = 18, face = "bold", hjust = 0.5, vjust = 4),
                      legend.title=element_text(size=14, face="bold"), 
                      legend.text=element_text(size=12),
                      axis.title.x=element_blank(),  # X axis title
                      axis.title.y=element_text(size=14, face="bold"),  # Y axis title  # X axis text
                      axis.ticks.x = element_blank(),
                      axis.text.x = element_blank())
        z = z + theme(plot.margin=unit(c(1.5,0.5,.5,.5),"cm"))
        z
        
    })
    
    
    
    output$tabletot5 <- renderTable({
        set.seed(123456)
        cpST <- c(90, 20, 0 , 0)
        dpST <- cp2dp(cpST, family="ST")
        sims <- rst(950, dp = dpST)
        
        xx = quantile(sims, probs=c(.1, .2, .3, .4, .5, .6, .7, .8, .9))
        xx = as.data.frame(xx)
        y = c(.6, .7, .8, .9, 1, .9, .8, .7, .6)
        nmiss = 50
        
        sims2 = matrix(NA, nrow = nmiss, ncol=9)
        
        for(i in 1:9){
            cpST <- c(xx[i,1], (20*y[i]), 0, 0)
            dpST <- cp2dp(cpST, family="ST")
            sims2[1:nmiss,i] <- rst(nmiss, dp = dpST)}
        
        
        sims2 = as.data.frame(sims2)
        sims=as.data.frame(sims)
        
        sims2[(nmiss+1):(nmiss+length(sims$sims)),1] = sims$sims
        sims2[(nmiss+1):(nmiss+length(sims$sims)),2] = sims$sims
        sims2[(nmiss+1):(nmiss+length(sims$sims)),3] = sims$sims
        sims2[(nmiss+1):(nmiss+length(sims$sims)),4] = sims$sims
        sims2[(nmiss+1):(nmiss+length(sims$sims)),5] = sims$sims
        sims2[(nmiss+1):(nmiss+length(sims$sims)),6] = sims$sims
        sims2[(nmiss+1):(nmiss+length(sims$sims)),7] = sims$sims
        sims2[(nmiss+1):(nmiss+length(sims$sims)),8] = sims$sims
        sims2[(nmiss+1):(nmiss+length(sims$sims)),9] = sims$sims
        
        colnames(sims2) = c("10th Percentile + Observed Data", "20th Percentile + Observed Data","30th Percentile + Observed Data",
                            "40th Percentile + Observed Data","50th Percentile + Observed Data","60th Percentile + Observed Data",
                            "70th Percentile + Observed Data","80th Percentile + Observed Data","90th Percentile + Observed Data")
        
        sims3 = matrix(NA, nrow = 9, ncol=3)
        
        for(i in 1:9){
            dd = CI(sims2[,i], ci=0.95)
            sims3[i, 1:3] = dd[c(3,2,1)]}
        
        colnames(sims3) = c("Lower Bound", "Mean", "Upper Bound")
        rownames(sims3) = colnames(sims2)[1:9]
        sims3 = as.data.frame(sims3)
        sims3[,4] = rownames(sims3)[1:9]
        sims3 = sims3[1:9, c(4,1,2,3)]
        sims3 = na.omit(sims3)
        sims3 = as.data.frame(sims3)
        colnames(sims3)[1] = " "
        
        sims3
    
    })
   
    
    
    
    output$tabletot10 <- renderTable({
        set.seed(123456)
        cpST <- c(90, 20, 0 , 0)
        dpST <- cp2dp(cpST, family="ST")
        sims <- rst(900, dp = dpST)
        
        xx = quantile(sims, probs=c(.1, .2, .3, .4, .5, .6, .7, .8, .9))
        xx = as.data.frame(xx)
        y = c(.6, .7, .8, .9, 1, .9, .8, .7, .6)
        nmiss = 100
        
        sims2 = matrix(NA, nrow = nmiss, ncol=9)
        
        for(i in 1:9){
            cpST <- c(xx[i,1], (20*y[i]), 0, 0)
            dpST <- cp2dp(cpST, family="ST")
            sims2[1:nmiss,i] <- rst(nmiss, dp = dpST)}
        
        
        sims2 = as.data.frame(sims2)
        sims=as.data.frame(sims)
        
        sims2[(nmiss+1):(nmiss+length(sims$sims)),1] = sims$sims
        sims2[(nmiss+1):(nmiss+length(sims$sims)),2] = sims$sims
        sims2[(nmiss+1):(nmiss+length(sims$sims)),3] = sims$sims
        sims2[(nmiss+1):(nmiss+length(sims$sims)),4] = sims$sims
        sims2[(nmiss+1):(nmiss+length(sims$sims)),5] = sims$sims
        sims2[(nmiss+1):(nmiss+length(sims$sims)),6] = sims$sims
        sims2[(nmiss+1):(nmiss+length(sims$sims)),7] = sims$sims
        sims2[(nmiss+1):(nmiss+length(sims$sims)),8] = sims$sims
        sims2[(nmiss+1):(nmiss+length(sims$sims)),9] = sims$sims
        
        colnames(sims2) = c("10th Percentile + Observed Data", "20th Percentile + Observed Data","30th Percentile + Observed Data",
                            "40th Percentile + Observed Data","50th Percentile + Observed Data","60th Percentile + Observed Data",
                            "70th Percentile + Observed Data","80th Percentile + Observed Data","90th Percentile + Observed Data")
        
        sims3 = matrix(NA, nrow = 9, ncol=3)
        
        for(i in 1:9){
            dd = CI(sims2[,i], ci=0.95)
            sims3[i, 1:3] = dd[c(3,2,1)]}
        
        colnames(sims3) = c("Lower Bound", "Mean", "Upper Bound")
        rownames(sims3) = colnames(sims2)[1:9]
        sims3 = as.data.frame(sims3)
        sims3[,4] = rownames(sims3)[1:9]
        sims3 = sims3[1:9, c(4,1,2,3)]
        sims3 = na.omit(sims3)
        sims3 = as.data.frame(sims3)
        colnames(sims3)[1] = " "
        
        sims3
        
    })
    
    
    
    output$tabletot20 <- renderTable({
        set.seed(123456)
        cpST <- c(90, 20, 0 , 0)
        dpST <- cp2dp(cpST, family="ST")
        sims <- rst(800, dp = dpST)
        
        xx = quantile(sims, probs=c(.1, .2, .3, .4, .5, .6, .7, .8, .9))
        xx = as.data.frame(xx)
        y = c(.6, .7, .8, .9, 1, .9, .8, .7, .6)
        nmiss = 200
        
        sims2 = matrix(NA, nrow = nmiss, ncol=9)
        
        for(i in 1:9){
            cpST <- c(xx[i,1], (20*y[i]), 0, 0)
            dpST <- cp2dp(cpST, family="ST")
            sims2[1:nmiss,i] <- rst(nmiss, dp = dpST)}
        
        
        sims2 = as.data.frame(sims2)
        sims=as.data.frame(sims)
        
        sims2[(nmiss+1):(nmiss+length(sims$sims)),1] = sims$sims
        sims2[(nmiss+1):(nmiss+length(sims$sims)),2] = sims$sims
        sims2[(nmiss+1):(nmiss+length(sims$sims)),3] = sims$sims
        sims2[(nmiss+1):(nmiss+length(sims$sims)),4] = sims$sims
        sims2[(nmiss+1):(nmiss+length(sims$sims)),5] = sims$sims
        sims2[(nmiss+1):(nmiss+length(sims$sims)),6] = sims$sims
        sims2[(nmiss+1):(nmiss+length(sims$sims)),7] = sims$sims
        sims2[(nmiss+1):(nmiss+length(sims$sims)),8] = sims$sims
        sims2[(nmiss+1):(nmiss+length(sims$sims)),9] = sims$sims
        
        colnames(sims2) = c("10th Percentile + Observed Data", "20th Percentile + Observed Data","30th Percentile + Observed Data",
                            "40th Percentile + Observed Data","50th Percentile + Observed Data","60th Percentile + Observed Data",
                            "70th Percentile + Observed Data","80th Percentile + Observed Data","90th Percentile + Observed Data")
        
        sims3 = matrix(NA, nrow = 9, ncol=3)
        
        for(i in 1:9){
            dd = CI(sims2[,i], ci=0.95)
            sims3[i, 1:3] = dd[c(3,2,1)]}
        
        colnames(sims3) = c("Lower Bound", "Mean", "Upper Bound")
        rownames(sims3) = colnames(sims2)[1:9]
        sims3 = as.data.frame(sims3)
        sims3[,4] = rownames(sims3)[1:9]
        sims3 = sims3[1:9, c(4,1,2,3)]
        sims3 = na.omit(sims3)
        sims3 = as.data.frame(sims3)
        colnames(sims3)[1] = " "
        
        sims3

    })
    
    
    
    
    output$tabletot40 <- renderTable({
        set.seed(123456)
        cpST <- c(90, 20, 0 , 0)
        dpST <- cp2dp(cpST, family="ST")
        sims <- rst(600, dp = dpST)
        
        xx = quantile(sims, probs=c(.1, .2, .3, .4, .5, .6, .7, .8, .9))
        xx = as.data.frame(xx)
        y = c(.6, .7, .8, .9, 1, .9, .8, .7, .6)
        nmiss = 400
        
        sims2 = matrix(NA, nrow = nmiss, ncol=9)
        
        for(i in 1:9){
            cpST <- c(xx[i,1], (20*y[i]), 0, 0)
            dpST <- cp2dp(cpST, family="ST")
            sims2[1:nmiss,i] <- rst(nmiss, dp = dpST)}
        
        
        sims2 = as.data.frame(sims2)
        sims=as.data.frame(sims)
        
        sims2[(nmiss+1):(nmiss+length(sims$sims)),1] = sims$sims
        sims2[(nmiss+1):(nmiss+length(sims$sims)),2] = sims$sims
        sims2[(nmiss+1):(nmiss+length(sims$sims)),3] = sims$sims
        sims2[(nmiss+1):(nmiss+length(sims$sims)),4] = sims$sims
        sims2[(nmiss+1):(nmiss+length(sims$sims)),5] = sims$sims
        sims2[(nmiss+1):(nmiss+length(sims$sims)),6] = sims$sims
        sims2[(nmiss+1):(nmiss+length(sims$sims)),7] = sims$sims
        sims2[(nmiss+1):(nmiss+length(sims$sims)),8] = sims$sims
        sims2[(nmiss+1):(nmiss+length(sims$sims)),9] = sims$sims
        
        colnames(sims2) = c("10th Percentile + Observed Data", "20th Percentile + Observed Data","30th Percentile + Observed Data",
                            "40th Percentile + Observed Data","50th Percentile + Observed Data","60th Percentile + Observed Data",
                            "70th Percentile + Observed Data","80th Percentile + Observed Data","90th Percentile + Observed Data")
        
        sims3 = matrix(NA, nrow = 9, ncol=3)
        
        for(i in 1:9){
            dd = CI(sims2[,i], ci=0.95)
            sims3[i, 1:3] = dd[c(3,2,1)]}
        
        colnames(sims3) = c("Lower Bound", "Mean", "Upper Bound")
        rownames(sims3) = colnames(sims2)[1:9]
        sims3 = as.data.frame(sims3)
        sims3[,4] = rownames(sims3)[1:9]
        sims3 = sims3[1:9, c(4,1,2,3)]
        sims3 = na.omit(sims3)
        sims3 = as.data.frame(sims3)
        colnames(sims3)[1] = " "
        
        sims3
        
        
    })
 
}



# Run the application 
shinyApp(ui = ui, server = server)
