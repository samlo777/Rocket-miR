#Load Libraries
{
  #Shiny UI Packages
  library(shiny)
  library(shinydashboard)
  library(shinyBS)
  library(shinyjs)
  library(DT)
  library(shinycssloaders)

  #Analysis Visualization Packages
  library(plotly)
  library(kmer)
  
  #Data Manipulation Packages
  library(stringr)

}

#UI Code
{
  ui <- dashboardPage(
    
    #Initialize Header
    dashboardHeader(
      title = "Rocket-miR",
      titleWidth = 300
      ),
    skin = "blue",
  
    
    #Initialize Sidebar
    dashboardSidebar(
      width = 300,
      sidebarMenu(
        
        #Drop-down menu: let user pick species by associated disease, type (gram negative bacteria, gram positive bacteria, fungi)
        #Also provide button that allows user to reset filters
        menuItem("Species Filter", tabName = "Species Filter",
                 
            br(),
            tags$h6("Filtering by any of the categories below will update"),
            tags$h6("the list of species in the 'Species Selection' window"),
            br(),
            
            selectizeInput(
              inputId = "Filter_Disease",
              label = "Filter by associated disease:",
              choices = c("App data is loading (30 seconds)..."),
              multiple = TRUE
            ),
            
            selectizeInput(
              inputId = "Filter_Species_Type",
              label = "Filter by species type:",
              choices = c("App data is loading (30 seconds)..."),
              multiple = TRUE
            ),
            
            selectizeInput(
              inputId = "Filter_Species_Site",
              label = "Filter by colonization site:",
              choices = c("App data is loading (30 seconds)..."),
              multiple = TRUE
            ),
            
            br(),

            downloadButton(
              outputId = "Download_Species_Info",
              label = "Download table of supported species",
              icon = icon("download"),
              style = "color: black; margin-left: 15px; margin-bottom: 5px;"
            ),
            
            br(),
            br()
            
        ), 
        
        #Menu item: After filter applied, show species that the user can pick. Should be able to pick multiple species, have them show up on sidebar
        #Also provide button that allows user to reset species
        
        menuItem("Species Selection", tabName = "Species Selection",
                 
            radioButtons(
              inputId = "Select_Species",
              label = "Select species of interest",
              choices = c("App data is loading (30 seconds)..."),
              selected = character(0)
            ),
              
            actionButton("Reset_Species_Filters", "Reset to all species"),
            
            br()
            
                    
        ),
        
        #Button that allows the user to open the user manual model dialog again
        menuItem("Re-Open User Manual", tabName = "Re-Open User Manual",
             
            br(),
            
            actionButton("Reopen_User_Manual", "Reopen the user manual window"),
            
            br()
                 
        ),
        
        #Provide links to preprint/published paper, GitHub Repo, and citation for preprint/paper (also provided in user manual)
        menuItem("Reference Publication", tabName = "Reference Publication",
            
            br(),
            
            tags$h6("If you use our application in your own research,"),
            tags$h6("please cite our associated publication:"),
            tags$h6("PUBLICATION CITATION GOES HERE"),
            
            br(),
            
            tags$a(href="http://scangeo.dartmouth.edu/CFSeq/", 
                    "Click here to view our associated publication"),
            
            br(),
            br(),
            
            tags$a(href="http://scangeo.dartmouth.edu/CFSeq/", 
                   "Click here to access our GitHub Repository")
                 
        )
        
      )
    ),
    
    dashboardBody(
      
      useShinyjs(),
      
      fluidRow(
        column(
          width = 12,
            #Note: bsButton is a special kind of button that gives you more options to change its style
            bsButton("Summary_View", #When the user presses one of these button, it should change what analysis outputs (below) they are seeing in the app.
                     type = "action",
                     label = "Summary View", 
                     icon = icon("fingerprint"),
                     size = "large",
                     style = "primary" #The color of the button should turn red after the user presses it (Summary view is red by default)
                     ),
            bsButton("miRNA_View", 
                    type = "action",
                    label = "miRNA View", 
                    icon = icon("chevron-right"),
                    size = "large"
            ),
            bsButton("Pathway_View", 
                     type = "action",
                     label = "Pathway View", 
                     icon = icon("border-none"),
                     size = "large"
            ),
            bsButton("Compare_Species", 
                     type = "action",
                     label = "Compare Species", 
                     icon = icon("comments"),
                     size = "large"
            ),
            bsButton("Structural_Analysis", 
                     type = "action",
                     label = "Structural Analysis", 
                     icon = icon("dna"),
                     size = "large"
            )

          
            #Create boxes for each analysis view: https://rstudio.github.io/shinydashboard/structure.html (change boxes when user changes the view)
      ), 
      
      #Starting Message
      fluidRow(id = "Starting_Message",
               column(
                 width = 12,
                 
                 br(),
                 
                 box(width = 8,
                   code(id = "Please_Select_Species", "Please select one species from the sidebar on
                   the left. This will allow you to see the different analysis views indicated above and download analysis results for all chosen species.
                   Species data may take up to 30 seconds to load. In the meantime, you can re-read the manual or check out the table of supported species in the
                   'Species Filter' section of the drop-down menu."),
                 )
                 
                 
                 
               )
      ),
      
      #Summary View: First Row
      fluidRow(id = "Summary_Row_1",
        column(
          width = 12,
          
          br(),
          
          box(width = 12,
            title = "Selected Species",
            id = "Summary_View_1", height = "100%",
            selectizeInput(inputId = "Select_Chosen_Species_1", label =  "You have selected:", width = "100%",
                           choices = c("[Select a Species]"),
                           selected = "[Select a Species]"
            ),
            strong("To compare multiple species at once, navigate to the 'Compare Species' view. To change your selected species, use the sidebar. For more info on this section of the app, read the 'Summary View Explainer'", style = "color:blue"),
            br(),
            br(),
            actionButton("Summary_View_Explainer", "Summary View Explainer")
          )
          
        )
      ),
      
      #Summary View: Second Row
      fluidRow(id = "Summary_Row_2",
        
        column(
          width = 12,
          box(width = 6, collapsible = T,
              title = "Top miRNAs",
              id = "Summary_View_2", height = "100%",
              shinycssloaders::withSpinner(
              DT::dataTableOutput("Top_miRNAs", width = "100%"), type = 6, color = #1C84C4
              ),
              br(),
              actionButton("Energy_Score_Explainer_1", "Energy Score Explainer"),
              downloadButton("Summary_2", "Download Table"),
              br()
              
          ),
          
          box(width = 6, collapsible = T,
              title = "Top Targeted Proteins",
              id = "Summary_View_3", height = "100%",
              shinycssloaders::withSpinner(
                DT::dataTableOutput("Top_Proteins", width = "100%"), type = 6, color = #1C84C4
              ),
              br(),
              actionButton("Energy_Score_Explainer_2", "Energy Score Explainer"),
              downloadButton("Summary_3", "Download Table"),
              br()
              
          )
          
        )
      ),
      
      #Summary View: Third Row
      fluidRow(id = "Summary_Row_3",
        
        column(
          width = 12,
          box(width = 6, collapsible = T,
              title = "Energy Score Heat Map",
              id = "Summary_View_4", height = "100%",
              shinycssloaders::withSpinner(
              plotlyOutput("HeatMap"), type = 6, color = #1C84C4
              ),
              br(),
              actionButton("Heat_Map_Explainer", "Heat Map Explainer"),
              br()
          ),
          
          box(width = 6, collapsible = T,
              title = "Energy Score Distribution",
              id = "Summary_View_5", height = "100%",
              selectizeInput(inputId = "Select_Summary_Statistic", label =  "Select a summary statistic for the distribution", width = "100%",
                             choices = c("Mean", "Median", "Minimum"),
                             selected = "[Default = X]"
              ),
              shinycssloaders::withSpinner(
              plotOutput("EnergyScoreDistribution"), type = 6, color = #1C84C4
              ),
              downloadButton("Summary_5", "Download Figure")
          )
          
        )
        
        
      ),
      
      #miRNA View: First Row
      fluidRow(id = "miRNA_Row_1",
        column(width = 12, 
               
               br(), 
               
               box(width = 6,
                 title = "Selected Species",
                 id = "miRNA_View_1", height = "100%",
                 selectizeInput(inputId = "Select_Chosen_Species_2", label =  "You have selected:", width = "100%",
                                choices = c("[Select a Species]"),
                                selected = "[Select a Species]"
                 ),
                 strong("To compare multiple species at once, navigate to the 'Compare Species' view. To change your selected species, use the sidebar. For more info on this section of the app, read the 'miRNA View Explainer'", style = "color:blue")
               ),
               
               box(width = 6,
                   title = "Select miRNA of Interest",
                   id = "miRNA_View_2", height = "100%",
                   selectizeInput(inputId = "Select_miRNA", label =  "Select a miRNA of interest to view analysis outputs", width = "100%",
                                  choices = c("[Select a miRNA]"),
                                  selected = "[Select a miRNA]"
                   ),
                   actionButton("miRNA_View_Explainer", "miRNA View Explainer")
                            
                   
               )     
        )
               
    
      ),
      
      #miRNA View: Second Row
      fluidRow(id = "miRNA_Row_2",
        column(width = 12,
               
               box(width = 12, collapsible = T,
                   title = "Best Targeting miRNAs",
                   id = "miRNA_View_3", height = "100%",
                   selectizeInput(inputId = "Select_Targeting_Cutoff_1", label =  "Select a targeting cutoff", width = "100%",
                                  choices = c("Strong", "Medium", "Weak"),
                                  selected = "Strong"
                   ),
                   br(),
                   DT::dataTableOutput("Top_miRNAs_2", width = "100%"),
                   br(),
                   actionButton("Targeting_Cutoff_Explainer_1", "Targeting Cutoff Explainer"),
                   actionButton("KEGG_Pathway_Image_Explainer", "KEGG Pathway Image Explainer"),
                   downloadButton("miRNA_3", "Download Table")
                   
               )     
               
        ) 
      ),
      
      #Pathway View: First Row
      fluidRow(id = "Pathway_Row_1",
        column(width = 12, 
               
               br(),
               
               box(width = 6,
                   title = "Selected Species",
                   id = "Pathway_View_1", height = "100%",
                   selectizeInput(inputId = "Select_Chosen_Species_3", label =  "You have selected:", width = "100%",
                                  choices = c("[Select a Species]"),
                                  selected = "[Select a Species]"
                   ),
                   strong("To compare multiple species at once, navigate to the 'Compare Species' view. To change your selected species, use the sidebar. For more info on this section of the app, read the 'Pathway View Explainer'", style = "color:blue")
                   
               ),
               
               box(width = 6,
                   title = "Select Pathway of Interest",
                   id = "Pathway_View_2", height = "100%",
                   selectizeInput(inputId = "Select_Pathway", label =  "Select a pathway of interest to view analysis outputs", width = "100%",
                                  choices = c("[Select a Pathway]"),
                                  selected = "[Select a Pathway]"
                   ),
                   textOutput("Pathway_Length"),
                   br(),
                   actionButton("Pathway_View_Explainer", "Pathway View Explainer")
                   
               )     
               
        ) 
                     
      ),
      
      #Pathway View: Second Row
      fluidRow(id = "Pathway_Row_2",
        column(width = 12,
              
               box(width = 12, collapsible = T,
                   title = "Top Targeted Pathways",
                   id = "Pathway_View_3", height = "100%",
                   selectizeInput(inputId = "Select_Targeting_Cutoff_2", label =  "Select a targeting cutoff", width = "100%",
                                  choices = c("Strong", "Medium", "Weak"),
                                  selected = "Strong"
                   ),
                   br(),
                   DT::dataTableOutput("Top_Pathways", width = "100%"),
                   br(),
                   actionButton("Targeting_Cutoff_Explainer_2", "Targeting Cutoff Explainer"),
                   downloadButton("Pathway_3", "Download Table")
                   
               )
               
        )      
      ),
      
      #Comparison View: First Row
      fluidRow(id = "Comparison_Row_1",
               column(width = 12, 
                      
                      br(), 
                      
                      box(width = 6,
                          title = "Select Chosen Species",
                          id = "Comparison_View_1", height = "100%",
                          selectizeInput(
                            inputId = "Select_Chosen_Species_4",
                            label = "Select multiple species of interest",
                            choices = c(""),
                            multiple = TRUE
                          ),
                          actionButton("Compare_Species_Explainer", "Compare Species Explainer")
                      ),
                      
                      box(width = 6,
                          title = "Select miRNA of Interest",
                          id = "Comparison_View_2", height = "100%",
                          selectizeInput(inputId = "Select_miRNA_2", label =  "Select a miRNA of interest to view analysis outputs", width = "100%",
                                         choices = c("[Select a miRNA]"),
                                         selected = "[Select a miRNA]"
                          )
                          
                      ),
                      
               )
               
               
      ),
      
      #Comparison View: Second Row
      fluidRow(id = "Comparison_Row_2",
               column(width = 12,
                      
                      box(width = 12, collapsible = T,
                          title = "Best Targeting miRNAs",
                          id = "Comparison_View_3", height = "100%",
                          selectizeInput(inputId = "Select_Targeting_Cutoff_3", label =  "Select a targeting cutoff", width = "100%",
                                         choices = c("Strong","Medium","Weak"),
                                         selected = "Strong"
                          ),
                          br(),
                          plotlyOutput("Community_Health_Plot"),
                          br(),
                          selectizeInput(inputId = "Select_Pathway_2", label =  "Select a pathway of interest to view analysis outputs", width = "100%",
                                         choices = c("[Select a set of species to view shared pathways]"),
                                         selected = "[Select a set of species to view shared pathways]"
                          ),
                          
                          strong("The graphic above works like a health bar - a shorter bar indicates that the selected pathway is more heavily targeted by the selected miRNA.", style = "color:blue"),
                          strong("The pathway selection menu includes all pathways that the selected species have in common.", style = "color:blue"),
                          
                          br(),
                          br(),
                          
                          actionButton("Targeting_Cutoff_Explainer_3", "Targeting Cutoff Explainer"),
                          
                          br()
                          
                      )     
                      
               ) 
      ),
      
      #Structural Analysis View: First Row
      fluidRow(id = "Structure_Row_1",
               column(width = 12,
                      
                      br(),
                      
                      box(width = 12,
                          title = "Select miRNA of Interest",
                          id = "Structure_View_1", height = "100%",
                          selectizeInput(inputId = "Select_miRNA_3", label =  "Select a miRNA of interest to view analysis outputs", width = "100%",
                                         choices = c("[Select a miRNA]"),
                                         selected = "[Select a miRNA]",
                                         multiple = TRUE
                          ),
                          actionButton("Structural_Analysis_Explainer", "Structural Analysis Explainer")
                          
                      )
                      
               ) 
      ),
      
      #Structural Analysis View: Second Row
      fluidRow(id = "Structure_Row_2",
               column(width = 12,
                      
                      box(width = 6,
                          title = "miRNA Sequence Details",
                          id = "Structure_View_2", height = "100%",
                          DT::dataTableOutput("miRNA_Sequences", width = "100%"),
                          downloadButton("Structure_2", "Download Table")
                      ),
                      
                      box(width = 6,
                          title = "miRNA Sequence Clustering",
                          id = "Structure_View_3", height = "100%",
                          plotOutput("miRNA_Clustering"),
                          downloadButton("Structure_3", "Download Figure")
                      )   
                      
               ) 
      ),
      
      #Structural Analysis View: Third Row
      fluidRow(id = "Structure_Row_3",
               column(width = 12,
                      
                      box(width = 12, collapsible = T,
                          title = "miRNA Targeting motifs",
                          id = "Structure_View_4", height = "100%",
                          selectizeInput(inputId = "Select_k_value", label =  "Select a miRNA sequence motif length", width = "100%",
                                         choices = c(3,4,5,6,7),
                                         selected = 3
                          ),
                          br(),
                          plotlyOutput("Kmer_Counting_Output"),
                          br(),
                          actionButton("Kmer_Counting_Explainer", "Kmer Counting Explainer"),
                          br()
                          
                      )  
                      
               ) 
      )
      
      
  )
  )
  )
}

#Server Code
{
  server <- function(input, output, session) {
    
    #Hide analysis windows until species is selected
    {
      shinyjs::hide(id = "Summary_Row_1")
      shinyjs::hide(id = "Summary_Row_2")
      shinyjs::hide(id = "Summary_Row_3")
      shinyjs::hide(id = "Summary_Row_4")
      shinyjs::hide(id = "miRNA_Row_1")
      shinyjs::hide(id = "miRNA_Row_2")
      shinyjs::hide(id = "Pathway_Row_1")
      shinyjs::hide(id = "Pathway_Row_2")
      shinyjs::hide(id = "Comparison_Row_1")
      shinyjs::hide(id = "Comparison_Row_2")
      shinyjs::hide(id = "Structure_Row_1")
      shinyjs::hide(id = "Structure_Row_2")
      shinyjs::hide(id = "Structure_Row_3")
    }
    
    #Launch Rocket-miR Manual
    {
      showModal(modalDialog(
        title = "Rocket-miR: A Translational Launchpad for miRNA-based Antimicrobial Drug Discovery",
        strong("Application Goals: "),
        br(),
        br(),
        "The goal of this web application is to provide a translational launchpad for a new class of miRNA-based antimicrobial therapies",
        br(),
        br(),
        "We have found in prior work",
        tags$a(href = 'https://www.pnas.org/doi/10.1073/pnas.2105370118', "(Koeppen et al., 2021)"),
        "that a certain human miRNA (let-7b-5p) is capable of infiltrating cells of the common cystic fibrosis (CF) pathogen Pseudomonas aeruginosa and inhibiting protein activity - with the positive consequences (from the human perspective) of reducing antibiotic resistance and bacterial biofilm formation, two hallmarks of recalcitrant infection.",
        br(),
        br(),
        "We believe that this principle may be applied to find other miRNAs that could be useful to treat drug-resistant infections, in Pseudomonas as well as additional pathogens that are implicated in CF or other human diseases. The app provides predictions of miRNA activity that can be tested in the lab. If successful, these experiments would lay the basis for miRNAs as antimicrobial drug candidates.",
        br(),
        br(),
        strong("Application Use: "),
        br(),
        br(),
        "The Rocket-miR application is broken up into five sections, each of which explores miRNA targeting from a slightly different perspective:",
        br(),
        br(),
        "1. Summary View",
        br(),
        "2. miRNA View",
        br(),
        "3. Pathway View",
        br(),
        "4. Compare Species",
        br(),
        "5. Structural Analysis",
        br(),
        br(),
        "Each section comes with text explaining why it is useful and how it can be used. To get started, just close this user manual and select a species in the sidebar."
      ))
    }
    
    #Load Data
    {
      #Load App metadata
      {
        load("Data1.Rdata")
      }
      
      #Load Core App data
      {
        load("Data2.Rdata")
        load("Data3.Rdata")
        load("Data4.Rdata")
        load("Data5.Rdata")
      }
    }
    
    #Initialize Variables
    {
      #Species metadata
      {
        ###
        Supported_Species_List <- c()
        for (i in 1:length(unique(Supported_Species$Species))) {
          Supported_Species_List <- append(Supported_Species_List, unlist(strsplit(unique(Supported_Species$Species)[i], ",")))
        }
        for(i in 1:length(Supported_Species_List)) {
          if (substr(Supported_Species_List[i], 1,1) == " ") {
            Supported_Species_List[i] <- substr(Supported_Species_List[i], 2, nchar(Supported_Species_List[i]))
          }
        }
        for (i in 1:length(Supported_Species_List)) {
          if (substr(Supported_Species_List[i], nchar(Supported_Species_List[i]), nchar(Supported_Species_List[i])) == " ") {
            Supported_Species_List[i] <- substr(Supported_Species_List[i], 1, nchar(Supported_Species_List[i])-1)
          }
        }
        ###
        
        ###
        Supported_Strains_List <- c()
        for (i in 1:length(unique(Supported_Species$Strain))) {
          Supported_Strains_List <- append(Supported_Strains_List, unlist(strsplit(unique(Supported_Species$Strain)[i], ",")))
        }
        for(i in 1:length(Supported_Strains_List)) {
          if (substr(Supported_Strains_List[i], 1,1) == " ") {
            Supported_Strains_List[i] <- substr(Supported_Strains_List[i], 2, nchar(Supported_Strains_List[i]))
          }
        }
        for (i in 1:length(Supported_Strains_List)) {
          if (substr(Supported_Strains_List[i], nchar(Supported_Strains_List[i]), nchar(Supported_Strains_List[i])) == " ") {
            Supported_Strains_List[i] <- substr(Supported_Strains_List[i], 1, nchar(Supported_Strains_List[i])-1)
          }
        }
        ###
        
        ###
        Supported_Disease_List <- c()
        for (i in 1:length(unique(Supported_Species$Disease))) {
          Supported_Disease_List <- append(Supported_Disease_List, unlist(strsplit(unique(Supported_Species$Disease)[i], ",")))
        }
        for(i in 1:length(Supported_Disease_List)) {
          if (substr(Supported_Disease_List[i], 1,1) == " ") {
            Supported_Disease_List[i] <- substr(Supported_Disease_List[i], 2, nchar(Supported_Disease_List[i]))
          }
        }
        for (i in 1:length(Supported_Disease_List)) {
          if (substr(Supported_Disease_List[i], nchar(Supported_Disease_List[i]), nchar(Supported_Disease_List[i])) == " ") {
            Supported_Disease_List[i] <- substr(Supported_Disease_List[i], 1, nchar(Supported_Disease_List[i])-1)
          }
        }
        ###
        Supported_Type_List <- c()
        for (i in 1:length(unique(Supported_Species$Type))) {
          Supported_Type_List <- append(Supported_Type_List, unlist(strsplit(unique(Supported_Species$Type)[i], ",")))
        }
        for(i in 1:length(Supported_Type_List)) {
          if (substr(Supported_Type_List[i], 1,1) == " ") {
            Supported_Type_List[i] <- substr(Supported_Type_List[i], 2, nchar(Supported_Type_List[i]))
          }
        }
        for (i in 1:length(Supported_Type_List)) {
          if (substr(Supported_Type_List[i], nchar(Supported_Type_List[i]), nchar(Supported_Type_List[i])) == " ") {
            Supported_Type_List[i] <- substr(Supported_Type_List[i], 1, nchar(Supported_Type_List[i])-1)
          }
        }
      }
    } 
    
    #Update species selection menus
    {
      observe({
        updateSelectizeInput(session, inputId = "Filter_Disease", choices = Supported_Disease_List, selected = "")
      })
      
      observe({
        updateSelectizeInput(session, inputId = "Filter_Species_Type", choices = Supported_Type_List, selected = "")
      })
      
      observe({
        updateRadioButtons(session, inputId = "Select_Species", choices = Supported_Species_List, selected = "")
      })
      
    }
    
    #A. Species Selection Sidebar
    {
      #Filter species list by selected disease(s)
      {
        observeEvent(input$Filter_Disease, {
          
          Index_List <- c()
          
          for(i in 1:length(input$Filter_Disease)) {
            Index_List <- append(Index_List, grep(input$Filter_Disease[i], Supported_Species$Disease))
          }
          
          Index_List <- unique(Index_List)
          
          Vec <- c()
          for (i in 1:length(Index_List)) {
            Ind <- Index_List[i]
            Vec <- append(Vec, Supported_Species$Species[Ind])
          }
          
          observe({
            updateRadioButtons(session, inputId = "Select_Species", choices = Vec, selected = character(0))
          })
          
          observe({
            updateSelectizeInput(session, inputId = "Filter_Species_Type", choices = Supported_Type_List, selected = "")
          })
          
          
        })
      }
      
      #Filter species list by selected species type(s)
      {
        observeEvent(input$Filter_Species_Type, {
          
          Index_List <- c()
          
          for(i in 1:length(input$Filter_Species_Type)) {
            Index_List <- append(Index_List, grep(input$Filter_Species_Type[i], Supported_Species$Type))
          }
          
          Index_List <- unique(Index_List)
          
          Vec <- c()
          for (i in 1:length(Index_List)) {
            Ind <- Index_List[i]
            Vec <- append(Vec, Supported_Species$Species[Ind])
          }
          
          observe({
            updateRadioButtons(session, inputId = "Select_Species", choices = Vec, selected = character(0))
          })
          
          observe({
            updateSelectizeInput(session, inputId = "Filter_Disease", choices = Supported_Disease_List, selected = "")
          })
          
          
        })
      }
      
      #Filter species list by selected colonization site(s)
      {
        observeEvent(input$Filter_Species_Site, {
          
          Index_List <- c()
          
          for(i in 1:length(input$Filter_Species_Site)) {
            Index_List <- append(Index_List, grep(input$Filter_Species_Site[i], Supported_Species$Site))
          }
          
          Index_List <- unique(Index_List)
          
          Vec <- c()
          for (i in 1:length(Index_List)) {
            Ind <- Index_List[i]
            Vec <- append(Vec, Supported_Species$Species[Ind])
          }
          
          observe({
            updateRadioButtons(session, inputId = "Select_Species", choices = Vec, selected = character(0))
          })
          
          observe({
            updateSelectizeInput(session, inputId = "Filter_Disease", choices = Supported_Disease_List, selected = "")
          })
          
          observe({
            updateSelectizeInput(session, inputId = "Filter_Species_Type", choices = Supported_Type_List, selected = "")
          })
          
          
        })
      }
      
      #Reset species filters
      {
        observeEvent(input$Reset_Species_Filters, {
          
          updateRadioButtons(
            inputId = "Select_Species",
            label = "Select species of interest",
            choices = Supported_Species_List,
            selected = character(0)
          )
          
          observe({
            updateSelectizeInput(session, inputId = "Filter_Disease", choices = Supported_Disease_List, selected = "")
          })
          
          observe({
            updateSelectizeInput(session, inputId = "Filter_Species_Type", choices = Supported_Type_List, selected = "")
          })
          
        })
      }
      
      #Download supported species table
      {
        output$Download_Species_Info <- downloadHandler(
          filename = "Supported_Species.csv",
          content = function(file) {
            write.csv(Supported_Species, file, row.names = TRUE)
          }
        )
      }
      
      #Reopen User Manual
      {
        
        observeEvent(input$Reopen_User_Manual, {
          #Launch Rocket-miR Manual
          {
            showModal(modalDialog(
              title = "Rocket-miR: A Translational Launchpad for miRNA-based Antimicrobial Drug Discovery",
              strong("Application Goals: "),
              br(),
              br(),
              "The goal of this web application is to provide a translational launchpad for a new class of miRNA-based antimicrobial therapies",
              br(),
              br(),
              "We have found in prior work",
              tags$a(href = 'https://www.pnas.org/doi/10.1073/pnas.2105370118', "(Koeppen et al., 2021)"),
              "that a certain human miRNA (let-7b-5p) is capable of infiltrating cells of the common cystic fibrosis (CF) pathogen Pseudomonas aeruginosa and inhibiting protein activity - with the positive consequences (from the human perspective) of reducing antibiotic resistance and bacterial biofilm formation, two hallmarks of recalcitrant infection.",
              br(),
              br(),
              "We believe that this principle may be applied to find other miRNAs that could be useful to treat drug-resistant infections, in Pseudomonas as well as additional pathogens that are implicated in CF or other human diseases. The app provides predictions of miRNA activity that can be tested in the lab. If successful, these experiments would lay the basis for miRNAs as antimicrobial drug candidates.",
              br(),
              br(),
              strong("Application Use: "),
              br(),
              br(),
              "The Rocket-miR application is broken up into five sections, each of which explores miRNA targeting from a slightly different perspective:",
              br(),
              br(),
              "1. Summary View",
              br(),
              "2. miRNA View",
              br(),
              "3. Pathway View",
              br(),
              "4. Compare Species",
              br(),
              "5. Structural Analysis",
              br(),
              br(),
              "Each section comes with text explaining why it is useful and how it can be used. To get started, just close this user manual and select a species in the sidebar."
            ))
          }
        })
        
      }
    }
    
    observeEvent(input$Select_Species, {
    
    #Reset all inputs outputs and outputs from previous species selection
    {
      output$Top_miRNAs <- NULL
      output$Top_Proteins <- NULL
      output$HeatMap <- NULL
      output$EnergyScoreDistribution <- NULL
      output$Top_miRNAs_2 <- NULL
      output$Top_Pathways <- NULL
      output$Community_Health_Plot <- NULL
      output$miRNA_Sequences <- NULL
      output$miRNA_Clustering <- NULL
      output$Kmer_Counting_Output <- NULL
    }
      
    #Teleport user back to Summary View if they select a new species
    {
      shinyjs::show(id = "Summary_Row_1")
      shinyjs::show(id = "Summary_Row_2")
      shinyjs::show(id = "Summary_Row_3")
      shinyjs::show(id = "Summary_Row_4")
      shinyjs::hide(id = "Starting_Message")
      shinyjs::hide(id = "miRNA_Row_1")
      shinyjs::hide(id = "miRNA_Row_2")
      shinyjs::hide(id = "Pathway_Row_1")
      shinyjs::hide(id = "Pathway_Row_2")
      shinyjs::hide(id = "Comparison_Row_1")
      shinyjs::hide(id = "Comparison_Row_2")
      shinyjs::hide(id = "Structure_Row_1")
      shinyjs::hide(id = "Structure_Row_2")
      shinyjs::hide(id = "Structure_Row_3")
      
      updateButton(session, "Summary_View", 
                   label = "Summary View", 
                   icon = icon("fingerprint"),
                   size = "large",
                   style = "primary")
      
      updateButton(session, "miRNA_View", 
                   label = "miRNA View", 
                   icon = icon("chevron-right"),
                   size = "large",
                   style = "default")
      
      updateButton(session, "Pathway_View", 
                   label = "Pathway View", 
                   icon = icon("border-none"),
                   size = "large",
                   style = "default")
      
      updateButton(session, "Compare_Species", 
                   label = "Compare Species", 
                   icon = icon("comments"),
                   size = "large",
                   style = "default")
      
      updateButton(session, "Structural_Analysis", 
                   label = "Structural Analysis", 
                   icon = icon("dna"),
                   size = "large",
                   style = "default")
    }
      
    IntaRNA_Data <<- Target_Prediction_Data[[which(names(Target_Prediction_Data) == input$Select_Species[1])]][,c(1:630)]

    #Update all select species drop-down menus throughout the application
    {
        updateSelectizeInput(session, inputId = "Select_Chosen_Species_1", "You have selected:", choices = c(input$Select_Species))
        updateSelectizeInput(session, inputId = "Select_Chosen_Species_2", "You have selected:", choices = c(input$Select_Species))
        updateSelectizeInput(session, inputId = "Select_Chosen_Species_3", "You have selected:", choices = c(input$Select_Species))
        updateSelectizeInput(session, inputId = "Select_Chosen_Species_4", "You have selected:", choices = Supported_Species_List)
        
      }
      
    #B. Summary View
    {
      #Initial selection of chosen species to view summary outputs
      {

          #Show summary view windows
          {
            shinyjs::show(id = "Summary_Row_1")
            shinyjs::show(id = "Summary_Row_2")
            shinyjs::show(id = "Summary_Row_3")
            shinyjs::show(id = "Summary_Row_4")
            shinyjs::hide(id = "Starting_Message")
            shinyjs::hide(id = "miRNA_Row_1")
            shinyjs::hide(id = "miRNA_Row_2")
            shinyjs::hide(id = "Pathway_Row_1")
            shinyjs::hide(id = "Pathway_Row_2")
            shinyjs::hide(id = "Comparison_Row_1")
            shinyjs::hide(id = "Comparison_Row_2")
            shinyjs::hide(id = "Structure_Row_1")
            shinyjs::hide(id = "Structure_Row_2")
            shinyjs::hide(id = "Structure_Row_3")
          }
        
          #Show Summary View Explainer
          {
            observeEvent(input$Summary_View_Explainer, {
              showModal(modalDialog(
                title = "Summary View Explainer",
                "Here in the summary view, you can identify the miRNAs predicted to be most effective at targeting your pathogen of interest.",
                br(),
                br(),
                "The best way to start is by opening up the ‘Energy Score Heat Map’. Here, the predicted most effective miRNAs are represented by columns that are darker blue in color. If you choose a single miRNA, the ‘Top miRNAs’ panel and the ‘Energy Score Distribution’ panel will give you a sense of how its energy scores stack up to compared to all the other miRNAs.",
                br(),
                br(),
                "For a more functional perspective on the impact of effective miRNAs, you can search the ‘Top Targeted Proteins’ panel in two ways. Entering a miRNA of interest in the search bar will show you all the proteins that are most effectively targeted by that miRNA. Alternatively, you could search for a protein of interest and see what miRNA targets it most effectively."
              ))
            })
          }
        
          #Show Energy Score Explainer 1
          {
            observeEvent(input$Energy_Score_Explainer_1, {
              showModal(modalDialog(
                title = "Energy Score Explainer",
                "The energy score predictions depend on the IntaRNA algorithm (See details in the sidebar ‘Reference Publication’ tab). A more negative energy score indicates a stronger interaction between the targeting miRNA and targeted mRNA.",
                br(),
                br(),
                "The algorithm predicts the strength of interaction between two separate RNA molecules based on their nucleotide sequence (in our case - targeting human miRNAs and a targeted microbial mRNA), but also takes into account the tendency of the RNAs to undergo intra-molecular (internal) interactions and form a secondary structure that is incapable of interacting with other miRNAs. If this tendency is high, the energy score is reduced in magnitude (becomes less negative).",
                br(),
                br(),
                "For reference, see:",
                br(),
                br(),
                "Review: https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4838349/",
                br(),
                "IntaRNA: https://pubmed.ncbi.nlm.nih.gov/18940824/",
                br(),
                "IntaRNA 2.0: https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5570192/#B2"
              ))
            })
          }
        
          #Show Energy Score Explainer 2
          {
            observeEvent(input$Energy_Score_Explainer_2, {
              showModal(modalDialog(
                title = "Energy Score Explainer",
                "The energy score predictions depend on the IntaRNA algorithm (See details in the sidebar ‘Reference Publication’ tab). A more negative energy score indicates a stronger interaction between the targeting miRNA and targeted mRNA.",
                br(),
                br(),
                "The algorithm predicts the strength of interaction between two separate RNA molecules based on their nucleotide sequence (in our case - targeting human miRNAs and a targeted microbial mRNA), but also takes into account the tendency of the RNAs to undergo intra-molecular (internal) interactions and form a secondary structure that is incapable of interacting with other miRNAs. If this tendency is high, the energy score is reduced in magnitude (becomes less negative).",
                br(),
                br(),
                "For reference, see:",
                br(),
                br(),
                "Review: https://www.ncbi.nlm.nih.gov/pmc/articles/PMC4838349/",
                br(),
                "IntaRNA: https://pubmed.ncbi.nlm.nih.gov/18940824/",
                br(),
                "IntaRNA 2.0: https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5570192/#B2"
              ))
            })
          }
        
          #Show Heat Map Explainer
          observeEvent(input$Heat_Map_Explainer, {
            showModal(modalDialog(
              title = "Heat Map Explainer",
              "The top 10% of miRNAs by mean targeting score are represented",
              "Darker blue columns represent miRNAs that target many genes well.",
              "Darker blue rows represent genes that are targeted well by many different miRNAs.",
              "Drag your cursor along the plot to zoom in on certain areas."
            ))
            
          })
          
          #Create outputs
          observeEvent(input$Select_Chosen_Species_1, {
            
            if(input$Select_Chosen_Species_1 != "[Select a Species]") {

              
              #Output: Table - List of top targeting miRNAs, can filter by different elements of the five number summary (+ mean)
              {
                Top_miRNAs_DF <- data.frame(unlist(apply(IntaRNA_Data, 2, mean, na.rm = TRUE)), 
                                            unlist(apply(IntaRNA_Data, 2, median, na.rm = TRUE)),
                                            unlist(apply(IntaRNA_Data, 2, min, na.rm = TRUE)))
                colnames(Top_miRNAs_DF) <- c("Mean Energy Score", "Median Energy Score", "Minimum Energy Score")
                
                output$Top_miRNAs <- DT::renderDataTable(
                  {
                    shiny::isolate(Top_miRNAs_DF)
                  },
                  escape = F,
                  rownames = TRUE,
                  options = list(processing = FALSE, order = list(1, 'asc')), 
                )
                
              }
              
              #Output: Table - List of top targeted genes/proteins
              {
                Top_Proteins_DF <- Target_Protein_Data[[which(names(Target_Protein_Data) == input$Select_Chosen_Species_1)]]
                
                output$Top_Proteins <- DT::renderDataTable(
                  {
                    shiny::isolate(Top_Proteins_DF)
                  },
                  escape = F,
                  rownames = TRUE,
                  options = list(processing = FALSE, order = list(2, 'asc')), 
                )
              }
              
              #Output: Figure - Heatmap of miRNA-gene interaction scores for chosen species 
              {
              
              #Display heatmap
                  output$HeatMap <- renderPlotly({
                    
                    plot_ly(
                      z = as.matrix(Energy_HM_Data[[which(names(Energy_HM_Data) == input$Select_Species[1])]][,c(1:63)]), type = "heatmap"
                    ) %>% layout(plot_bgcolor='#e5ecf6',
                                 xaxis = list(
                                   title='miRNA',
                                   zerolinecolor = '#ffff',
                                   zerolinewidth = 2,
                                   gridcolor = 'ffff',
                                   showticklabels = FALSE),
                                 yaxis = list(
                                   title='Protein Target',
                                   zerolinecolor = '#ffff',
                                   zerolinewidth = 2,
                                   gridcolor = 'ffff',
                                   showticklabels = FALSE)) %>%
                      add_trace(type = "heatmap", x = colnames(Energy_HM_Data[[which(names(Energy_HM_Data) == input$Select_Species[1])]][,c(1:63)]), y = Energy_HM_Data[[which(names(Energy_HM_Data) == input$Select_Species[1])]][,64]) %>%
                      hide_colorbar()
                  })
                  

              }
              
              #Output: Figure - Distributions of energy scores for the chosen species
              {
                observeEvent(input$Select_Summary_Statistic, {
                  
                  if (input$Select_Summary_Statistic == "Mean") {
                    
                    output$EnergyScoreDistribution <- renderPlot({
                      hist(unlist(lapply(IntaRNA_Data[,1:ncol(IntaRNA_Data)], mean, na.rm = TRUE)), xlab = "Mean", main = NULL)
                    })
                    
                  } else if (input$Select_Summary_Statistic == "Median") {
                    
                    output$EnergyScoreDistribution <- renderPlot({
                      hist(unlist(lapply(IntaRNA_Data[,1:ncol(IntaRNA_Data)], median, na.rm = TRUE)), xlab = "Median", main = NULL)
                    })
                    
                  } else {
                    
                    output$EnergyScoreDistribution <- renderPlot({
                      hist(unlist(lapply(IntaRNA_Data[,1:ncol(IntaRNA_Data)], min, na.rm = TRUE)), xlab = "Minimum", main = NULL)
                    })
                    
                  }
                  
                })
                
              }
              
              #Download Outputs
              {
                #Top miRNAs
                output$Summary_2 <- downloadHandler(
                  filename = function() {
                    paste(input$Select_Chosen_Species_1, "_Top_miRNAs", ".csv", sep = "")
                  },
                  content = function(file) {
                    write.csv(Top_miRNAs_DF, file, row.names = TRUE)
                  }
                )
                
                #Top Targeted Proteins
                output$Summary_3 <- downloadHandler(
                  filename = function() {
                    paste(input$Select_Chosen_Species_1, "_Top_Targeted_Proteins", ".csv", sep = "")
                  },
                  content = function(file) {
                    write.csv(Top_Proteins_DF, file, row.names = TRUE)
                  }
                )
                
                #Download Heatmap: Can download directly from plotly
                
                #Energy Score Distribution
                output$Summary_5 <- downloadHandler(
                  filename = function() {
                    paste(input$Select_Chosen_Species_1, "_Energy_Score_Distribution", ".png", sep = "")
                  },
                  content = function(file) {
                    png(file)
                    
                    if (input$Select_Summary_Statistic == "Mean") {
                    
                        hist(unlist(lapply(IntaRNA_Data[,1:ncol(IntaRNA_Data)], mean, na.rm = TRUE)), xlab = "Mean", main = NULL)
                      
                    } else if (input$Select_Summary_Statistic == "Median") {
                      
                        hist(unlist(lapply(IntaRNA_Data[,1:ncol(IntaRNA_Data)], median, na.rm = TRUE)), xlab = "Median", main = NULL)

                    } else {
                      
                        hist(unlist(lapply(IntaRNA_Data[,1:ncol(IntaRNA_Data)], min, na.rm = TRUE)), xlab = "Minimum", main = NULL)

                    }
                    
                    dev.off()
                  }
                )
                
              }
              
            }
            
          })
        

      }
      
      #Alternatively, if coming back from one of the other analysis views, respond to pressing of the 'Summary View' button
      {
        observeEvent(input$Summary_View, {
          
          #Update button appearance
          {
            updateButton(session, "Summary_View", 
                         label = "Summary View", 
                         icon = icon("fingerprint"),
                         size = "large",
                         style = "primary")
            
            updateButton(session, "miRNA_View", 
                         label = "miRNA View", 
                         icon = icon("chevron-right"),
                         size = "large",
                         style = "default")
            
            updateButton(session, "Pathway_View", 
                         label = "Pathway View", 
                         icon = icon("border-none"),
                         size = "large",
                         style = "default")
            
            updateButton(session, "Compare_Species", 
                         label = "Compare Species", 
                         icon = icon("comments"),
                         size = "large",
                         style = "default")
            
            updateButton(session, "Structural_Analysis", 
                         label = "Structural Analysis", 
                         icon = icon("dna"),
                         size = "large",
                         style = "default")
            
            
          }
          
          #Show/hide analysis windows
          {
            shinyjs::show(id = "Summary_Row_1")
            shinyjs::show(id = "Summary_Row_2")
            shinyjs::show(id = "Summary_Row_3")
            shinyjs::show(id = "Summary_Row_4")
            shinyjs::hide(id = "Starting_Message")
            shinyjs::hide(id = "miRNA_Row_1")
            shinyjs::hide(id = "miRNA_Row_2")
            shinyjs::hide(id = "Pathway_Row_1")
            shinyjs::hide(id = "Pathway_Row_2")
            shinyjs::hide(id = "Comparison_Row_1")
            shinyjs::hide(id = "Comparison_Row_2")
            shinyjs::hide(id = "Structure_Row_1")
            shinyjs::hide(id = "Structure_Row_2")
            shinyjs::hide(id = "Structure_Row_3")
          }
          
          
        })
      }
      
    }
      
    })
    
    #C. miRNA View
    {
      observeEvent(input$miRNA_View, {
        
        #Update button appearance
        {
          updateButton(session, "miRNA_View", 
                       label = "miRNA View", 
                       icon = icon("chevron-right"),
                       size = "large",
                       style = "primary")
          
          updateButton(session, "Summary_View", 
                       label = "Summary View", 
                       icon = icon("fingerprint"),
                       size = "large",
                       style = "default")
          
          updateButton(session, "Pathway_View", 
                       label = "Pathway View", 
                       icon = icon("border-none"),
                       size = "large",
                       style = "default")
          
          updateButton(session, "Compare_Species", 
                       label = "Compare Species", 
                       icon = icon("comments"),
                       size = "large",
                       style = "default")
          
          updateButton(session, "Structural_Analysis", 
                       label = "Structural Analysis", 
                       icon = icon("dna"),
                       size = "large",
                       style = "default")
          
        }
        
        IntaRNA_Data <- Target_Prediction_Data[[which(names(Target_Prediction_Data) == input$Select_Chosen_Species_2)]][,c(1:630)]
        
        #Update drop-down menus
        updateSelectizeInput(session, inputId = "Select_miRNA", "Choose your miRNA of interest", choices = c(colnames(IntaRNA_Data)))
        
        
        #Show/hide analysis windows
        {
          shinyjs::show(id = "miRNA_Row_1")
          shinyjs::show(id = "miRNA_Row_2")
          shinyjs::hide(id = "Summary_Row_1")
          shinyjs::hide(id = "Summary_Row_2")
          shinyjs::hide(id = "Summary_Row_3")
          shinyjs::hide(id = "Summary_Row_4")
          shinyjs::hide(id = "Starting_Message")
          shinyjs::hide(id = "Pathway_Row_1")
          shinyjs::hide(id = "Pathway_Row_2")
          shinyjs::hide(id = "Comparison_Row_1")
          shinyjs::hide(id = "Comparison_Row_2")
          shinyjs::hide(id = "Structure_Row_1")
          shinyjs::hide(id = "Structure_Row_2")
          shinyjs::hide(id = "Structure_Row_3")
        }
        
        #Show miRNA View Explainer
        {
          observeEvent(input$miRNA_View_Explainer, {
            showModal(modalDialog(
              title = "miRNA View Explainer",
              "In the miRNA view, you can choose a miRNA of interest and see all of the pathways that it targets, ranked by effectiveness.",
              br(),
              br(),
              "Each pathway is given a targeting score that can be read as: ‘X% of genes on the pathway are effectively targeted by the selected miRNA’. You can read more about the targeting score and the targeting cutoffs in the ‘Targeting Cutoff Explainer’ below.",
              br(),
              br(),
              "Each pathway is also linked - you can follow this link to the KEGG database, where the KEGG color mapper tool has been set up via an API to highlight all of the genes that are effectively targeted on the linked pathway."
            ))
          })
        }
        
        #Show target cutoff explainer
        {
          observeEvent(input$Targeting_Cutoff_Explainer_1, {
            showModal(modalDialog(
              title = "Targeting Cutoff Explainer",
              "For each KEGG pathway (in the ‘miRNA View’ and ‘Compare Species’ View), the assigned score indicates the percentage of genes on the KEGG pathway that have been targeted by the chosen miRNA at a selected cutoff value for the IntaRNA prediction score.",
              br(),
              br(),
              "Likewise, for each miRNA (in the ‘Pathway View’), the assigned score indicates the percentage of the genes on the chosen KEGG pathway that meet the selected cutoff.",
              br(),
              br(),
              "The energy cutoffs (Strong: -16.98, Medium: -15.35, Weak, -13.47) in the application are set based on real data from prior experimental work.",
              br(),
              br(),
              "It was determined that Pseudomonas aeruginosa cells expressing miRNA let-7b-5p significantly down-regulated a set of 48 proteins, with the targeted proteins having a distribution of predicted IntaRNA energy scores.",
              br(),
              br(),
              "The strong cutoff in the application corresponds to the first quartile value in this distribution, the medium cutoff to the median, and the ‘weak’ cutoff to the third quartile value.",
              br(),
              br(),
              "We observed some proteins with predicted energy scores less negative than the ‘weak’ cutoff to be down-regulated significantly – meaning that the weak cutoff is legitimate to use, though one would have less confidence in the likelihood of protein targeting than they would if using the ‘medium’ and ‘strong’ cutoffs."
            ))
          })
        }
        
        #Show KEGG pathway image explainer
        {
          observeEvent(input$KEGG_Pathway_Image_Explainer, {
            showModal(modalDialog(
              title = "KEGG Pathway Image Explainer",
              "The KEGG pathway images that come up when the links in the Top Targeting miRNAs table are clicked will show each gene that is effectively targeted highlighted in blue.",
              br(),
              br(),
              "If the Rocket-miR application indicates that 100% of genes on a pathway are effectively targeted, this means that 100% of the genes in the selected species that are known to reside on that KEGG pathway are targeted.",
              br(),
              br(),
              "KEGG pathways are designed to apply to many different species at once - drawing on the principle of gene homology across species. However, any given species will only have a certain number of genes associated with the gene entities of the KEGG pathway - it many not have homologs for all of them.",
              br(),
              br(),
              "Therefore, if 100% of the genes of the pathway are predicted to be effectively targeted by Rocket-miR, yet many of the gene entities on the KEGG pathway are not highlighted, this is because most of the genes on a pathway do not have identified homologs in the selected species."
            ))
          })
        }
        
        #User selects a species to visualize data for
        observeEvent(input$Select_Chosen_Species_2, {
          
          
          #User selects miRNA to visualize top targeted pathways
          observeEvent(input$Select_miRNA, {
            
            #Strong targeting cutoff set by default, but user can select a different cutoff if they wish
            if (input$Select_miRNA != "[Select a miRNA]" & input$Select_miRNA != "") {
              
              observeEvent(input$Select_Targeting_Cutoff_1, {
                
                #Now use targeting cutoff input to select the matrices (one for each pathway) of strongly targeted genes for the specific species and cutoff chosen
                miRNA_Data <- Target_Cutoff_List[[which(names(Target_Cutoff_List) == input$Select_Chosen_Species_2)]]
                miRNA_Data <- miRNA_Data[[which(names(miRNA_Data) == input$Select_Targeting_Cutoff_1)]]
                
                #miRNA_Data2 <- miRNA_Data
                
                #for (i in 1:length(miRNA_Data2)) { #Filter out duplicate genes with more than one Uniprot ID
                #miRNA_Data2[[i]] <- unique(miRNA_Data2[[i]][,c(1:631)])
                #} 
                
                #For all of the pathways, calculate the column sum of the chosen miRNA (will be a column in each matrix) and store in a vector
                miRNA_Vec1 <- c()
                for (i in 1:length(miRNA_Data)) {
                  Col <- miRNA_Data[[i]][[which(colnames(miRNA_Data[[i]]) == input$Select_miRNA)]]
                  miRNA_Vec1[i] <- sum(Col)
                }
                
                Pathway_Lengths <- KEGG_Gene_Lists[[which(names(KEGG_Gene_Lists) == input$Select_Chosen_Species_3)]]
                
                #Divide the column sums by the number of genes in each pathway and store the output in a vector
                miRNA_Vec2 <- c()
                for (i in 1:length(miRNA_Vec1)) {
                  miRNA_Vec2[i] <- miRNA_Vec1[i] / length(KEGG_Gene_Lists[[input$Select_Chosen_Species_2]][[i]])
                  #length(miRNA_Data[[i]][[which(colnames(miRNA_Data[[i]]) == input$Select_miRNA)]]) 
                }
                
                
                #Use the vector of column sums / # pathway genes + add to a data frame with the names of all the chosen pathways
                miRNA_DF <- data.frame(names(miRNA_Data), miRNA_Vec2)
                colnames(miRNA_DF) <- c("KEGG Pathway", "% Genes Effectively Targeted")
                miRNA_DF[,2] <- miRNA_DF[,2] * 100 #To look more like percentages
                miRNA_DF[,2] <- round(miRNA_DF[,2], digits = 1)
                
                miRNA_Output <- miRNA_DF
                
                #Download Outputs
                {
                  #Top miRNAs
                  output$miRNA_3 <- downloadHandler(
                    filename = function() {
                      paste(input$Select_Chosen_Species_2, "_Best_Targeting_miRNAs", ".csv", sep = "")
                    },
                    content = function(file) {
                      write.csv(miRNA_Output, file, row.names = TRUE)
                    }
                  )
                }
                
                #Create links to KEGG Color Mapper so user can visualize what genes are strongly targeted
                {
                  URL_multi_query_sep <- "%0d%0a"
                  URL_query_sep <- "?"
                  URL_base <- "https://www.kegg.jp/kegg-bin/show_pathway"
                  No_Color ="&nocolor=1"
                  URL_query_args_chars <- "&multi_query="
                  Target_Color <- "blue"
                  URL_multi_query_sep <- "%0d%0a"
                  
                  KeggMapperURL <- function(Pathway_name){
                    
                    Pathway_Table <- miRNA_Data[[Pathway_name]]
                    T1 <- Pathway_Table[,c(input$Select_miRNA, "KEGG_ID")]
                    T2 <- T1[which(T1[,1] == 1),]
                    Targeted_Genes <- T2$KEGG_ID
                    
                    URL_Targeted <- paste(paste(Targeted_Genes, Target_Color, sep ='+'),
                                          collapse = URL_multi_query_sep)
                    
                    #ID <- "afm00010"
                    ID <- Pathway_Name_Matching[[input$Select_Species]][grep(Pathway_name, Pathway_Name_Matching[[input$Select_Species]][,1]),2][1] 
                    #Note: For a very small number of pathways, there are two separate KEGG IDs (two separate 'autophagy' pathways, for example - so we want to select just the first one)
                    
                    URL_map_arg <- paste("map", ID, sep = '=')
                    
                    paste(URL_base,
                          URL_query_sep,
                          URL_map_arg,
                          URL_query_args_chars,
                          paste(URL_Targeted, collapse = URL_multi_query_sep), 
                          No_Color, sep = '')
                    
                  }
                  
                  myLinks <- lapply(miRNA_DF[,1], KeggMapperURL)
                  link_text_to_show <- miRNA_DF[,1]
                  contents = paste('<a href="', myLinks,'" target="_blank">', link_text_to_show, '</a>', collapse="</br>")
                  
                  your.html = paste('<!DOCTYPE html><html><head></head><body>', contents, '</body></html>', sep="")
                  
                  fileConn<-file("Pathways.html")
                  writeLines(your.html, fileConn)
                  close(fileConn)
                  
                  Links_df <- data.frame("Link" = unlist(myLinks),
                                         "Name" = miRNA_DF[,1])
                  
                  miRNA_DF[,1] <- apply(Links_df, 1, function(x){
                    paste('<a href="', x[1],'" target="_blank">', 
                          x[2], '</a>', collapse="</br>")
                  })
                }
                
                #Now present the resulting table in the application
                output$Top_miRNAs_2 <- DT::renderDataTable(
                  {
                    shiny::isolate(miRNA_DF)
                  },
                  escape = F,
                  rownames = TRUE,
                  options = list(processing = FALSE, order = list(2, 'desc')), 
                  
                )  
                
                
              })
              
            }
            
            
          })
          
          
        })
        
        
      })
      
    }
    
    #D. Pathway View
    {
      
      observeEvent(input$Pathway_View, {
        
        #Update button appearance
        {
          updateButton(session, "Pathway_View", 
                       label = "Pathway View", 
                       icon = icon("border-none"),
                       size = "large",
                       style = "primary")
          
          updateButton(session, "Summary_View", 
                       label = "Summary View", 
                       icon = icon("fingerprint"),
                       size = "large",
                       style = "default")
          
          updateButton(session, "miRNA_View", 
                       label = "miRNA View", 
                       icon = icon("chevron-right"),
                       size = "large",
                       style = "default")
          
          updateButton(session, "Compare_Species", 
                       label = "Compare Species", 
                       icon = icon("comments"),
                       size = "large",
                       style = "default")
          
          updateButton(session, "Structural_Analysis", 
                       label = "Structural Analysis", 
                       icon = icon("dna"),
                       size = "large",
                       style = "default")
          
        }
        
        #Show/hide analysis windows
        {
          shinyjs::show(id = "Pathway_Row_1")
          shinyjs::show(id = "Pathway_Row_2")
          shinyjs::hide(id = "Summary_Row_1")
          shinyjs::hide(id = "Summary_Row_2")
          shinyjs::hide(id = "Summary_Row_3")
          shinyjs::hide(id = "Summary_Row_4")
          shinyjs::hide(id = "Starting_Message")
          shinyjs::hide(id = "miRNA_Row_1")
          shinyjs::hide(id = "miRNA_Row_2")
          shinyjs::hide(id = "Comparison_Row_1")
          shinyjs::hide(id = "Comparison_Row_2")
          shinyjs::hide(id = "Structure_Row_1")
          shinyjs::hide(id = "Structure_Row_2")
          shinyjs::hide(id = "Structure_Row_3")
        }
        
        #Show Pathway View Explainer
        {
          observeEvent(input$Pathway_View_Explainer, {
            showModal(modalDialog(
              title = "Pathway View Explainer",
              "If you have a biological pathway of interest and want to see what miRNAs target it most effectively, you can use the ‘Pathway View’",
              br(),
              br(),
              "Here, each miRNA is associated with a targeting score that can be read as: ‘X% of genes on the selected pathway are effectively targeted by this miRNA’. You can read more about the targeting score and the targeting cutoffs in the ‘Targeting Cutoff Explainer’ below."
            ))
          })
        }
        
        #Show target cutoff explainer
        {
          observeEvent(input$Targeting_Cutoff_Explainer_2, {
            showModal(modalDialog(
              title = "Targeting Cutoff Explainer",
              "For each KEGG pathway (in the ‘miRNA View’ and ‘Compare Species’ View), the assigned score indicates the percentage of genes on the KEGG pathway that have been targeted by the chosen miRNA at a selected cutoff value for the IntaRNA prediction score.",
              br(),
              br(),
              "Likewise, for each miRNA (in the ‘Pathway View’), the assigned score indicates the percentage of the genes on the chosen KEGG pathway that meet the selected cutoff.",
              br(),
              br(),
              "The energy cutoffs (Strong: -16.98, Medium: -15.35, Weak, -13.47) in the application are set based on real data from prior experimental work.",
              br(),
              br(),
              "It was determined that Pseudomonas aeruginosa cells expressing miRNA let-7b-5p significantly down-regulated a set of 48 proteins, with the targeted proteins having a distribution of predicted IntaRNA energy scores.",
              br(),
              br(),
              "The strong cutoff in the application corresponds to the first quartile value in this distribution, the medium cutoff to the median, and the ‘weak’ cutoff to the third quartile value.",
              br(),
              br(),
              "We observed some proteins with predicted energy scores less negative than the ‘weak’ cutoff to be down-regulated significantly – meaning that the weak cutoff is legitimate to use, though one would have less confidence in the likelihood of protein targeting than they would if using the ‘medium’ and ‘strong’ cutoffs."
            ))
          })
        }
        
        #User selects a species to visualize data for
        observeEvent(input$Select_Chosen_Species_3, {
          
          #Update drop-down menus
          updateSelectizeInput(session, inputId = "Select_Pathway", "Choose your pathway of interest", choices = names(KEGG_Gene_Lists[[which(names(KEGG_Gene_Lists) == input$Select_Chosen_Species_3)]]))
          
          IntaRNA_Data <- Target_Prediction_Data[[which(names(Target_Prediction_Data) == input$Select_Chosen_Species_3)]][,c(1:630)]
          
          #User selects miRNA to visualize top targeted pathways
          
          observeEvent(input$Select_Pathway, {
            
            if (input$Select_Pathway != "[Select a Pathway]" & input$Select_Pathway != "") {
              
              #Strong targeting cutoff set by default, but user can select a different cutoff if they wish
              observeEvent(input$Select_Targeting_Cutoff_2, {
                
                #Now use targeting cutoff input to select the matrices (one for each pathway) of strongly targeted genes for the specific species and cutoff chosen
                Pathway_Data <- Target_Cutoff_List[[which(names(Target_Cutoff_List) == input$Select_Chosen_Species_3)]]
                Pathway_Data <- Pathway_Data[[which(names(Pathway_Data) == input$Select_Targeting_Cutoff_2)]]
                Pathway_Data <- Pathway_Data[[which(names(Pathway_Data) == input$Select_Pathway)]] 
                #Pathway_Data <- unique(Pathway_Data[[which(names(Pathway_Data) == input$Select_Pathway)]][,c(1:631)]) #Filter out duplicate genes with more than one Uniprot ID
                
                #Now pick out the full pathway lengths from the KEGG_Gene_Lists object
                Pathway_Lengths <- KEGG_Gene_Lists[[which(names(KEGG_Gene_Lists) == input$Select_Chosen_Species_3)]]
                
                Selected_Length <- length(Pathway_Lengths[[which(names(Pathway_Lengths) == input$Select_Pathway)]])
                
                #Display pathway length
                output$Pathway_Length <- renderText({ 
                  paste("The pathway length is:", Selected_Length)
                })
                
                #For the chosen pathway, calculate the column sum for each of the individual miRNAs and divide the column sums by the number of genes in the chosen pathway
                Pathway_Info <- apply(Pathway_Data[,c(2:631)], 2, sum) / length(Pathway_Lengths[[which(names(Pathway_Lengths) == input$Select_Pathway)]])
                
                
                #Use the vector of column sums / number of genes in the chosen pathway + add to a data frame with the names of all the miRNAs
                Pathway_DF <- data.frame(unlist(Pathway_Info))
                colnames(Pathway_DF) <- c("% Genes Effectively Targeted")
                Pathway_DF[,1] <- Pathway_DF[,1] * 100 #To look more like percentages
                Pathway_DF[,1] <- round(Pathway_DF[,1], digits = 1)
                
                output$Top_Pathways <- DT::renderDataTable(
                  {
                    shiny::isolate(Pathway_DF)
                  },
                  server = FALSE,
                  escape = F,
                  rownames = TRUE,
                  options = list(processing = FALSE, order = list(1, 'desc')), 
                )  
                
                #Download Outputs
                {
                  #Top Pathways
                  output$Pathway_3 <- downloadHandler(
                    filename = function() {
                      paste(input$Select_Chosen_Species_3, "_Top_Targeted_Pathways", ".csv", sep = "")
                    },
                    content = function(file) {
                      write.csv(Pathway_DF, file, row.names = TRUE)
                    }
                  )
                }
                
              })
              
            }
            
          })
          
          
          
        })
        
      })
      
    }
    
    #E. Compare Species
    {
      observeEvent(input$Compare_Species, {
        
        #Update button appearance
        {
          updateButton(session, "Compare_Species", 
                       label = "Compare Species", 
                       icon = icon("comments"),
                       size = "large",
                       style = "primary")
          
          updateButton(session, "Pathway_View", 
                       label = "Pathway View", 
                       icon = icon("border-none"),
                       size = "large",
                       style = "default")
          
          updateButton(session, "Summary_View", 
                       label = "Summary View", 
                       icon = icon("fingerprint"),
                       size = "large",
                       style = "default")
          
          updateButton(session, "miRNA_View", 
                       label = "miRNA View", 
                       icon = icon("chevron-right"),
                       size = "large",
                       style = "default")
          
          updateButton(session, "Structural_Analysis", 
                       label = "Structural Analysis", 
                       icon = icon("dna"),
                       size = "large",
                       style = "default")
        }
        
        #Update drop-down menus
        updateSelectizeInput(session, inputId = "Select_miRNA_2", "Choose your miRNA of interest", choices = c(colnames(IntaRNA_Data)))
        
        #Show/hide analysis windows
        {
          shinyjs::show(id = "Comparison_Row_1")
          shinyjs::show(id = "Comparison_Row_2")
          shinyjs::hide(id = "Summary_Row_1")
          shinyjs::hide(id = "Summary_Row_2")
          shinyjs::hide(id = "Summary_Row_3")
          shinyjs::hide(id = "Summary_Row_4")
          shinyjs::hide(id = "Starting_Message")
          shinyjs::hide(id = "miRNA_Row_1")
          shinyjs::hide(id = "miRNA_Row_2")
          shinyjs::hide(id = "Pathway_Row_1")
          shinyjs::hide(id = "Pathway_Row_2")
          shinyjs::hide(id = "Structure_Row_1")
          shinyjs::hide(id = "Structure_Row_2")
          shinyjs::hide(id = "Structure_Row_3")
        }
        
        #Show Compare Species Explainer
        {
          observeEvent(input$Compare_Species_Explainer, {
            showModal(modalDialog(
              title = "Compare Species Explainer",
              "You may be more interested in a community of microbes than an individual microbe. The Compare Species view allows you to look at a whole community of pathogens at the same time - and explore how individual miRNAs target certain species in the community more or less effectively.",
              br(),
              br(),
              "To get at this question, you can select a set of species, a miRNA, and a KEGG pathway that is shared by all the selected species.",
              br(),
              br(),
              "When you make these selections, the panel will show the percent of proteins effectively targeted for each species in the form of a health bar (a shorter bar means that more proteins are targeted)."
            ))
          })
        }
        
        #Show targeting cutoff explainer
        {
          observeEvent(input$Targeting_Cutoff_Explainer_3, {
            showModal(modalDialog(
              title = "Targeting Cutoff Explainer",
              "For each KEGG pathway (in the ‘miRNA View’ and ‘Compare Species’ View), the assigned score indicates the percentage of genes on the KEGG pathway that have been targeted by the chosen miRNA at a selected cutoff value for the IntaRNA prediction score.",
              br(),
              br(),
              "Likewise, for each miRNA (in the ‘Pathway View’), the assigned score indicates the percentage of the genes on the chosen KEGG pathway that meet the selected cutoff.",
              br(),
              br(),
              "The energy cutoffs (Strong: -16.98, Medium: -15.35, Weak, -13.47) in the application are set based on real data from prior experimental work.",
              br(),
              br(),
              "It was determined that Pseudomonas aeruginosa cells expressing miRNA let-7b-5p significantly down-regulated a set of 48 proteins, with the targeted proteins having a distribution of predicted IntaRNA energy scores.",
              br(),
              br(),
              "The strong cutoff in the application corresponds to the first quartile value in this distribution, the medium cutoff to the median, and the ‘weak’ cutoff to the third quartile value.",
              br(),
              br(),
              "We observed some proteins with predicted energy scores less negative than the ‘weak’ cutoff to be down-regulated significantly – meaning that the weak cutoff is legitimate to use, though one would have less confidence in the likelihood of protein targeting than they would if using the ‘medium’ and ‘strong’ cutoffs."
            ))
          })
        }
        
        observeEvent(input$Select_Chosen_Species_4, {
          
          #First figure out what pathways are shared between the selected species
          {
            Species_Set <- Target_Cutoff_List[names(Target_Cutoff_List) %in% input$Select_Chosen_Species_4]
            
            Paths_List <- vector(mode = "list", length = length(Species_Set))
            for (i in 1:length(Species_Set)) {
              Paths_List[[i]] <- names(Species_Set[[i]][[1]])
            }
            
            Shared_Paths_List <- Reduce(intersect, Paths_List) 
            
            updateSelectizeInput(session, inputId = "Select_Pathway_2", "Select a pathway of interest to view analysis outputs", choices = Shared_Paths_List, selected = Shared_Paths_List[1])
            
          }
          
          #Now create the 'Community Health Figure'
          {
            #User selects pathway to create figure
            observeEvent(input$Select_Pathway_2, {
              
              if (input$Select_Pathway_2 != "[Select a set of species to view shared pathways]" & input$Select_Pathway_2 != "") {
                
                #First filter data by targeting cutoff, selected miRNA, and chosen pathway
                {
                  observeEvent(input$Select_Targeting_Cutoff_3, {
                    
                    observeEvent(input$Select_miRNA_2, {
                      
                      if (input$Select_miRNA_2 != "[Select a Pathway]" & input$Select_miRNA_2 != "") {
                        
                        Species_Health_Data <- vector(mode = "list", length = length(Species_Set))
                        
                        for (i in 1:length(Species_Health_Data)) {
                          Species_Health_Data[[i]] <- Species_Set[[i]][[which(names(Species_Set[[i]]) == input$Select_Targeting_Cutoff_3)]] 
                        }
                        
                        Species_Health_Scores <- vector(mode = "list", length = length(Species_Health_Data))
                        
                        Species_Setlist <- KEGG_Gene_Lists[names(KEGG_Gene_Lists) %in% names(Species_Set)]
                        
                        for (i in 1:length(Species_Health_Scores)) {
                          
                          miRNA_Vec1 <- c()
                          for (j in 1:length(Species_Health_Data[[i]])) {
                            Col <- Species_Health_Data[[i]][[j]][[which(colnames(Species_Health_Data[[i]][[j]]) == input$Select_miRNA_2)]] 
                            #Col <- unique(Species_Health_Data[[i]][[j]][,c(1:631)])[[which(colnames(Species_Health_Data[[i]][[j]]) == input$Select_miRNA_2)]] #Filter out duplicate genes with more than one Uniprot ID
                            miRNA_Vec1[j] <- sum(Col)
                          }
                          
                          miRNA_Vec2 <- c()
                          for (k in 1:length(miRNA_Vec1)) {
                            miRNA_Vec2[k] <- miRNA_Vec1[k] / length(Species_Setlist[[i]][[k]])
                            #length(Species_Health_Data[[i]][[k]][[which(colnames(Species_Health_Data[[i]][[k]]) == input$Select_miRNA_2)]])
                          }
                          
                          Species_Health_Scores[[i]] <- data.frame(miRNA_Vec2, names(Species_Health_Data[[i]]))
                          names(Species_Health_Scores[[i]]) <- c("Targeting Score", "Pathway")
                          
                        }
                        
                        if (length(Species_Health_Scores) == length(input$Select_Chosen_Species_4)) {
                          names(Species_Health_Scores) <- input$Select_Chosen_Species_4
                        }
                        
                        
                        #Now filter by pathway
                        {
                          Species_Health_Scores_Filtered_X <- c()
                          
                          for (i in 1:length(Species_Health_Scores)) {
                            
                            Species_Health_Scores_Filtered_X[i] <- Species_Health_Scores[[i]][Species_Health_Scores[[i]]$Pathway == input$Select_Pathway_2,][[1]]
                          }
                          
                          
                        }
                        
                        #Now build the components of the figure with plotly
                        {
                          X <- names(Species_Set)
                          Y <- 1 - Species_Health_Scores_Filtered_X
                          Y = Y * 100 #To look more like percentages
                          Y <- round(Y, digits = 1)
                          
                          Data <- data.frame(X, Y)
                          
                          Community_Health_Figure <- plot_ly(Data, x = ~X, y = ~Y, type = 'bar',
                                                             text = Y, textposition = 'auto') %>% 
                            layout(title = 'Community Health', plot_bgcolor = "#e5ecf6", xaxis = list(title = 'Selected Microbial Species'), 
                                   yaxis = list(title = 'Species Health %', range = list(0,100)))
                          
                          output$Community_Health_Plot <- renderPlotly({
                            
                            Community_Health_Figure
                            
                          })
                          
                          #Download Outputs
                          {
                            #Top miRNAs
                            output$Summary_2 <- downloadHandler(
                              filename = function() {
                                paste(input$Select_Chosen_Species_2, "_Best_Targeting_miRNAs", ".csv", sep = "")
                              },
                              content = function(file) {
                                write.csv(miRNA_DF, file, row.names = TRUE)
                              }
                            )
                          }
                        }
                        
                      }
                      
                    })
                    
                    
                    
                  })
                }
                
                
              }
              
              
            })
            
            
            
            
          }
          
          
        })
        
      })
      
    }
    
    #F. Structural Analysis
    {
      observeEvent(input$Structural_Analysis, {
        
        #Update button appearance
        {
          updateButton(session, "Structural_Analysis", 
                       label = "Structural Analysis", 
                       icon = icon("dna"),
                       size = "large",
                       style = "primary")
          
          updateButton(session, "Compare_Species", 
                       label = "Compare Species", 
                       icon = icon("comments"),
                       size = "large",
                       style = "default")
          
          updateButton(session, "Pathway_View", 
                       label = "Pathway View", 
                       icon = icon("border-none"),
                       size = "large",
                       style = "default")
          
          updateButton(session, "Summary_View", 
                       label = "Summary View", 
                       icon = icon("fingerprint"),
                       size = "large",
                       style = "default")
          
          updateButton(session, "miRNA_View", 
                       label = "miRNA View", 
                       icon = icon("chevron-right"),
                       size = "large",
                       style = "default")
          
        }
        
        #Update drop-down menus
        updateSelectizeInput(session, inputId = "Select_miRNA_3", "Choose your miRNA of interest", choices = c(colnames(IntaRNA_Data)))
        
        #Show/hide analysis windows
        {
          shinyjs::show(id = "Structure_Row_1")
          shinyjs::show(id = "Structure_Row_2")
          shinyjs::show(id = "Structure_Row_3")
          shinyjs::hide(id = "Summary_Row_1")
          shinyjs::hide(id = "Summary_Row_2")
          shinyjs::hide(id = "Summary_Row_3")
          shinyjs::hide(id = "Summary_Row_4")
          shinyjs::hide(id = "Starting_Message")
          shinyjs::hide(id = "miRNA_Row_1")
          shinyjs::hide(id = "miRNA_Row_2")
          shinyjs::hide(id = "Pathway_Row_1")
          shinyjs::hide(id = "Pathway_Row_2")
          shinyjs::hide(id = "Comparison_Row_1")
          shinyjs::hide(id = "Comparison_Row_2")
        }
        
        #Show Structural Analysis Explainer
        {
          observeEvent(input$Structural_Analysis_Explainer, {
            showModal(modalDialog(
              title = "Structural Analysis Explainer",
              "The ‘Structural Analysis view’ allows you to explore sets of miRNAs and see what structural similarities these miRNAs share.",
              br(),
              br(),
              "For example, you could examine a set of miRNAs that you found in the other analysis views to be most effective at targeting a pathway in a given species or group of species and see if they share common structural motifs."
            ))
          })
        }
        
        #Show kmer counting explainer
        {
          observeEvent(input$Kmer_Counting_Explainer, {
            showModal(modalDialog(
              title = "Kmer Counting Explainer",
              "The k-mer counting algorithm - embedded in the ‘kmer’ R package - allows one to gauge the abundance of short sequence motifs in a set of miRNAs.",
              br(),
              br(),
              "A ‘k-mer’ refers to a series of k nucleotides (k can be 3,4,5,10, or any integer greater than one) in a specific order - and the k-mer algorithm figures out how many times each possible k-mer appears throughout a set of sequences.",
              br(),
              br(),
              "For example, say k were equal to 3. Because RNAs have four possible nucleotides (A,U,G, and C) there are 4^3 possible 3-mers, including AAA, AAT, AAU, AAG, ATT, and so on. The k-mer algorithm figure out how often each individual 3-mer appears in the set of miRNA sequences. Each 3-mer may appear once or more than once in some miRNAs, and it may not appear at all in others.",
              br(),
              br(),
              "For reference, see:",
              br(),
              br(),
              "Kmer R package: https://www.rdocumentation.org/packages/kmer/versions/1.1.2"
            ))
          })
        }
        
        observeEvent(input$Select_miRNA_3, {
          
          #First, update table to show selected miRNAs + Sequence information
          {
            miRNA_Sequence_DF <- miRNA_Sequence_Data[which(miRNA_Sequence_Data$miRNA_Names %in% input$Select_miRNA_3),]
            
            output$miRNA_Sequences <- DT::renderDataTable(
              {
                shiny::isolate(miRNA_Sequence_DF)
              },
              escape = F,
              rownames = TRUE,
              options = list(processing = FALSE), 
            )
          }
          
          if (nrow(miRNA_Sequence_DF) != 0) {
            #Now take the chosen miRNAs and split them up into individual nucleotides
            {
              
              
              
              Nucleotide_DF <- data.frame(matrix(ncol = 40, nrow = length(input$Select_miRNA_3))) 
              
              rownames(Nucleotide_DF) <- input$Select_miRNA_3
              
              Sequence_Lengths <- c()
              for (i in 1:length(input$Select_miRNA_3)) {
                Sequence_Lengths[i] <- nchar(input$Select_miRNA_3[i])
              }
              
              for (j in 1:nrow(miRNA_Sequence_DF)) {
                vec <- c()
                for (i in 1:nchar(miRNA_Sequence_DF[,2][j])) {
                  vec[i] <- substr(miRNA_Sequence_DF[,2][j],i,i)
                }
                vec[(length(vec)+1):40] <- 0
                Nucleotide_DF[j,] <- vec
              }
              
              Nucleotide_DF <- Nucleotide_DF[,c(1:max(Sequence_Lengths))]
              
              
              
            }
            
            #Now update the miRNA sequence clustering dendrogram
            {
              
              Nucleotide_Tree <- cluster(as.matrix(Nucleotide_DF), nstart = 5)
              
              output$miRNA_Clustering <- renderPlot({
                par(mar = c(2,2,2,10))
                plot(Nucleotide_Tree, horiz = TRUE)
                height = "auto"
                width = "auto"
              })
              
              
            }
            
            #Download Outputs
            {
              #miRNA Sequence Details
              output$Structure_2 <- downloadHandler(
                filename = function() {
                  paste("miRNA_Sequences", ".csv", sep = "")
                },
                content = function(file) {
                  write.csv(miRNA_Sequence_DF, file, row.names = TRUE)
                }
              )
              
              #miRNA Dendrogram
              output$Structure_3 <- downloadHandler(
                filename = function() {
                  paste("miRNA_Dendrogram", ".png", sep = "")
                },
                content = function(file) {
                  png(file)
                  
                  par(mar = c(2,2,2,10))
                  plot(Nucleotide_Tree, horiz = TRUE)
                  height = "auto"
                  width = "auto"
                  
                  dev.off()
                }
              )
              
            }
            
            #Now update the miRNA targeting motif bar graph
            {
              
              observeEvent(input$Select_k_value, {
                
                Cluster_kmers <- kcount(as.matrix(Nucleotide_DF), k = as.numeric(input$Select_k_value), residues = "RNA", gap = "-", named = TRUE, compress = TRUE, encode = FALSE)
                Counts_kmers <- as.data.frame(apply(Cluster_kmers, 2, sum))
                Counts_kmers$Sequence_Motifs <- rownames(Counts_kmers)
                colnames(Counts_kmers) <- c("Frequency", "Sequence_Motifs")
                Counts_kmers_ordered <- Counts_kmers[order(-Counts_kmers$Frequency),]
                
                A <- Counts_kmers_ordered[c(1:20),2]
                B <- Counts_kmers_ordered[c(1:20),1]
                
                Kmer_Data <- data.frame(A, B)
                
                Kmer_Analysis_Plot <- plot_ly(Kmer_Data, x = ~B, y = ~reorder(A,-B), type = 'bar', orientation = "h")
                
                output$Kmer_Counting_Output <- renderPlotly({
                  
                  Kmer_Analysis_Plot
                  
                })
                
              })
              
              
            }
            
          }  
          
          
        })
        
      })
    }
    
  }
}

#Additional Functions

shinyApp(ui = ui, server = server)
