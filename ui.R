#UI  
shinyUI(
  navbarPage("NEON Field-Based Organismal Data ",
             tabPanel("Home", 
                      tabsetPanel(
                        tabPanel("Home",
                                 fluidPage(
                                   titlePanel("Field-Based Organismal Data Application"),
                                   sidebarLayout(
                                     sidebarPanel(width=2,
                                                  img(src="NEON-Logo.png", height = 50, width=150),
                                                  br(),
                                                  br(),
                                                  p("This application uses the National Ecological Observatory Network Data"), 
                                                  br(),
                                                  p("In here you will be able to make use of different data visualization techniques designed in the 2016 NEON Internship.")
                                     ), 
                                     mainPanel(
                                       p("In this application you will find tools to analyze the different field-based organismal datasets. 
                                         For the momment we only make use of the following datasets:"),
                                       br(),
                                       strong("- Small Mammals Data"), 
                                       br(), 
                                       br(), 
                                       br(),
                                       img(src="2016_NEONDomainOverview_0.jpg", height = 400, width=800)
                                       )
                                   )
                                 )
                        ), 
                        tabPanel("Map",
                                 
                                   leaflet::leafletOutput("neonmap", height = 600)
                                 
                                 #         fluidPage(htmlOutput("neonmap")
                                 #includeHTML("www/neon_overview.html"))
                        ))), 
             tabPanel("Small Mammals Data Description", 
                      fluidPage(
                        titlePanel("  "),
                        sidebarLayout(
                          sidebarPanel(
                            width=2,
                            img(src="NEON-Logo.png", height = 50, width=150),
                            br(),
                            br(), 
                            p("For more information on the Small Mammals' design you are welcome to visit the following links:"),
                            a(href="http://data.neonscience.org/documents?p_p_id=110_INSTANCE_JEygRkSpUBoq&p_p_lifecycle=0&p_
                              p_state=normal&p_p_mode=view&p_p_col_id=column-1&p_p_col_count=1&_110_INSTANCE_JEygRkSpUBoq_str
                              uts_action=%2Fdocument_library_display%2Fview_file_entry&_110_INSTANCE_JEygRkSpUBoq_redirect=ht
                              tp%3A%2F%2Fdata.neonscience.org%2Fdocuments%2F-%2Fdocument_library_display%2FJEygRkSpUBoq%2Fvie
                              w%2F1723439%3F_110_INSTANCE_JEygRkSpUBoq_redirect%3Dhttp%253A%252F%252Fdata.neonscience.org%252
                              Fdocuments%253Fp_p_id%253D110_INSTANCE_JEygRkSpUBoq%2526p_p_lifecycle%253D0%2526p_p_state%253D
                              normal%2526p_p_mode%253Dview%2526p_p_col_id%253Dcolumn-1%2526p_p_col_count%253D1&_110_INSTANCE_
                              JEygRkSpUBoq_fileEntryId=1723537", "NEON TOS Science Design For Small Mammal Abundance and Diversity"), 
                            br(), 
                            br(),
                            a(href="http://data.neonscience.org/documents?p_p_id=110_INSTANCE_JEygRkSpUBoq&p_p_lifecycle=0&p_
                              p_state=normal&p_p_mode=view&p_p_col_id=column-1&p_p_col_count=1&_110_INSTANCE_JEygRkSpUBoq_str
                              uts_action=%2Fdocument_library_display%2Fview_file_entry&_110_INSTANCE_JEygRkSpUBoq_redirect=&_
                              110_INSTANCE_JEygRkSpUBoq_fileEntryId=17624877", "NEON TOS Protocol and Procedure: Small Mammal Design")
                            ), 
                          mainPanel(
                            p(" All the information provided here was extracted from: "),
                            a(href="http://data.neonscience.org/documents?p_p_id=110_INSTANCE_JEygRkSpUBoq&p_p_lifecycle=0&p_
                              p_state=normal&p_p_mode=view&p_p_col_id=column-1&p_p_col_count=1&_110_INSTANCE_JEygRkSpUBoq_str
                              uts_action=%2Fdocument_library_display%2Fview_file_entry&_110_INSTANCE_JEygRkSpUBoq_redirect=ht
                              tp%3A%2F%2Fdata.neonscience.org%2Fdocuments%2F-%2Fdocument_library_display%2FJEygRkSpUBoq%2Fvie
                              w%2F1723439%3F_110_INSTANCE_JEygRkSpUBoq_redirect%3Dhttp%253A%252F%252Fdata.neonscience.org%252
                              Fdocuments%253Fp_p_id%253D110_INSTANCE_JEygRkSpUBoq%2526p_p_lifecycle%253D0%2526p_p_state%253D
                              normal%2526p_p_mode%253Dview%2526p_p_col_id%253Dcolumn-1%2526p_p_col_count%253D1&_110_INSTANCE_
                              JEygRkSpUBoq_fileEntryId=1723537", "NEON TOS Science Design For Small Mammal Abundance and Diversity"),
                            br(),
                            h2("Introduction to the Small Mammal Sampling Design"),
                            p("The foci of the small mammal sampling design efforts are the dynamics of"),
                            strong(" - Demography, Density, and Disease prevalence"),
                            br(),
                            strong(" - Small Mammal Community Structure & Composition"), 
                            br(),
                            p(" as they relate to climate, productivity, & insect abundance"),
                            
                            ##### Background ####
                            h3("Background Information"),
                            p("Small mammal field studies have played a key role throughout the history and development of the field of 
                              ecology, particularly in the subdisciplines of behavioral, population, and community ecology (Stapp 2010). 
                              Their scientific utility results from a number of important characteristics, including the relative ease 
                              of handling imbued by their small size and trapability. Small mammals are also abundant in virtually all 
                              ecosystems, from harsh deserts to arctic and alpine tundra (Merritt 2010). These characteristics lend this 
                              group to studies of fundamental autecological issues, such as intraspecific interactions and behavior 
                              (e.g., Cameron 1995, Cameron and Spencer 2008, Torregrossa and Dearing 2009), resource selection (e.g., Kelt et al. 2004), 
                              habitat selection (e.g., Stapp and Van Horne 1997), and distribution (e.g., Orrock et al. 2000, Anderson et al. 2002). 
                              In the temperate zones in which they have been most intensively studied, small mammal communities vary in diversity, 
                              from monotypic assemblages to those including more than a dozen species. This variation combined with the relatively low, 
                              and thus scientifically tractable, levels of local richness (compared to insects or birds, for example) have led ecologists 
                              studying small mammals to important insights into patterns of diversity (e.g., McCain 2005), community assembly, and 
                              interspecific interactions (e.g., Bowers and Brown 1982, Meserve et al. 1996, Kelt et al. 1999)."),
                            h4("What is a small mammal?"),
                            p("While there is no established definition of the term ‘small mammal’ (Merritt 2010), it is generally used to refer to small 
                              rodents (voles and mice in the order Rodentia) and insectivores (shrews and moles in the order Soricomorpha), and sometimes 
                              squirrels (Rodentia: Sciuridae) and rabbits (order Lagomorpha), all with body masses less than 120g – 2kg, depending on the 
                              source. Here small mammals are defined based on a combination of behavioral, dietary, and size constraints, as the NEON design 
                              is limited to species sampled by box traps, due to logistical constraints. This definition includes any mammal that is (1) 
                              nonvolant; (2) nocturnally active; (3) forages predominantly aboveground; and (4) is greater than 5 grams but less than 
                              approximately 500 g (one exception to this includes the bushy-tailed woodrat, Neotoma cinerea, males of which can weigh up 
                              to 600 grams). In North America, this includes cricetids, heteromyids, small sciurids, and introduced murids. It does not 
                              include shrews, large squirrels, rabbits, or weasels, despite the fact that individuals of these species may be incidentally captured."),
                            h4("Several Key Small Mammal Data Products"), 
                            p("The data collected by NEON is processed to be produced as Data products. This data products open the door to understand complex 
                              interactions and/or population dynamics. Below, you can find a conceptual map explaining this in more detail."), 
                            img(src="Causal-Inference.png", height=520, width=735),
                            
                            #### Sampling Methods ####
                            h3("Sampling Methods"),
                            p("Small mammal populations can be sampled in a number of ways (Wilson et al. 1996), and the best method depends on study
                              objectives and the species of interest. Since the target species for NEON are nocturnal and cryptic, the most common methods used to sample them are passive, 
                              involving kill- or live-traps, baited or unbaited (e.g., Wilson et al. 1996, Evangelista et al. 2008). For long-term studies of small mammal community, population, 
                              and disease dynamics, mark-recapture methods are the most commonly deployed (e.g., Mills et al. 1999, Douglass et al. 2001, Meserve et al. 2003, Ernest et al. 2009). 
                              A number of analytical methods have been developed to use mark-recapture data to derive estimates of density and diversity, given that many animal species, including small mammals, 
                              cannot be observed perfectly (White and Burnham 1999, Efford et al. 2009, Royle et al. 2009). Moreover, disease studies often employ live-trapping (even for removal studies), 
                              in order to ensure the integrity of the tissue samples (Mills et al. 1995). For these reasons, NEON employs a mark-recapture approach to studying small mammals and mammal-borne disease."),
                            p("Small mammals are typically live-trapped using 3 general trap types: box traps (e.g., Sherman traps, Longworth traps, Elliott traps) 
                              that are placed on the ground or in trees and typically baited; cage traps (e.g. Tomahawk, Havahart traps) that are set like box traps;
                              and pitfall traps, which are buckets buried in the ground and are not intentionally baited (Wilson et al. 1996). Within these categories, 
                              a diversity of sizes is available as well, as no single trap type or size is appropriate for all species. For NEON’s target small mammal 
                              species, box traps are the most commonly used trap type, and Sherman traps (H.B. Sherman Inc., Tallahassee, FL) have been the standard in 
                              mammalogy for decades, particularly in the US. Longworth traps (Penlon Ltd., Oxford, U.K.) are distinguished from Sherman traps by an additional, 
                              padded compartment that provides greater insulation under very cold conditions."),
                            p("NEON employs 10 x 10 trapping grids, with 10 m spacing and 1 trap per station. This design yields an approximate sampling area of 1 ha; 
                              allows for consistent sampling across NEON sites of deermice and voles, the most common reservoirs of the NEON target rodent-borne pathogens; is 
                              logistically desirable to facilitate the setting of as many grids as possible within the time available; and will yield sample sizes of approximately 
                              10 individuals per grid (assuming a mean capture rate of 10%), on average. Below, an image of the traps arrangement."),
                            img(src="Grid-Traps.png", height=581, width=756)
                            
                            )
                            )
                            )
                            ), 
             tabPanel("Mark-Recapture Models")
                            ))
tabPanel("Small Mammals Data",
         tabsetPanel(
           tabPanel("Plots", 
                    fluidPage(
                      titlePanel("Small Mammals' Data Plots"),
                      sidebarLayout(
                        sidebarPanel(width=2,
                                     helpText("Small Mammals Data"),
                                     # uiOutput("indvar"),
                                     # uiOutput('depvar'),
                                     # uiOutput('thirdvar')
                                     selectInput("indvar",
                                                 label = "Choose an independent variable to display",
                                                 choices = list("Weight", "Total Length", "Hindfoot Length", "Ear Length",
                                                                "Tail Length", "Sex", "Life Stage", "Site"),
                                                 selected = "Weight"),
                                     
                                     selectInput("depvar",
                                                 label = "Choose a dependent variable to display",
                                                 choices = c("Weight", "Total Length", "Hindfoot Length", "Ear Length",
                                                             "Tail Length"),
                                                 selected = "Total Length"),
                                     selectInput("thirdvar",
                                                 label = "Choose if to subset the data based on one of the following variables:",
                                                 choices=list("None", "Sex", "NEON Domain", "Field Site", "Species", "Life Stage", "Year"),
                                                 selected = "None")
                        )
                        
                        ,
                        mainPanel(
                          plotOutput('scatterPlot'), 
                          fluidRow(column(dataTableOutput("mammals"), width=12)))
                      ))
           ),
           tabPanel("Contour Maps",
                    fluidPage(
                      titlePanel("Spatial Distributions"),
                      fluidRow(
                        column(width = 2, uiOutput('domains')),
                        column(width = 2, uiOutput("sites")),
                        column(width = 2, uiOutput("plotID")),
                        column(width = 2, uiOutput("species")),
                        column(width = 2, uiOutput("slid"))
                      ),
                      fluidRow(
                        column(6, plotOutput('mymaps1')), 
                        column(6, plotOutput('kest'))
                      ), 
                      fluidRow(
                        column(6, plotOutput('mrc1')), 
                        column(6, plotOutput('pmrc'))
                      )
                    ))
         ) 
)
                            



