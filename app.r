source('dependencies.R')
source('datasets.R')
source('preprocess.R')
source('predmodels.R')
source('pcaqda_add.R')
source('dropdown.R')
source('spectra_plotting.R')
source('stats.R')
source('database.R')

#User Interface of Webpage-----------------------------
ui <- 
  dashboardPage(
    title="Covid AppTect", skin="black",
    # put the shinyauthr logout ui module in here
    dashboardHeader(title= span(img(src= "logfin.png", height = 35), strong("PRISMA")),titleWidth=187,
                    
                    #SIGN OUT------------------------------------------            
                    uiOutput("signout")
    ),
    dashboardSidebar(collapsed = TRUE, uiOutput("sidebar")),
    dashboardBody(
      useFirebase(),
      firebaseUIContainer(),
      tags$script(HTML("$('body').addClass('fixed');")), #Fixed Header and Menu Bar
      
      shinyjs::useShinyjs(),
      tags$style(type="text/css",
                 ".shiny-output-error { visibility: hidden; }",
                 ".shiny-output-error:before { visibility: hidden; }"
      ), #Hide Error Messages
      
      
      reqSignin(
        tabItems(
          tabItem(tabName ="home",
                  includeMarkdown("www/home-2.md")),
          tabItem(tabName ="pcaqda", uiOutput("pcaqda_ui"),
                  fluidPage(titlePanel("Personal Information"),
                            sidebarLayout(position = "left", fluid = TRUE,
                                          sidebarPanel(
                                            style = "overflow-y:scroll; max-height: 1070px",
                                            fluidRow(column(10,
                                                            selectInput("calibration_pcaqda", h3("Model Calibration",
                                                                                                 style="color:black"),
                                                                        c("", "Calibration Set 1", "Calibration Set 2"), 
                                                                        selected = "", selectize = FALSE))),
                                            fluidRow(column(10,         
                                                            fileInput("file_pcaqda", 
                                                                      h3("Input CSV File", style = "color:black"),
                                                                      buttonLabel = "Browse",
                                                                      accept = ".csv"))),
                                            fluidRow(column(10, 
                                                            textInput("name_pcaqda", h3("Patient's Name", style="color:black"), "")
                                            )),
                                            fluidRow(column(10,           
                                                            numericInput("age_pcaqda", h3("Age", style="color:black"), "")
                                            )),
                                            fluidRow(column(10, 
                                                            dateInput("bday_pcaqda", h3("Birthdate", style="color:black"), "")
                                            )),
                                            fluidRow(column(10,         
                                                            selectInput("sex_pcaqda", strong(h3("Sex", style="color:black")), c("","Male", "Female")))),
                                            fluidRow(column(10,     
                                                            selectInput("loc_pcaqda",strong(h3("Location", style="color:black")), c("","Caloocan City",
                                                                                                                                    "Las Pinas City",
                                                                                                                                    "Makati City",
                                                                                                                                    "Malabon City",
                                                                                                                                    "Mandaluyong City",
                                                                                                                                    "Manila City", 
                                                                                                                                    "Marikina City",
                                                                                                                                    "Muntinlupa City",
                                                                                                                                    "Navotas City",
                                                                                                                                    "Paranaque City", 
                                                                                                                                    "Pasay City",
                                                                                                                                    "Pasig City",
                                                                                                                                    "Pateros ",
                                                                                                                                    "Quezon City", 
                                                                                                                                    "San Juan City",
                                                                                                                                    "Taguig City",
                                                                                                                                    "Valenzuela City")
                                                                        
                                                            )
                                                            
                                            )),
                                            
                                            tags$h3("Comorbidities", style="color:black") ,
                                            fluidRow(column(
                                              width = 10,
                                              dropdownButton(
                                                label = "Choose Comorbidity", status = "default",
                                                checkboxGroupInput(inputId = "comor_pcaqda", label = "Choices:", choices = c("Hypertension",
                                                                                                                             "Cardiovascular and Cerebrovascular Conditions",
                                                                                                                             "Diabetes",
                                                                                                                             "Malignancy",
                                                                                                                             "Respiratory Illnesses",
                                                                                                                             "Renal Disorders",
                                                                                                                             "Immunodeficiencies"))
                                              )
                                            )                   
                                            ),
                                            tags$h3("Symptoms", style="color:black"),
                                            fluidRow(column(width = 10,
                                                            dropdownButton(
                                                              label = "Choose Symptom", status = "default",
                                                              checkboxGroupInput(inputId = "symptoms_pcaqda", label = "Choices:", choices = c("Fever",
                                                                                                                                              "Cough",
                                                                                                                                              "Tiredness",
                                                                                                                                              "Loss of Taste or Smell",
                                                                                                                                              "Sore Throat",
                                                                                                                                              "Headache",
                                                                                                                                              "Aches and Pains",
                                                                                                                                              "Diarrhoea",
                                                                                                                                              "Rash on Skin, or Discoloration of Fingers or Toes",
                                                                                                                                              "Red or Irritated Eyes",
                                                                                                                                              "Difficulty Breathing or Shortness of Breath",
                                                                                                                                              "Loss of Speech or Mobility, or Confusion",
                                                                                                                                              "Chest Pain"))
                                                            ),             
                                            )),
                                            tags$h3("Download Lab Report", style = "color:black"),
                                            downloadButton("download_pcaqda", "Export Report as PDF"), width = 3,
                                            fluidRow(column(10,
                                                            textInput("recipient_pcaqda",
                                                                      h3("E-mail of Patient", style="color:black"),
                                                                      value = "@gmail.com"))),   #E-Mail
                                            fluidRow(column(10,         
                                                            fileInput("fileemail_pcaqda", 
                                                                      h3("Input Lab Report", style = "color:black"),
                                                                      buttonLabel = "Browse",
                                                                      accept = ".pdf"))),
                                            fluidRow(column(10,
                                                            tags$h3("Action Buttons", style="color:black"),
                                                            tags$br(actionButton("send_pcaqda", "Send Mail")),
                                                            tags$br(actionButton("sendDB_pcaqda", "Store to Database")),
                                                            tags$br(actionButton("clear_pcaqda", "Clear All Fields"))
                                            ))
                                          ),
                                          mainPanel(
                                            #Output Calibration Set and Diagnosis
                                            fluidRow(
                                              box(title = "Calibration Set",
                                                  solidHeader = T,
                                                  width = 7, 
                                                  collapsible = T,
                                                  collapsed = T,
                                                  DT::dataTableOutput("calibration_contents_pcaqda")),
                                              box(title = "Diagnosis of Calibration Set",
                                                  solidHeader = T,
                                                  width = 5, 
                                                  collapsible = T,
                                                  collapsed = T,
                                                  tags$p(htmlOutput("calibration_diagnosis_pcaqda", inline = T))
                                              )
                                            ), #End of 1st Row
                                            
                                            #Output Preview of Input Data Set
                                            fluidRow(
                                              box(title = "Data Preview",
                                                  solidHeader = T,
                                                  width = 12, 
                                                  collapsible = T,
                                                  collapsed = F,
                                                  DT::dataTableOutput("pcaqda_contents")),
                                            ), #End of 2nd Row
                                            
                                            #Output Spectra
                                            fluidRow(
                                              box(title = "Graph of Spectrum",
                                                  solidHeader = T,
                                                  width = 12,
                                                  collapsible = T,
                                                  collapsed = F,
                                                  plotOutput("spectrum_pcaqda", width = "100%", height = "400px",
                                                             dblclick = "plot1_dblclick",
                                                             brush = brushOpts(
                                                               id = "plot1_brush",
                                                               resetOnNew = TRUE)
                                                             ))
                                            ), #End of 3rd Row
                                            
                                            #Name, Age, and Sex
                                            fluidRow(
                                              box(title = "Name",
                                                  solidHeader = T,
                                                  width = 8, 
                                                  collapsible = T,
                                                  collapsed = F,
                                                  tags$p(textOutput("name_pcaqda", inline = T))),
                                              box(title = "Age", 
                                                  solidHeader = T,
                                                  width = 2, collapsible = T,
                                                  collapsed = F,
                                                  tags$p(textOutput("age_pcaqda", inline = T))),
                                              box(title = "Sex", 
                                                  solidHeader = T,
                                                  width = 2, collapsible = T,
                                                  collapsed = F,
                                                  tags$p(textOutput("sex_pcaqda", inline = T))),
                                            ), #End of 4th Row
                                            
                                            #Comorbidities and Symptoms
                                            fluidRow(
                                              box(title = "Comorbidities",
                                                  solidHeader = T,
                                                  width = 6, 
                                                  collapsible = T,
                                                  collapsed = F,
                                                  tags$p(textOutput("comor_pcaqda", inline = T))),
                                              box(title = "Symptoms", solidHeader = T,
                                                  width = 6, collapsible = T,
                                                  tags$p(textOutput("symptoms_pcaqda", inline = T))),
                                            ), #End of 5th Row
                                            
                                            #Patient Diagnosis
                                            fluidRow(
                                              box(title = "Diagnosis",
                                                  solidHeader = T,
                                                  width = 6, 
                                                  collapsible = T,
                                                  collapsed = F,
                                                  tags$p(htmlOutput("pcaqda_diagnosis", inline = T))),
                                              box(title = "QR Code",
                                                  solidHeader = T,
                                                  width = 6,
                                                  collapsible = T,
                                                  collapsed = F,
                                                  plotOutput("qrcode_pcaqda", width = "100%", height = "150px"))
                                            ), #End of 6th Row
                                          )
                            ),
                            
                  )),
          tabItem(tabName ="inventory",
                  actionButton("refresh_pcaqda", "Reload"),
                  dataTableOutput("table_pcaqda")),
          tabItem(tabName = "statistics", uiOutput("stats_ui"),
                  fluidPage(titlePanel("COVID-19 Tracker"),
                            sidebarLayout(
                              sidebarPanel(
                                
                                span(tags$i(h6("Data are based on the diagnosis reports generated by the COVID AppTect")), style="color:#045a8d"),
                                h4(textOutput("reactive_case_count"), align = "right"),
                                h5(textOutput("reactive_latest_count"), align = "right"),
                                h6(textOutput("clean_date_reactive"), align = "right"),
                                
                                pickerInput("level_select", "Level:",   
                                            choices = c("NCR", "Cities"), 
                                            selected = c("Cities"),
                                            multiple = FALSE),
                                
                                pickerInput("location_select", "Location:",   
                                            choices = as.character(unique(cv_cases[order(cv_cases$location),]$location)), 
                                            options = list(`actions-box` = TRUE, `none-selected-text` = "Please make a selection!"),
                                            selected = as.character(unique(cv_cases[order(-cv_cases$diagnosis),]$location))[1:10],
                                            multiple = TRUE),   
                                
                                sliderInput("minimum_date",
                                            "Minimum date:",
                                            min = as.Date(cv_min_date,"%Y-%m-%d"),
                                            max = as.Date(current_date,"%Y-%m-%d"),
                                            value=as.Date(cv_min_date),
                                            timeFormat="%d %b"),
                                
                                
                              ),
                              mainPanel( plotlyOutput("location_plot")
                              )
                            )
                  )
          ),
          tabItem(tabName ="features",
                  includeMarkdown("www/features_new.md")),
          tabItem(tabName ="team",
                  includeMarkdown("www/team.md")),
          tabItem(tabName = "privacy",
                  includeMarkdown("www/privacy_policy.md")),
          tabItem(tabName ="contact", uiOutput("contact_ui"),
                  fluidPage(
                    fluidRow(
                      includeMarkdown("www/contacts.md")
                    ),
                    fluidRow(
                      wellPanel(
                        titlePanel("Feedback Form"),
                        textInput("name_feedback", "Full Name:"),
                        textInput("email_feedback", "E-Mail:", value = "@gmail.com"),
                        textAreaInput("message_feedback", "Message:", "Type your message...", width = "2000px"),
                        actionButton("send_feedback", "Submit")
                      )
                    )
                  ))
        ))
    ))

server <- function(input, output){
  f <- FirebaseUI$
    new()$ # instantiate
    set_providers( # define providers
      email = TRUE, 
    )$
    launch() # launch
  
  a <- Analytics$
    new()$
    launch()
  
  observeEvent(f$get_signed_in(), {
    f$req_sign_in()
    
    a$log_event('session_start')
  })
  
  #ADD SIGN OUT BUTTON---------------------------
  output$signout <- renderUI({
    f$req_sign_in()
    actionButton("signout", "Sign out", class = "btn-danger", style="margin-top: 7px;margin-right:8px; padding: 8px;")
    
  })
  
  
  #----------------SIDEBAR MENU----------------------------------------------
  output$sidebar <- renderUI({
    f$req_sign_in()
    sidebarMenu(
      style = "position: fixed; overflow: visible;",
      id = "tabs",
      HTML(paste0(
        "<br>",
        "<img style = 'display: block; margin-left: auto; margin-right: auto;' src='logo1.png' width = '200'>",
        "<br>",
        "<br>"
      )),
      # Setting id makes input$tabs give the tabName of currently-selected tab
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("Testing", tabName = "pcaqda", icon = icon("prescription-bottle")),
      menuItem("Inventory", tabName = "inventory", icon = icon("plus-square")),
      menuItem("Statistics", tabName = "statistics", icon = icon("chart-line")),
      menuItem("Features", tabName = "features", icon = icon("question")),
      menuItem("Team", tabName = "team", icon = icon("users-cog")),
      menuItem("Privacy Policy", tabName = "privacy", icon = icon("lock")),
      menuItem("Contact Us", tabName = "contact", icon = icon("id-badge"))
    )
  })
  
  # PCA-QDA UI and output ----------------------------------------
  output$pcaqda_ui <- renderUI({
    f$req_sign_in()
    
    #Pop-up Box for Data Privacy Agreement
    shinyalert(
      title = "Data Privacy Agreement",
      text = p(style = "text-align: justify;
                        padding-right: 35px;
                        padding-left: 35px;
                        margin-top: 5px !important;",
               "In accordance with the Data Privacy Act (DPA) of 2012 and its Implementing 
      Rules and Regulations (IRR), which became effective on September 9, 2016, I authorize 
      the students of Technological University of the Philippines - Manila to store and use 
      my patient's information for the purpose of commencing their research entitled, 
      'SARS-CoV 2 Detection Using FTIR Spectroscopy by Comparison of Chemometric Analysis 
      Through Saliva Absorbance Levels'. By submitting through this website application, I 
      provide my express approval and authorization to participate in this research."),
      size = "s", 
      closeOnEsc = FALSE,
      closeOnClickOutside = FALSE,
      html = TRUE,
      type = "info",
      showConfirmButton = TRUE,
      confirmButtonText = "I accept and fully understand the Data Privacy Agreement.",
      confirmButtonCol = "#3C5377",
      timer = 0,
      animation = TRUE,
    )
    
    #Code for Showing Plot After Reset
    val <- reactiveValues(input.spec = NULL)
    
    observe({
      # Initially will be empty
      if (is.null(input$file_pcaqda)){
        return()
      }
      
      isolate({
        val$input.spec <- read.csv(input$file_pcaqda$datapath, fileEncoding = 'UTF-8-BOM', check.names = FALSE)
      })
    })
    
    #Tidy Data for Spectrum Output
    tidy.input <- reactive({
      req(input$file_pcaqda)
      tidy.input <- melt(as.data.table(val$input.spec, keep.rownames = "Vars"), id.vars = "Vars")
      old_colnames1 <- c("Vars", "variable", "value")
      new_colnames1<- c("No", "Wavenumber", "Absorbance")
      setnames(tidy.input, old_colnames1, new_colnames1)
    })
    
    #Reactive Values for Ploot Zoom
    ranges <- reactiveValues(x = NULL, y = NULL)
    
    #Output Plot of Spectra
    output$spectrum_pcaqda <- renderPlot({
      ggplot() +
        geom_line(data = tidy.negative,
                  aes(x = factor(Wavenumber), y = as.numeric(Absorbance), colour = 'b', group = 1)) +
        geom_line(data = tidy.positive,
                  aes(x = factor(Wavenumber), y = as.numeric(Absorbance), colour = 'r', group = 1)) +
        geom_line(data = tidy.input(),
                  aes(x = factor(Wavenumber), y = as.numeric(Absorbance), colour = 'g', group = 1)) +
        scale_color_manual(name = "Legend:",
                           values = c('b' = '#003cff', 'r' = '#c20606', 'g' = '#1eb060'),
                           labels = c("Negative", "Positive", "Patient's Spectrum")) +
        labs(x = bquote("Wavenumber in"~cm^-1),
             y = "Absorbance",
             title = "Absorbance Spectra of Different Diagnoses and Patient's File") +
        theme_bw() +
        theme(plot.title = element_text(hjust = 0.5, face = "bold"),
              axis.text.x = element_text(angle = 20)) +
        scale_x_discrete(guide = guide_axis(check.overlap = TRUE)) +
        scale_y_continuous(breaks = seq(0, 5, by = 0.05)) +
        coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = FALSE)
    })
    
    #Observe Event for Plot Zoom
    observeEvent(input$plot1_dblclick, {
      brush <- input$plot1_brush
      if (!is.null(brush)) {
        ranges$x <- c(brush$xmin, brush$xmax)
        ranges$y <- c(brush$ymin, brush$ymax)
        
      } else {
        ranges$x <- NULL
        ranges$y <- NULL
      }
    })
    
    #E-mail of Lab Report
    observe({
      if(is.null(input$send_pcaqda) || input$send_pcaqda == 0) return(NULL)
      
      sub <- "Oropharyngeal Swab Laboratory Result"
      msg <- "From: PRISMA

Dear Sir/Madam,

This is with reference to the oropharyngeal swab submitted by you at our laboratory for carrying out certain tests as suggested by your physician.

Please note that as per your instruction, we have carried out ATR-FTIR Spectroscopy.

Attached herewith is the laboratory result from Hospital ABC. Please acknowledge receipt of this email. For more questions about your laboratory result, you may text 09452942784. Thank you.






Please consider the environment before you print this e-mail.

DISCLAIMER: The information contained in this communication is intended solely for the use of the individual or entity to whom it is addressed and others authorized to receive it. If you are not the intended recipient you are hereby notified that any disclosure, copying, distribution or taking any action in reliance on the contents of this information is strictly prohibited and may be unlawful. If you have erroneously received this communication, please notify us immediately by responding to this email and then delete it from your system. PRISMA is neither liable for the improper and incomplete transmission of the information contained in this communication nor for any delay in its receipt.
"
      send <- "from@gmail.com"
      receiver <- isolate(input$recipient_pcaqda)
      submit_labrep  <- function(send_pcaqda, fileemail_pcaqda, subject, msg, send, receiver) {
        id2 <- showNotification(
          "Sending laboratory report...", 
          duration = NULL, 
          closeButton = FALSE,
          type = "warning"
        )
        on.exit(removeNotification(id2), add = TRUE)
        
        send.mail(from = send,
                  to = receiver,
                  subject = sub,
                  body = msg,
                  file.names = input$fileemail_pcaqda$name,
                  attach.files = input$fileemail_pcaqda$datapath,
                  smtp = list(host.name = "smtp.gmail.com", port = 465,
                              user.name = "tupm.prisma@gmail.com", passwd="PRISMA2122", ssl=TRUE),
                  authenticate = TRUE,
                  send = TRUE)
      }
      submit_labrep(send_pcaqda, fileemail_pcaqda, subject, msg, send, receiver)
      
      #Dialog Box for Sent Email
      showModal(modalDialog(
        title = "E-mail Sent Successfully!",
        "The patient is successfully notified of their laboratory result.",
        easyClose = TRUE,
        footer = NULL
      ))
    })
    
    #Location
    location_pcaqda <- reactive({
      req(input$loc_pcaqda)
      
      input$loc_pcaqda
    })
    
    #Symptoms and Comorbidities
    output$symptoms_pcaqda <- renderText(paste(input$symptoms_pcaqda, collapse = ", "))
    output$comor_pcaqda <- renderText(paste(input$comor_pcaqda, collapse = ", "))
    
    #Patient's Name, Age and Sex
    output$name_pcaqda <- renderText(paste(input$name_pcaqda))
    output$age_pcaqda <- renderText(paste(input$age_pcaqda))
    output$sex_pcaqda <- renderText(paste(input$sex_pcaqda))
    
    #Convert Symptoms and Comorbidities into Global Variable
    sypms_pcaqda <- reactive(paste(input$symptoms_pcaqda, collapse = ", "))
    com_pcaqda <- reactive(paste(input$comor_pcaqda, collapse = ", "))
    
    #Generate QR Code
    qr_pcaqda <- reactive ({
      req(input$name_pcaqda)
      req(input$age_pcaqda)
      req(input$sex_pcaqda)
      req(input$file_pcaqda)
      
      qrcode = qr_code(paste("Name:", input$name_pcaqda, 
                             "Age:", input$age_pcaqda, 
                             "Sex:", input$sex_pcaqda,
                             "Comorbidities:", com_pcaqda(),
                             "Symptoms:", sypms_pcaqda(),
                             "Diagnosis:", pcaqda_diag_sentence1(),
                             "Performed by: Hospital ABC",
                             sep = "\n"))
      
      plot(qrcode)
    })
    
    #QR Code
    output$qrcode_pcaqda = renderPlot({
      qr_pcaqda()
    })
    
    #Filename for Lab Report
    pdfname_pcaqda <- reactive(paste(input$name_pcaqda, sep = "-", Sys.time()))
    
    #Download Button For PCA-QDA
    output$download_pcaqda <- downloadHandler(
      filename = function() {
        paste(pdfname_pcaqda(),sep = '.', 'pdf')
      },
      content = function(file_pcaqda) {
        #Notification for Report Rendering
        id <- showNotification(
          "Rendering report...", 
          duration = NULL, 
          closeButton = FALSE,
          type = "message"
        )
        on.exit(removeNotification(id), add = TRUE)
        
        #Store Uploaded CSV into Global Variable
        pcaqda_test_pdf <- read.csv(input$file_pcaqda$datapath, fileEncoding = 'UTF-8-BOM', check.names = FALSE)
        
        #Generating of PDF File
        rmarkdown::render("lab_report_template_pcaqda.Rmd", 
                          output_file = file_pcaqda,
                          params = list(
                            patient_name   = input$name_pcaqda,
                            age            = input$age_pcaqda,
                            sex            = input$sex_pcaqda,
                            symptoms       = input$symptoms_pcaqda,
                            comors         = input$comor_pcaqda,
                            file           = pcaqda_test_pdf,
                            diagnosis_qr   = pcaqda_diag_sentence1(),
                            diagnosis_word = pcaqda_diag_word(),
                            diagnosis_perc = pcaqda_diag_percent(),
                            show_code    = FALSE
                          ),
                          envir = new.env(parent = globalenv())
        )
      }
    )
    
    #Initializing Data Table and Diagnosis as NULL
    output$pcaqda_contents <- renderDataTable({{NULL}})
    output$pcaqda_diagnosis <- renderText({NULL})
    
    #Diagnosis of PCA-QDA Model
    pcaqda_diag <- reactive({
      req(input$file_pcaqda)
      pcaqda_test <- read.csv(input$file_pcaqda$datapath, fileEncoding = 'UTF-8-BOM', check.names = FALSE)
      pcaqda_test1 <- rbind(sarscov.data, pcaqda_test)
      new_grouptest_pcaqda <- predict(sarscov.pr, newdata = pcaqda_test1[nrow(pcaqda_test1),])
      new_grouptest.df_pcaqda <- as.data.frame(new_grouptest_pcaqda)
      result_pcaqda <- predict(covid_pcaqda, newdata = new_grouptest.df_pcaqda)
      pcaqda_pred <- result_pcaqda$class
      
      # IfElse Statement for Percent Match and Diagnosis
      if (pcaqda_pred == 1){
        pcaqda_diag_word <- "Negative"
        pcaqda_diag_percent <- round(result_pcaqda$posterior[ ,2]*100, digits = 4)
      } else if (pcaqda_pred == 0) {
        pcaqda_diag_word <- "Positive"
        pcaqda_diag_percent <- round(result_pcaqda$posterior[ ,1]*100, digits = 4)
      }
      
      pcaqda_diag_list <- list(pcaqda_diag_percent = pcaqda_diag_percent,
                               pcaqda_diag_word = pcaqda_diag_word)
    })
    
    #Diagnosis for Output at MainPanel
    pcaqda_diag_sentence <- reactive({
      pcaqda_diag_list <- pcaqda_diag()
      paste("<b>", pcaqda_diag_list$pcaqda_diag_word, "</b>",
            " for SARS-CoV-2 FTIR Chemometrics with a matching percentage of",
            "<b>", pcaqda_diag_list$pcaqda_diag_percent, "%", "</b>",
            ".")
    })
    
    #Diagnosis for QR Code
    pcaqda_diag_sentence1 <- reactive({
      pcaqda_diag_list <- pcaqda_diag()
      paste(pcaqda_diag_list$pcaqda_diag_word,
            " for SARS-CoV-2 FTIR Chemometrics with a matching percentage of",
            pcaqda_diag_list$pcaqda_diag_percent, "%",
            ".")
    })
    
    #Store Diagnosis as Reactive Elements for PDF Report
    pcaqda_diag_word <- reactive({
      pcaqda_diag_list <- pcaqda_diag()
      pcaqda_diag_word <- pcaqda_diag_list$pcaqda_diag_word
    })
    
    pcaqda_diag_percent <- reactive({
      pcaqda_diag_list <- pcaqda_diag()
      pcaqda_diag_percent <- pcaqda_diag_list$pcaqda_diag_percent
    })
    
    observe({
      input$file_pcaqda
      
      #Output Contents of CSV File in PCAQDA
      output$pcaqda_contents <- DT::renderDataTable({
        DT::datatable(
          req(input$file_pcaqda),
          read.csv(input$file_pcaqda$datapath, fileEncoding = 'UTF-8-BOM', check.names = FALSE),
          extensions = 'Scroller',
          options = list(scrollX = 500,
                         scroller = TRUE,
                         bFilter = 0,
                         bInfo = 0),
        )
      })
      
      #Output Diagnosis in PCA-QDA
      output$pcaqda_diagnosis <- renderText({
        pcaqda_diag_sentence()
      })
    })
    
    #Reactive Value for Calibration Sets in PCA-QDA
    datasetInput_pcaqda <- reactive({
      switch(input$calibration_pcaqda,
             "Calibration Set 1" = calibration_set1,
             "Calibration Set 2" = calibration_set2)
    })
    
    #Output Content of Calibration Set in PCAQDA
    output$calibration_contents_pcaqda <- DT::renderDataTable({
      DT::datatable(
        datasetInput_pcaqda(),
        extensions = 'Scroller',
        options = list(scrollX = 500,
                       scroller = TRUE,
                       bFilter = 0,
                       bInfo = 0),
      )
    })
    
    #Output Diagnosis of Calibration Set in PCA-QDA
    output$calibration_diagnosis_pcaqda <- renderText({
      req(datasetInput_pcaqda())
      
      calib_set_pcaqda <- datasetInput_pcaqda()
      calib_set_pcaqda <- calib_set_pcaqda[,-1]
      calib_set_pcaqda1 <- rbind(sarscov.data, calib_set_pcaqda)
      new_group_pcaqda <- predict(sarscov.pr, newdata = calib_set_pcaqda1[nrow(calib_set_pcaqda1),])
      new_group.df_pcaqda <- as.data.frame(new_group_pcaqda)
      res_pcaqda <- predict(covid_pcaqda, newdata = new_group.df_pcaqda)
      pred_calib_pcaqda <- res_pcaqda$class
      
      if (pred_calib_pcaqda == 1){
        pred_calib_pcaqda_percent <- res_pcaqda$posterior[ ,2]
      } else if (pred_calib_pcaqda == 0) {
        pred_calib_pcaqda_percent <- res_pcaqda$posterior[ ,1]
      }
      
      # IfElse Statement for Printing the Diagnosis
      if (pred_calib_pcaqda == 1){
        diagnosis_pcaqda <- paste("<b>Negative</b>","for SARS-CoV-2 FTIR Chemometrics with a matching percentage of ",
                                  "<b>", round(pred_calib_pcaqda_percent*100, digits = 4),
                                  "%",  "</b>", ".")
      } else if (pred_calib_pcaqda == 0) {
        diagnosis_pcaqda <- paste("<b>Positive</b>","for SARS-CoV-2 FTIR Chemometrics with a matching percentage of ",
                                  "<b>", round(pred_calib_pcaqda_percent*100, digits = 4),
                                  "%",  "</b>", ".")
      }
      diagnosis_pcaqda
    })
    
    #CLear All Button Function
    observeEvent(input$clear_pcaqda, {
      output$pcaqda_contents <- renderDataTable({NULL})
      output$pcaqda_diagnosis <- renderText({NULL})
      val$input.spec <- NULL
      reset("calibration_pcaqda")
      reset("file_pcaqda")
      reset("uniqueID_pcaqda")
      reset("name_pcaqda")
      reset("age_pcaqda")
      reset("bday_pcaqda")
      reset("sex_pcaqda")
      reset("loc_pcaqda")
      reset("comor_pcaqda")
      reset("symptoms_pcaqda")
      reset("pcaqda_diagnosis")
      reset("recipient_pcaqda")
      reset("fileemail_pcaqda")
    })
    
    #Chunk of Code for Database
    output$table_pcaqda <-  renderDataTable({
      qda_results<-get_query("SELECT * FROM mydb_results.`pcaqda_patients`ORDER BY `pcaqda_patients`.`id` DESC;")
      output$table_pcaqda<- renderDataTable(datatable(qda_results))
      return(qda_results)
    })
    
    observeEvent(input$uniq_pcaqda, {
      
    })
    observeEvent(input$sendDB_pcaqda,{
      stored_database <- function(symps_pcaqda, com_pcaqda, uniqueID_pcaqda, df) {
        id3 <- showNotification(
          "Storing generated results in database...", 
          duration = NULL, 
          closeButton = FALSE,
          type = "warning"
        )
        on.exit(removeNotification(id3), add = TRUE)
        
        sypms_pcaqda <- reactive(paste(input$symptoms_pcaqda, collapse = ", "))
        com_pcaqda <- reactive(paste(input$comor_pcaqda, collapse = ", "))
        uniqueID_pcaqda <- ({Idpca<- list()
        Idpca[[1]]<-LETTERS
        Idpca[[2]]<-1:9
        
        tmp<-mapply(sample,Idpca,c(3,3))
        paste(sample(tmp),collapse="")})
        
        df <- 
          data.frame(
            uniqueID = uniqueID_pcaqda ,
            name = input$name_pcaqda, 
            age = input$age_pcaqda,
            birthdate = input$bday_pcaqda,
            sex = input$sex_pcaqda, 
            comorbidities = com_pcaqda(),
            symptoms = sypms_pcaqda(),
            location = input$loc_pcaqda,
            diagnosis = pcaqda_diag_word(),
            date = Sys.time(),
            phl_region = "NCR"
          )
        
        write_query("pcaqda_patients",df)
      }
      stored_database(symps_pcaqda, com_pcaqda, uniqueID_pcaqda, df)
      
      #Dialog Box for Database
      showModal(modalDialog(
        title = "Record Stored Successfully!",
        "The generated laboratory result is successfully stored in the Inventory Tab.",
        easyClose = TRUE,
        footer = NULL
      ))
    })
    
    observeEvent(input$refresh_pcaqda,{
      qda_results<-get_query("SELECT * FROM mydb_results.`pcaqda_patients`ORDER BY `pcaqda_patients`.`id` DESC;")
      output$table_pcaqda<- renderDataTable(datatable(qda_results))
      return(qda_results)
    })
  })
  
  #Statistics UI and output --------------------------------------
  #Side Panel
  output$stats_ui<-renderUI({req(credentials()$user_auth)
    formatted_date = reactive({
      format(as.Date(input$plot_date, format="%d %B %Y"), "%Y-%m-%d")
    })
    current_date = reactive({
      format(as.Date(max(cv_cases$date),"%Y/%m/%d"))
    })
    output$clean_date_reactive <- renderText({
      format(as.Date(max(formatted_date()),"%d %B %Y"))
    })
    
    reactive_db = reactive({
      cv_cases %>% filter(date == formatted_date())
    })
    
    output$reactive_case_count <- renderText({
      paste0((count), " total cases")
    })
    
    reactive_db_latest = reactive({
      cv_cases %>% filter(date == current_date())
    })
    
    output$reactive_latest_count <- renderText({
      paste0(prettyNum(sum(reactive_db_latest()$diagnosis), big.mark=","), " new cases")
    })
    
    #update location selections
    
    observeEvent(input$level_select, {
      if (input$level_select=="NCR") {
        updatePickerInput(session = session, inputId = "location_select", 
                          choices = "NCR", selected = "NCR")
      }
      
      if (input$level_select=="Cities") {
        updatePickerInput(session = session, inputId = "location_select", 
                          choices = as.character(unique(cv_cases$location)), 
                          selected = as.character(unique(cv_cases[order(-cv_cases$diagnosis),]$location))[1:10])
      }
    }, ignoreInit = TRUE)
    
    #location-specific plots
    output$location_plot <- renderPlotly({
      location_cases_plot(location_reactive_db(), input$minimum_date)
    })
    
    #create dataframe with selected cities
    
    location_reactive_db = reactive({
      if (input$level_select=="NCR") { 
        db = cv_cases_ncr
        db$region = db$phl_region
        
      }
      
      if (input$level_select=="Cities") { 
        db = cv_cases_cities
        db$region = db$location
        
       
      }
      db %>% filter(region %in% input$location_select)
    })
  })
  
  #-----------CONTACT UI----------------------------
  output$contact_ui <- renderUI({
    f$req_sign_in()
    observe({
      if(is.null(input$send_feedback) || input$send_feedback == 0) return(NULL)
      
      subject <- paste("Feedback Form Submission of", input$name_feedback, collapse = " ")
      message <- paste(input$message_feedback, 
                       "From:", 
                       input$email_feedback,
                       sep = "\n")
      sender <- "from@gmail.com"
      recipient <- "tupm.prisma@gmail.com"
      submit_feedback <- function(send_feedback, subject, message, sender, recipient){
        #Notification for Report Rendering
        id1 <- showNotification(
          "Sending e-mail...", 
          duration = NULL, 
          closeButton = FALSE,
          type = "warning"
        )
        on.exit(removeNotification(id1), add = TRUE)
        
        send.mail(from = sender,
                  to = recipient,
                  subject = subject,
                  body = message,
                  smtp = list(host.name = "smtp.gmail.com", port = 465,
                              user.name = "tupmprisma.feedback@gmail.com", passwd="PRISMA_2122", ssl=TRUE),
                  authenticate = TRUE,
                  send = TRUE)
      }
      submit_feedback(send_feedback, subject, message, sender, recipient)
      
      #Dialog Box for Sent Email
      showModal(modalDialog(
        title = "Feedback Sent Successfully!",
        "PRISMA is successfully notified of your feedback.",
        easyClose = TRUE,
        footer = NULL
      ))
    })
    
    # SIGN OUT TAB-------------------------------------------------------------------------------------------
    
    
    observeEvent(input$signout, {
      f$sign_out()
    })
    
    
  })
  
}

shinyApp(ui, server)