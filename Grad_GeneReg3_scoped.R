################################################
#PUBLISHED AS Grad_GeneReg3
################################################




# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(data.table)
library(DT)
library(tidyr)
library(dplyr)
library(combinat)
library(DescTools)
library(bslib)
library(shinyjs)



##############################################


ui <-  
  fluidPage(
    useShinyjs(),
    bootstrapPage(
      theme = bs_theme(version = 5),
      actionButton("check","Check my answer"),
      actionButton("refresh","Give me another"),
      tabsetPanel(
        id = "panels",
        tabPanel("Tutorial", 
                 h3("Gene Regulation"),
                 p("Gene expression (the process of a segment of DNA being transcribed and translated) is tightly controlled. 
                   After all, it is disadvantageous for cells to waste energy making a protein when it is not needed. The process of regulating the expression of a gene is called ", strong("gene regulation."), 
                   ("While gene regulation can be  exquisitely complicated, here we will use a simple model to illustrate to concept.")),
                 p(
                   h3("The Lac Operon"),
                   "The", em("lac"), "operon consists of five components:",
                   div(strong("LacI:"), "a gene that encodes the", em("lac"), "inhibitor protein.", style="margin-left:40px"),
                   div(strong("Promoter:"), "the promoter upstream of LacZ and LacY.", style="margin-left:40px"),
                   div(strong("LacO:"), "stands for", em("'lac"), "operator' and is the DNA sequence that the", em("lac"), "inhibitor binds", style="margin-left:40px"),
                   div(strong("LacZ:"), "gene encoding beta-galactosidase.", style="margin-left:40px"),
                   div(strong("LacY:"), "gene encoding permease.", style="margin-left:40px"),
                   br("LacZ and LacY encode genes that enable lactose to enter the cell (permease) and for it to be metabolized by the cell (beta-galactosidase). 
                      When there is no lactose in the environment, the cell keeps transcription of LacZ and Y off to conserve energy. 
                      This is accomplished through LacI protein binding to LacO, thus blocking the ability of RNA polymerase to bind the promoter and initiate transcription. 
                      When lactose is present, lactose binds to the", em("lac"), "inhibitor (LacI) protein, which causes LacI to 'fall off' the operator; this enables RNA polymerase to bind to the promoter and transcribe the LacZ and LacY genes. 
                      Therefore, in a normal (or wild type) scenario, when there is no lactose present, LacZ and LacY are OFF; when lactose is present, LacZ and LacY are ON.")),
                 p(
                   h3("Wild type and Mutant Alleles"),
                   "There are three possible alleles for each component of the", em("lac"), "operon:",
                   div(strong("+:"), "the wild type (or typical) version of the component", style="margin-left:40px"),
                   div(strong("-:"), "a null (non-existent) or non-functional mutant version of the component", style="margin-left:40px"),
                   div(strong("CA:"), "a mutant component that is constitutively (or always) active. ", em("NOTE: In this tutorial, only LacO will be constitutively active."), style="margin-left:40px")
                 ),
                 p(
                   h3("Genotypes"),
                   "There are two genotypes for you to work with:",
                   div(strong("Haploid:"), "there is one copy of each lac operon component.", style="margin-left:40px"),
                   div(strong("Merodiploid:"), "there are two copies of each lac operon component. This allows you to investigate the activity
                       of", em("cis"), "and", em("trans"), "factors.", style="margin-left:40px")
                 ),
                 p(
                   h3("Using this tutorial"),
                   p("Watch a video for how to use this tutorial", tags$a(href="https://www.youtube.com/watch?v=atkgPjCIYQk", "here!", target="_blank")),
                   
                   p("Select the 'Haploid' or 'Merodiploid' tab. You will be presented with a genotype and four columns with radio buttons. 
                   The columns correspond to LacY without lactose (LacY/-lac), LacY with lactose (LacY/+lac), LacZ without lactose (LacZ/-lac), and LacZ with lactose (LacZ/+lac).
                   Use the radio buttons to predict whether each gene will be transcribed (ON) or not (OFF). Click 'Check my answer' to see how you did. A tracker will also
                   appear to count how many times in a row you can correctly predict the output of the lac operon. To get a new genotype, click the 'Give me another' button.")
                 )),
        tabPanel("Haploids",
                 div(class = "container-fluid",
                     h5("Draw the components of the Lac operon and use the genotype below to predict the effects on B-Gal and Permease with and without lactose.
                When you have a solution, click 'Check my answer' to see if you are correct."),
                     h5(textOutput("A")),
                     h5(textOutput("ID"), style="color:gray"),
                     div(class="row",
                         div(class= "col-sm-4",
                             DT::dataTableOutput("new.geno")),
                         div(class= "col-sm-2",
                             radioButtons("Zminus", "LacZ/-lac", choices = c("ON", "OFF")), textOutput("value1U")),
                         div(class= "col-sm-2",
                             radioButtons("Zplus", "LacZ/+lac", choices = c("ON", "OFF")),  textOutput("value2U")),
                         div(class= "col-sm-2",
                             radioButtons("Yminus", "LacY/-lac", choices = c("ON", "OFF")), textOutput("value3U")),
                         div(class= "col-sm-2",
                             radioButtons("Yplus", "LacY/+lac", choices = c("ON", "OFF")), textOutput("value4U")),
                                    div(class= "col-sm-12",
                                       htmlOutput("Explain.Haplo"))
                     )
                 )
        ),
        tabPanel("Merodiploids",
                 div(class = "container-fluid",
                     h5("Draw the components of the Lac operon and use the genotype below to predict the effects on B-Gal and Permease with and without lactose.
                When you have a solution, click 'Check my answer' to see if you are correct."),
                     h5(textOutput("B")),
                     h5(textOutput("ID2"), style="color:gray"),
                     div(class="row",
                         div(class= "col-sm-4",
                             DT::dataTableOutput("new.geno.MD")),
                         div(class= "col-sm-2",
                             radioButtons("Zminus_MD", "LacZ/-lac", choices = c("ON", "OFF")), textOutput("value1_MD")),
                         div(class= "col-sm-2",
                             radioButtons("Zplus_MD", "LacZ/+lac", choices = c("ON", "OFF")),  textOutput("value2_MD")),
                         div(class= "col-sm-2",
                             radioButtons("Yminus_MD", "LacY/-lac", choices = c("ON", "OFF")), textOutput("value3_MD")),
                         div(class= "col-sm-2",
                             radioButtons("Yplus_MD", "LacY/+lac", choices = c("ON", "OFF")), textOutput("value4_MD")),
                                    div(class= "col-sm-12",
                                        htmlOutput("Explain.Haplo.MD"))
                     )
                 )
        ),
        
        tabPanel("Gene Regulation Tutorial", 
                 h3("Gene Regulation Part II"),
                 p(
                   h4("Expression of the lac operon is 'leaky'")
                 ),
                 p("When cells are grown in the absence of lactose, the", em("lac"), "repressor is bound to the operator and thus expression of the", em("lac"), "operon is off. One of the genes encoded by the operon,", em("lacY,"), "encodes a lactose permease that enables lactose to enter the cell; some of that lactose is enzymatically converted to allolactose, which binds to the", em("lac"), "repressor and causes it to dissociate from the operator. Thus allolactose, not lactose, is the molecule that induces the operon."),   
                 p("If the", em("lac"), "repressor turns off expression of the operon, how can lactose enter the cell if the", em("lacY"), "gene that encodes the permease isn’t expressed? The answer is that expression from the", em("lac"), "operon is “leaky”: that is, even in the absence of lactose/inducer, transcription of the operon is not completely off: there is a very low level of expression from the operon that is sufficient to produce the proteins required for control of gene expression."),
                 p(
                   h4("Expression of the lac operon is regulated by a repressor and an activator")
                 ),
                 p("In addition to negative regulation of gene expression by repressor, the", em("lac"), "operon is also subject to positive regulation by an activator. As the name implies, activators are proteins that activate, or have a positive effect on, gene expression."),  
                 p("Recall that the preferred carbon source for", em("E. coli"), "is the sugar glucose. To what extent is the", em("lac"), "operon expressed in the presence of both glucose and lactose?  Since lactose is present, inducer binds to repressor causing repressor to dissociate from the operator, so we might expect high levels of expression from the operon. But, in fact, we find that in the presence of glucose, the", em("lac"), "operon is expressed at relatively low levels. However, when lactose is present and glucose is absent, the operon is expressed at very high levels. How does the cell achieve this ‘activated level’ of operon expression?"),   
                 p("Activation of the", em("lac"), "operon involves a signaling molecule called cAMP that binds to an activator protein that is referred to as either CAP (catabolite activator protein) or CRP (cAMP receptor protein). Since CAP and CRP are different names for the same protein, for simplicity we will refer to it as CAP."),
                 p("When glucose levels are high, intracellular levels of cAMP are low, and in the absence of cAMP, CAP has a very low affinity for DNA and therefore does not bind to DNA. However, when glucose levels are low, intracellular levels of cAMP increase and cAMP binds to CAP, enabling the cAMP/CAP complex to bind to specific DNA sequences called 'CAP binding sites'. One of those CAP binding sites is adjacent to the", em("lac"), "promoter, so in the absence of glucose, the cAMP/CAP complex is bound to the DNA adjacent to the promoter. A portion of the CAP protein contains a surface that has high affinity for RNA polymerase, so the binding of the cAMP-CAP complex to a region upstream of the", em("lac"), "promoter results in recruitment of RNA polymerase to the promoter, leading to much higher levels of gene expression than would occur in the absence of cAMP/CAP binding. Thus, activation results from the recruitment of RNA polymerase to the promoter by the cAMP/CAP complex bound to a sequence adjacent to the promoter."
                 ),
                 p("This type of control, which involves a combination two inputs (e.g. presence of lactose and absence of glucose) is referred to as", strong("combinatorial control"), "."
                 ),
                 p(
                   h4("Summary")
                 ),
                 p(
                   tags$ul(
                     tags$li("In the absence of lactose,", em("lac"), "repressor is bound to the operator, so transcription of the operon is ‘off’ and only leaky/very low levels of transcription of the operon occurs."), 
                     tags$li("In the presence of both lactose and glucose,", em("lac"), "repressor is not bound to the operator, enabling RNA polymerase to bind to the promoter. However, the binding of RNA polymerase to the promoter is relatively weak, leading to only low (sometimes referred to as ‘unactivated’ or ‘basal’) transcription of the operon."),
                     tags$li("In the presence of lactose and absence of glucose,", em("lac"), "repressor is not bound to the operator, enabling RNA polymerase to bind to the promoter. In addition, since cAMP levels are high, cAMP binds to CAP and the cAMP/CAP complex binds to the CAP binding site adjacent to the promoter. The favorable interactions between the cAMP/CAP complex and RNA polymerase helps recruit RNA polymerase to the promoter, leading to very high (sometimes referred to ask ‘activated’) transcription of the operon.")
                   )),
                 p(
                   h4("Levels of transcription"),
                   "The levels of transcription are...",
                   div(strong("Off/very low:"), "also referred to as 'leaky' transcription -- very low levels of transcription occur.", style="margin-left:40px"),
                   div(strong("Basal/low:"), "RNA polymerase is recruited and typical levels of transcription occur.", style="margin-left:40px"),
                   div(strong("Activated/high:"), "RNA polymerase is stabily recruited and high levels of transcription occur.", style="margin-left:40px")
                 ),
                 p(
                   h4("Using this tutorial"),
                   "You will be presented with an environmental condition and three radio buttons. Use the radio buttons to predict if the lac operon will be transcribed at low, basal, or high levels. 
                   Click 'Check my answer' to see how you did. A tracker will also appear to count how many times in a row you can correctly predict the output of the lac operon. 
                   To get a new genotype, click the 'Give me another' button."
                 )),
        tabPanel("Wildtype operon",
                 div(class = "container-fluid",
                     h5("Draw the components of the Lac operon and use the environmental conditions below to predict the level of transcription of the lac operon.
                When you have a solution, click 'Check my answer' to see if you are correct."),
                     h5(textOutput("A2")),
                     h5(textOutput("ID3"), style="color:gray"),
                     div(class="row",
                         div(class= "col-sm-4",
                             DT::dataTableOutput("new.env")),
                         div(class= "col-sm-2",
                             radioButtons("transx", "Transcription level", choices = c("Off/very low", "Basal/low", "Activated/high")), textOutput("value1"))
                     )
                 )
        ),
        tabPanel("Operon Regulation",
                 div(class = "container-fluid",
                     h5("Draw the components of the Lac operon and use the environmental conditions below to predict the level of transcription of the lac operon.
               When you have a solution, click 'Check my answer' to see if you are correct."),
                     h5(textOutput("B2")),
                     h5(textOutput("ID4"), style="color:gray"),
                     div(class="row",
                         div(class= "col-sm-4",
                             DT::dataTableOutput("new.geno.gl")),
                         div(class= "col-sm-2",
                             radioButtons("PlusMin_env2", "+glu -lac", choices = c("Off/very low", "Basal/low", "Activated/high")), textOutput("value1_env2")),
                         div(class= "col-sm-2",
                             radioButtons("PlusPlus_env2", "+glu +lac", choices = c("Off/very low", "Basal/low", "Activated/high")), textOutput("value2_env2")),
                         div(class= "col-sm-2",
                             radioButtons("MinMin_env2", "-glu -lac", choices = c("Off/very low", "Basal/low", "Activated/high")), textOutput("value3_env2")),
                         div(class= "col-sm-2",
                             radioButtons("MinPlus_env2", "-glu +lac", choices = c("Off/very low", "Basal/low", "Activated/high")), textOutput("value4_env2")),
                                    div(class= "col-sm-12",
                                        htmlOutput("Explain.OpReg"))
                     )
                 )),
        
        
        
        
        tabPanel("Bug Report", "Did you find a mistake? Email", a(href="lacON.lacOFF@gmail.com","lacON.lacOFF@gmail.com"), "with the genotype and phenotype information.")
        # tabPanel("Tracking", DT::dataTableOutput("tracks"), DT::dataTableOutput("tracks2"))
        
      )))



server <- function(input,output,session) 
  
  
  
{
 
#Setting up all possible haploid permutations for the lac operon genes as WT, mutant, or constitutively active for HAPLOID
  fU <- (c("+","-","CA"))
  xU <- fU
  nU <- 5
  mU <- 5
  lac.permutationsU<-as.data.frame(CombSet(xU, mU, repl=TRUE, ord=TRUE))
  names(lac.permutationsU)<- c("LacI","Promoter","LacO","LacZ","LacY")
  
  #Limiting permutations to only LacO being constitutively active
  lac.permutationsU <- subset(lac.permutationsU, LacI != "CA" & Promoter != "CA" & LacZ != "CA" & 
                                LacY != "CA")
  
#Logic of the lac operon Haploids
  lac.permutationsU <- lac.permutationsU %>%
    mutate(`Bgal/-lac` = case_when(LacZ == "-" ~ "OFF",
                                   Promoter == "-" ~ "OFF",
                                   LacI == "-" ~ "ON",
                                   LacI == "CA" & LacO == "+" ~ "OFF",
                                   LacO == "+" ~ "OFF",
                                   LacO == "+" ~ "OFF",
                                   LacO == "-" | LacO == "CA" ~ "ON",
                                   Promoter == "CA" ~ "ON",
                                   Promoter == "+" ~ "OFF",
                                   LacZ == "CA" ~ "ON",
                                   TRUE ~ "NA")) %>%
    mutate(`Bgal/+lac` = case_when(LacZ == "-" ~ "OFF",
                                   Promoter == "-" ~ "OFF",
                                   LacI == "-" ~ "ON",
                                   LacI == "CA" & LacO == "+" ~ "OFF",
                                   LacO == "+" ~ "ON",
                                   LacO == "-" | LacO == "CA" ~ "ON",
                                   Promoter == "CA" ~ "ON",
                                   Promoter == "+" ~ "ON",
                                   TRUE ~ "NA")) %>%
    mutate(`Permease/-lac` = case_when(LacY == "-" ~ "OFF",
                                       Promoter == "-" ~ "OFF",
                                       LacI == "-" ~ "ON",
                                       LacI == "CA" & LacO == "+" ~ "OFF",
                                       LacO == "+" ~ "OFF",
                                       LacO == "-" | LacO == "CA" ~ "ON",
                                       Promoter == "CA" ~ "ON",
                                       Promoter == "+" ~ "OFF",
                                       LacY == "CA" ~ "ON",
                                       TRUE ~ "NA")) %>%
    mutate(`Permease/+lac` = case_when(LacY == "-" ~ "OFF",
                                       Promoter == "-" ~ "OFF",
                                       LacI == "-" ~ "ON",
                                       LacI == "CA" & LacO == "+" ~ "OFF",
                                       LacO == "+" ~ "ON",
                                       LacO == "-" | LacO == "CA" ~ "ON",
                                       Promoter == "CA" ~ "ON",
                                       Promoter == "+" ~ "ON",
                                       TRUE ~ "NA"))
  
#Creating genotype -- haploid  
  input.dataU <- lac.permutationsU[1,]
  Element1U = c("I","P","O","Z","Y")
  listU <- as.data.frame(paste(Element1U,input.dataU[1,c(1:5)], sep = "", collapse = ' '))
  listU <- as.data.frame(gsub("OCA", "O(CA)", listU))
  names(listU)<- "Genotype"
  
  RIU <- reactiveValues(data = input.dataU)
  RVU <- reactiveValues(data = listU)
  
#Creating "unique" ID for each user  
  InputIDU <- function(n = 10000) {
    aU <- do.call(paste0, replicate(5, sample(LETTERS, n, TRUE), FALSE))
    paste0(aU, sprintf("%04d", sample(9999, n, TRUE)), sample(LETTERS, n, TRUE))
  }
  
  RIDU <- reactiveValues(data = InputIDU(1))
  

  
#############################################

  
  
#Setting up all possible haploid permutations for the lac operon genes as WT, mutant, or constitutively active for DIPLOID
  
  gU <- (c("+","-","CA"))
  dU <- gU
  pU <- 10
  qU <- 10
  lac.permutations_MDU<-as.data.frame(CombSet(dU, qU, repl=TRUE, ord=TRUE))
  names(lac.permutations_MDU)<- c("LacI_1","Promoter_1","LacO_1","LacZ_1","LacY_1", "LacI_2","Promoter_2","LacO_2","LacZ_2","LacY_2")
  
  #Limiting permutations to only LacO being constitutively active
  lac.permutations_MDU <- subset(lac.permutations_MDU , LacI_1 != "CA" & Promoter_1 != "CA" & LacZ_1 != "CA" & 
                                   LacY_1 != "CA" & LacI_2 != "CA" & Promoter_2 != "CA" &
                                   LacZ_2 != "CA" & LacY_2 != "CA")
  
  #Logic of lac operon -- DIPLOID
  lac.permutations_MDU  <- lac.permutations_MDU %>%
    mutate(`Bgal/-lac` = case_when((Promoter_1  == "-" & Promoter_2 == "-") ~ "OFF",
                                   (LacZ_1 == "-" & LacZ_2 == "-") ~ "OFF",
                                   (Promoter_1 == "-" & LacZ_1 == "+" & LacZ_2 == "-") ~ "OFF",
                                   (Promoter_2 == "-" & LacZ_2 == "+" & LacZ_1 == "-") ~ "OFF",
                                   ((LacO_1 == "-" & LacO_2 == "-") | (LacO_1 == "CA" & LacO_2 == "CA") | (LacO_1 == "-" & LacO_2 == "CA") | (LacO_1 == "CA"& LacO_2 == "-") & (Promoter_1  == "+") & (LacZ_1 == "+")) ~ "ON",
                                   ((LacO_1 == "-" & LacO_2 == "-") | (LacO_1 == "CA" & LacO_2 == "CA") | (LacO_1 == "-" & LacO_2 == "CA") | (LacO_1 == "CA"& LacO_2 == "-") & (Promoter_2  == "+") & (LacZ_2 == "+")) ~ "ON",
                                   ((LacI_1 == "-" & LacI_2 == "-") & (Promoter_1  == "+") & LacO_1 == "-" & (LacZ_1 == "+")) ~ "ON",
                                   ((LacI_1 == "-" & LacI_2 == "-") & (Promoter_2  == "+") & LacO_2 == "-" & (LacZ_2 == "+")) ~ "ON",
                                   ((Promoter_1 == "+") & (LacO_1  == "-" |LacO_1  == "CA") & (LacZ_1  == "+")) ~ "ON",
                                   ((Promoter_2 == "+") & (LacO_2  == "-" |LacO_2  == "CA") & (LacZ_2  == "+")) ~ "ON",
                                   ((LacI_1 == "-" & LacI_2== "-") & (Promoter_1 == "+") & LacZ_1 != "-") ~ "ON",
                                   ((LacI_1 == "-" & LacI_2== "-") & (Promoter_2 == "+") & LacZ_2 != "-") ~ "ON",
                                   TRUE ~ "OFF")) %>%
    mutate(`Bgal/+lac` = case_when((LacZ_1  == "-" & LacZ_2 == "-") ~ "OFF",
                                   (Promoter_1  == "-" & Promoter_2 == "-") ~ "OFF",
                                   (Promoter_1  == "-" & Promoter_2 == "+" & LacZ_2 == "-") ~ "OFF",
                                   (Promoter_2  == "-" & Promoter_1 == "+" & LacZ_1 == "-") ~ "OFF",
                                   (Promoter_1 == "-" & LacZ_1 == "+" & LacZ_2 == "-") ~ "OFF",
                                   (Promoter_2 == "-" & LacZ_2 == "+" & LacZ_1 == "-") ~ "OFF",
                                   TRUE ~ "ON")) %>%
    mutate(`Permease/-lac` = case_when((Promoter_1  == "-" & Promoter_2 == "-") ~ "OFF",
                                       (LacY_1 == "-" & LacY_2 == "-") ~ "OFF",
                                       (Promoter_1 == "-" & LacY_1 == "+" & LacY_2 == "-") ~ "OFF",
                                       (Promoter_2 == "-" & LacY_2 == "+" & LacY_1 == "-") ~ "OFF",
                                       ((LacO_1 == "-" & LacO_2 == "-") | (LacO_1 == "CA" & LacO_2 == "CA") | (LacO_1 == "-" & LacO_2 == "CA") | (LacO_1 == "CA"& LacO_2 == "-") & (Promoter_1  == "+") & (LacY_1 == "+")) ~ "ON",
                                       ((LacO_1 == "-" & LacO_2 == "-") | (LacO_1 == "CA" & LacO_2 == "CA") | (LacO_1 == "-" & LacO_2 == "CA") | (LacO_1 == "CA"& LacO_2 == "-") & (Promoter_2  == "+") & (LacY_2 == "+")) ~ "ON",
                                       ((LacI_1 == "-" & LacI_2 == "-") & (Promoter_1  == "+") & LacO_1 == "-" & (LacY_1 == "+")) ~ "ON",
                                       ((LacI_1 == "-" & LacI_2 == "-") & (Promoter_2  == "+") & LacO_2 == "-" & (LacY_2 == "+")) ~ "ON",
                                       ((Promoter_1 == "+") & (LacO_1  == "-" |LacO_1  == "CA") & (LacY_1  == "+")) ~ "ON",
                                       ((Promoter_2 == "+") & (LacO_2  == "-" |LacO_2  == "CA") & (LacY_2  == "+")) ~ "ON",
                                       ((LacI_1 == "-" & LacI_2== "-") & (Promoter_1 == "+") & LacY_1 != "-") ~ "ON",
                                       ((LacI_1 == "-" & LacI_2== "-") & (Promoter_2 == "+") & LacY_2 != "-") ~ "ON",
                                       TRUE ~ "OFF")) %>%
    mutate(`Permease/+lac` = case_when((LacY_1  == "-" & LacY_2 == "-") ~ "OFF",
                                       (Promoter_1  == "-" & Promoter_2 == "-") ~ "OFF",
                                       (Promoter_1 == "-" & LacY_1 == "+" & LacY_2 == "-") ~ "OFF",
                                       (Promoter_2 == "-" & LacY_2 == "+" & LacY_1 == "-") ~ "OFF",
                                       (Promoter_1  == "-" & Promoter_2 == "+" & LacY_2 == "-") ~ "OFF",
                                       (Promoter_2  == "-" & Promoter_1 == "+" & LacY_1 == "-") ~ "OFF",
                                       TRUE ~ "ON"))

#Creating genotype -- diploid  
  input.data_MDU <- sample_n(lac.permutations_MDU, 1)
  Element1U = c("I","P","O","Z","Y")
  Element2U = c("I","P","O","Z","Y")
  list1U <- as.data.frame(paste(Element1U,input.data_MDU[1,c(1:5)], sep = "", collapse = ' '))
  list1U <- gsub("OCA", "O(CA)", list1U)
  names(list1U)<- "Genotype"
  list2U <- as.data.frame(paste(Element2U,input.data_MDU[1,c(6:10)], sep = "", collapse = ' '))
  list2U <- gsub("OCA", "O(CA)", list2U)
  names(list2U)<- "Genotype"
  list_MDU <- rbind(list1U, list2U)
  rownames(list_MDU) <- c("1", "2")
  names(list_MDU)<- "Genotype"
  
  RI_MDU<- reactiveValues(data = input.data_MDU)
  RV_MDU <- reactiveValues(data = list_MDU)
  

  
  
#############################################

  
  
  
#Setting glu and lac environments
  
  f <- (c("+","-"))
  x <- f
  n <- 2
  m <- 2
  glu.lac.permutations<-as.data.frame(CombSet(x, m, repl=TRUE, ord=TRUE))
  names(glu.lac.permutations)<- c("glu","lac")
  
  #Setting glu and lac environment logic
  glu.lac.permutations <- glu.lac.permutations %>%
    mutate(`Transcripton Level` = case_when((glu == "+" & lac == "-") ~ "Off/very low",
                                            (glu == "+" & lac == "+") ~ "Basal/low",
                                            (glu == "-" & lac == "-") ~ "Off/very low",                                       
                                            (glu == "-" & lac == "+") ~ "Activated/high",
                                            TRUE ~ "NA"))
  
  
  #Making new glu and lac environments
  input.data <- glu.lac.permutations[3,]
  Element1 = c("glu", "lac")
  list <- as.data.frame(paste(Element1,input.data[1,c(1:2)], sep = "", collapse = ' '))
  names(list)<- "Environmental condition"
  
  RI <- reactiveValues(data = input.data)
  RV <- reactiveValues(data = list)
  
  ################################################################################
  
  #InputID <- myFun <- function(n = 10000) {
  # a <- do.call(paste0, replicate(5, sample(LETTERS, n, TRUE), FALSE))
  #paste0(a, sprintf("%04d", sample(9999, n, TRUE)), sample(LETTERS, n, TRUE))
  #}
  

#############################################
#############################################
  
  
  
#Setting glu and lac environment + genotypes 
  g <- (c("+","-","CA"))
  d <- g
  p <- 6
  q <- 6
  gal.lac.permutations2<-as.data.frame(CombSet(d, q, repl=TRUE, ord=TRUE))
  names(gal.lac.permutations2)<- c("LacI","CAP", "Promoter","LacO","LacZ","LacY")
  
  #Limiting the number of genotype possibilities
  gal.lac.permutations2 <- subset(gal.lac.permutations2, LacI != "CA" & Promoter != "CA" & LacZ != "CA" & 
                                    LacY != "CA")
  gal.lac.permutations2 <- subset(gal.lac.permutations2, LacZ == "+" & LacY == "+")
  gal.lac.permutations2 <- gal.lac.permutations2[c(1, 2, 3, 7, 13, 25),]
  
  #Logic of glu and lac environment + genotypes 
  gal.lac.permutations2 <- gal.lac.permutations2 %>%
    mutate(`+ glu - lac` = case_when(CAP == "-" ~ "Off/very low",
                                     Promoter == "-" ~ "Off/very low",
                                     LacI == "-" | LacO == "-" | LacO == "CA" ~ "Basal/low",
                                     LacI == "+" & Promoter == "+" & LacO == "+" & CAP == "+" ~ "Off/very low",
                                     TRUE ~ "NA")) %>%
    
    mutate(`+glu +lac` = case_when(Promoter == "-" ~ "Off/very low",
                                   TRUE ~ "Basal/low")) %>%
    
    mutate(`-glu -lac` = case_when(Promoter == "-" | CAP == "-"  ~ "Off/very low",
                                   LacI == "-" | LacO == "-" | LacO == "CA" ~ "Activated/high",
                                   LacI == "+" & Promoter == "+" & LacO == "+" & CAP == "+" ~ "Off/very low",
                                   TRUE ~ "NA")) %>%
    
    mutate(`-glu +lac` = case_when(Promoter == "-" ~ "Off/very low",
                                   CAP == "-" ~ "Basal/low",
                                   TRUE ~ "Activated/high"))
  
  
  #Creating new genotypes
  input.data.gl <- gal.lac.permutations2[1,]
  Element3 = c("I", "C", "P","O","Z","Y")
  list3 <- as.data.frame(paste(Element3,input.data.gl[1,c(1:6)], sep = "", collapse = ' '))
  list3 <- as.data.frame(gsub("OCA", "O(CA)", list3))
  list3 <- as.data.frame(gsub("CCA", "C(CA)", list3))
  names(list3)<- "Genotype"
  
  
  RI_GL<- reactiveValues(data = input.data.gl)
  RV_GL <- reactiveValues(data = list3)
 
  
   
#############################################

  
  
  
#Output tables for new genotypes
  output$new.geno = DT::renderDataTable(RVU$data, options=list(dom='t'))
  output$new.geno.MD = DT::renderDataTable(RV_MDU$data, options=list(dom='t'))
  output$new.env = DT::renderDataTable(RV$data, options=list(dom='t'))
  output$new.geno.gl = DT::renderDataTable(RV_GL$data, options=list(dom='t'))

  
#Disabling buttons while moving between tabs
  observeEvent(input$check, {
    if(input$panels == "Haploids"){
      shinyjs::disable("check")}}
  )
  observeEvent(input$refresh, {
    if(input$panels == "Haploids"){
      shinyjs::enable("check")}}
  )
  
  observeEvent(input$check, {
    if(input$panels == "Merodiploids"){
      shinyjs::disable("check")}}
  )
  
  observeEvent(input$refresh, {
    if(input$panels == "Merodiploids"){
      shinyjs::enable("check")}}
  )
  
  observeEvent(input$check, {
    if(input$panels == "Wildtype operon"){
      shinyjs::disable("check")}}
  )
  observeEvent(input$refresh, {
    if(input$panels == "Wildtype operon"){
      shinyjs::enable("check")}}
  )
  
  observeEvent(input$check, {
    if(input$panels == "Operon Regulation"){
      shinyjs::disable("check")}}
  )
  
  observeEvent(input$refresh, {
    if(input$panels == "Operon Regulation"){
      shinyjs::enable("check")}}
  )
  
 
  observeEvent(input$panels, {
    if(input$panels == "Merodiploids" && is.na(report.dataRV_MDU$data[(nrow(report.dataRV_MDU$data)),2])) {
      shinyjs::enable("check")}
    else if(input$panels == "Haploids" && is.na(report.dataRVU$data[(nrow(report.dataRVU$data)),2])) {
      shinyjs::enable("check")}
   else if(input$panels == "Operon Regulation" && is.na(report.dataRV_GL$data[(nrow(report.dataRV_GL$data)),2])) {
      shinyjs::enable("check")}
    else if(input$panels == "Wildtype operon" && is.na(report.dataRV$data[(nrow(report.dataRV$data)),2])) {
      shinyjs::enable("check")}
    else shinyjs::disable("check")
  }
  )
  
  
  
  
  
  

  

  
#Making inputs reactive after click, not all the time   
  c1U <- reactive(input$Zminus)
  c2U <- reactive(input$Zplus)
  c3U <- reactive(input$Yminus)
  c4U <- reactive(input$Yplus)
  
  c1_MD <- reactive(input$Zminus_MD)
  c2_MD <- reactive(input$Zplus_MD)
  c3_MD <- reactive(input$Yminus_MD)
  c4_MD <- reactive(input$Yplus_MD)
  
  
  c1 <- reactive(input$transx)
  
  c1_GL <- reactive(input$PlusMin_env2)  
  c2_GL <- reactive(input$PlusPlus_env2)  
  c3_GL <- reactive(input$MinMin_env2)
  c4_GL <- reactive(input$MinPlus_env2)
  
  
  
#REFRESH: Button to make new genotype
  observeEvent(input$refresh, {
    
    #Haploid
    if(input$panels == "Haploids"){
      
      RIU$data <- sample_n(lac.permutationsU, 1)
      RVU$data[1,1] <- paste(c("I","P","O","Z","Y"), RIU$data[1,c(1:5)], sep = "", collapse = ' ')
      RVU$data[1,1] <- gsub("OCA", "O(CA)", RVU$data[1,1])
      rownames(RVU$data) <- "1"
      
      #making is so nothing is below input areas and that a reset changes the input to ON
      output$value1U = renderText("")
      output$value2U = renderText("")
      output$value3U = renderText("")
      output$value4U = renderText("")
      output$Explain.Haplo = renderText("")
      
      updateSelectInput(session, "Zminus", selected = "ON")
      updateSelectInput(session, "Zplus", selected = "ON")
      updateSelectInput(session, "Yminus", selected = "ON")
      updateSelectInput(session, "Yplus", selected = "ON")
    }
    
    
    #Diploid
    if(input$panels == "Merodiploids"){
      
      RI_MDU$data <- sample_n(lac.permutations_MDU, 1)
      RV_MDU$data[1,1] <- paste(c("I","P","O","Z","Y"), RI_MDU$data[1,c(1:5)], sep = "", collapse = ' ')
      RV_MDU$data[1,1] <- gsub("OCA", "O(CA)", RV_MDU$data[1,1])
      RV_MDU$data[2,1] <- paste(c("I","P","O","Z","Y"), RI_MDU$data[1,c(6:10)], sep = "", collapse = ' ')
      RV_MDU$data[2,1] <- gsub("OCA", "O(CA)", RV_MDU$data[2,1])
      rownames(RV_MDU$data) <- c("1", "2")
      names(RV_MDU$data)<- "Genotype"
      
      #making is so nothing is below input areas and that a reset changes the input to ON
      output$value1_MD = renderText("")
      output$value2_MD = renderText("")
      output$value3_MD = renderText("")
      output$value4_MD = renderText("")
      output$Explain.Haplo.MD = renderText("")
      updateSelectInput(session, "Zminus_MD", selected = "ON")
      updateSelectInput(session, "Zplus_MD", selected = "ON")
      updateSelectInput(session, "Yminus_MD", selected = "ON")
      updateSelectInput(session, "Yplus_MD", selected = "ON")
    }
    
    
    
    #Glucose/Lactose
    observeEvent(input$refresh, {
      if(input$panels == "Wildtype operon"){
        
        RI$data <- sample_n(glu.lac.permutations, 1)
        RV$data[1,1] <- paste(c("glu", "lac"), RI$data[1,c(1:2)], sep = "", collapse = ' ')
        rownames(RV$data) <- "1"
        
        #making is so nothing is below input areas and that a reset changes the input to LOW
        output$value1 = renderText("")
        updateSelectInput(session, "transx", selected = "Off/very low")
      }
      

      #Glucose/Lactose + Environment
      if(input$panels == "Operon Regulation"){
        
        RI_GL$data <- sample_n(gal.lac.permutations2, 1)
        RV_GL$data[1,1] <- paste(c("I","C", "P","O","Z","Y"), RI_GL$data[1,c(1:6)], sep = "", collapse = ' ')
        RV_GL$data[1,1] <- gsub("OCA", "O(CA)", RV_GL$data[1,1])
        RV_GL$data[1,1] <- gsub("CCA", "C(CA)", RV_GL$data[1,1])
        names(RV_GL$data)<- "Genotype"
        
        
        #making is so nothing is below input areas and that a reset changes the input to Low
        output$value1_env2 = renderText("")
        output$value2_env2 = renderText("")
        output$value3_env2 = renderText("")
        output$value4_env2 = renderText("")
        output$Explain.OpReg = renderText("")
        
        
        updateSelectInput(session, "PlusMin_env2", selected = "Off/very low")
        updateSelectInput(session, "PlusPlus_env2", selected = "Off/very low")
        updateSelectInput(session, "MinMin_env2", selected = "Off/very low")
        updateSelectInput(session, "MinPlus_env2", selected = "Off/very low")
      }
    })
  })
  
  
#KEEPING TRACK AND CHECKING ANSWERS -- HAPLOID  

  #Making a dataframe to keep track of correct answers
  observeEvent(input$refresh, {
    
    #haploid
    if(input$panels == "Haploids"){
      new_rowU = data.frame(matrix(ncol=3, nrow=1))
      names(new_rowU) <- c("Attempt", "correct", "consec")
      new_rowU[1,1] = nrow(report.dataRVU$data)+1
      new_rowU[1,3] = report.dataRVU$data[(nrow(report.dataRVU$data)),3]
      report.dataRVU$data=rbind(report.dataRVU$data, new_rowU)}
  }
  )
  
  #Making dataframe to house HAPLOID attempts  
  report.dataU <- data.frame(matrix(ncol=3, nrow=0))
  names(report.dataU) <- c("Attempt", "correct", "consec")
  report.dataU[1,1] = 1
  report.dataRVU = reactiveValues(data = report.dataU)
  
  output$tracksU = DT::renderDataTable(report.dataRVU$data)
  
  
  #Checking answer (haploid)
  observeEvent(input$check, {
    
    d1U <- c1U()
    d2U <- c2U()
    d3U <- c3U()
    d4U <- c4U()
    
    if(input$panels == "Haploids")
    {output$value1U <- renderText(
      {if(d1U == RIU$data[1,6]) "Correct"
        else "Try again"})
    output$value2U <- renderText(
      {if(d2U == RIU$data[1,7])  "Correct"
        else "Try again"})
    output$value3U <- renderText(
      {if(d3U == RIU$data[1,8]) "Correct"
        else "Try again"})
    output$value4U <- renderText(
      {if(d4U == RIU$data[1,9])  "Correct"
        else "Try again" })
    }
    else (NULL)
    
  #Adding in new row when check is performed (so that they cannot just fix results)   
    if(input$panels == "Haploids")
    {new_rowU = data.frame(matrix(ncol=3, nrow=1))
    names(new_rowU) <- c("Attempt", "correct", "consec")
    new_rowU[1,1] = nrow(report.dataRVU$data)+1
    new_rowU[1,3] = report.dataRVU$data[(nrow(report.dataRVU$data)),3]
    report.dataRVU$data=rbind(report.dataRVU$data, new_rowU)
    }
    
    #Counting number in a row correct (haploid)
      if(input$panels == "Haploids")
      {
        if((input$Zminus == RIU$data[1,6]) & (input$Zplus == RIU$data[1,7]) & (input$Yminus == RIU$data[1,8]) & (input$Yplus == RIU$data[1,9]))
          report.dataRVU$data[(nrow(report.dataRVU$data)),2] = "yes"
        else report.dataRVU$data[(nrow(report.dataRVU$data)),2] = "no"
        
        if(report.dataRVU$data[(nrow(report.dataRVU$data)),2] == "yes")
          report.dataRVU$data[(nrow(report.dataRVU$data)),3] = 1
        else report.dataRVU$data[(nrow(report.dataRVU$data)),3] = 0
        
        if(nrow(report.dataRVU$data)>2 & report.dataRVU$data[(nrow(report.dataRVU$data)),2] == "yes")
          report.dataRVU$data[(nrow(report.dataRVU$data)),3] = report.dataRVU$data[(nrow(report.dataRVU$data))-1,3] + report.dataRVU$data[(nrow(report.dataRVU$data)),3]
        else if(report.dataRVU$data[(nrow(report.dataRVU$data)),2] == "yes")
          report.dataRVU$data[(nrow(report.dataRVU$data)),3] = 1
        else report.dataRVU$data[(nrow(report.dataRVU$data)),3] = 0
        
        output$A = renderText({
          paste("Number in a row correct = ",(report.dataRVU$data[(nrow(report.dataRVU$data)),3]), sep = "")})
      }

    
    
  #Logic of correct answers (haploid)  
    if(input$panels == "Haploids")
    {if ((input$Zminus == RIU$data[1,6]) & (input$Zplus == RIU$data[1,7]) & (input$Yminus == RIU$data[1,8]) & (input$Yplus == RIU$data[1,9])) output$Explain.Haplo <- renderText("")
    else 
      output$Explain.Haplo <- renderUI({
        str1 <- paste("<b> EXPLANATION </b>")
        str2 <- paste("<b> LacZ/-lac: </b>", 
                      (if (RIU$data[1,2] == "-") "Because the Promoter is '-', transcription will be OFF because RNA polymerase cannot bind." else      #promoter
                        if (RIU$data[1,4] == "-") "Because LacZ is '-', transcription will be OFF because the gene is nonfunctional."     else           #lacZ
                          if (RIU$data[1,1] == "-") "Because LacI is '-', transcription will be ON because the inhibitor is not functional." else           #LacI 
                            if (RIU$data[1,1] == "CA" & RIU$data[1,3] == "+") "Because LacI is 'CA', transcription will be OFF because the inhibitor will always be bound." else      #LacI and LacO
                              if (RIU$data[1,3] == "+") "Because the operator is functional, the inhibitor will be bound when no lactose is present, thus keeping transcription OFF." else        #LacO
                                if (RIU$data[1,3] == "-" | RIU$data[1,3] == "CA") "Because the operator is not functional, the inhibitor cannot bind, thus keeping transcription ON" else        #LacO
                                  if (RIU$data[1,2] == "+") "Because the Promoter is '+' but RNA polymerase is blocked due to the the inhibitor protein, transcription will be OFF." else        #promoter
                                    if (RIU$data[1,4] == "CA") "Because LacZ is 'CA', so transcription will be ON."  else             #lacZ 
                                      ("")
                      )
        )
        str3 <- paste("<b> LacZ/+lac: </b>", 
                      (if  (RIU$data[1,2] == "-") "Because the Promoter is '-', transcription will be OFF because RNA polymerase cannot bind." else      #promoter
                        if (RIU$data[1,4] == "-") "Because LacZ is '-', transcription will be OFF because the gene is nonfunctional."     else           #lacZ
                          if (RIU$data[1,1] == "-") "Because LacI is '-', transcription will be ON because the inhibitor is not functional." else           #LacI 
                            if (RIU$data[1,1] == "CA" & RIU$data[1,3] == "+") "Because LacI is 'CA', transcription will be OFF because the inhibitor will always be bound." else      #LacI and LacO
                              if (RIU$data[1,3] == "+") "Because the operator is functional, meaning the inhibitor will not bind when lactose is present, thus transcription is ON" else        #LacO
                                if (RIU$data[1,3] == "-" | RIU$data[1,3] == "CA") "Because the operator is not functional, the inhibitor cannot bind, thus keeping transcription ON" else        #LacO
                                  if (RIU$data[1,2] == "CA") "Because the Promoter is always bound by RNA polymerase, thus keeping transcription will be ON." else        #promoter
                                    if (RIU$data[1,2] == "+") "Because the Promoter is functional, thus  transcription is ON."  else             #lacZ 
                                      ("")
                      )
        )
        str4 <- paste("<b> LacY/-lac: </b>", "Because",
                      (if (RIU$data[1,2] == "-") "Because the Promoter is '-', transcription will be OFF because RNA polymerase cannot bind." else      #promoter
                        if (RIU$data[1,5] == "-") "Because LacY is '-', transcription will be OFF because the gene is nonfunctional."     else           #lacZ
                          if (RIU$data[1,1] == "-") "Because LacI is '-', transcription will be ON because the inhibitor is not functional." else           #LacI 
                            if (RIU$data[1,1] == "CA" & RIU$data[1,3] == "+") "Because LacI is 'CA', transcription will be OFF because the inhibitor will always be bound." else      #LacI and LacO
                              if (RIU$data[1,3] == "+") "Because the operator is functional, the inhibitor will be bound when no lactose is present, thus keeping transcription OFF." else        #LacO
                                if (RIU$data[1,3] == "-" | RIU$data[1,3] == "CA") "Because the operator is not functional, the inhibitor cannot bind, thus keeping transcription ON" else        #LacO
                                  if (RIU$data[1,2] == "+") "Because the Promoter is '+' but RNA polymerase is blocked due to the the inhibitor protein, transcription will be OFF." else        #promoter
                                    if (RIU$data[1,5] == "CA") "Because LacZ is 'CA', so transcription will be ON."  else             #lacZ 
                                      ("")
                      )
        )
        
        str5 <- paste("<b> LacY/+lac: </b>", "Because",
                      (if  (RIU$data[1,2] == "-") "Because the Promoter is '-', transcription will be OFF because RNA polymerase cannot bind." else      #promoter
                        if (RIU$data[1,5] == "-") "Because LacZ is '-', transcription will be OFF because the gene is nonfunctional."     else           #lacZ
                          if (RIU$data[1,1] == "-") "Because LacI is '-', transcription will be ON because the inhibitor is not functional." else           #LacI 
                            if (RIU$data[1,1] == "CA" & RIU$data[1,3] == "+") "Because LacI is 'CA', transcription will be OFF because the inhibitor will always be bound." else      #LacI and LacO
                              if (RIU$data[1,3] == "+") "Because the operator is functional, meaning the inhibitor will not bind when lactose is present, thus transcription is ON" else        #LacO
                                if (RIU$data[1,3] == "-" | RIU$data[1,3] == "CA") "Because the operator is not functional, the inhibitor cannot bind, thus keeping transcription ON" else        #LacO
                                  if (RIU$data[1,2] == "CA") "Because the Promoter is always bound by RNA polymerase, thus keeping transcription will be ON." else        #promoter
                                    if (RIU$data[1,2] == "+") "Because the Promoter is functional, thus  transcription is ON."  else             #lacZ 
                                      ("")
                      )
        )
        HTML(paste("<hr>", str1, "<br/>", str2, "<br/><br/>",str3, "<br/><br/>",str4, "<br/><br/>",str5))
        #HTML(paste("<hr>", str1, str2, str3, str4, str5,  sep = '<br/><br/>'))
        
      })
    }
    else ("") 
  })
    

  
  
  
#KEEPING TRACK AND CHECKING ANSWERS -- DIPLOID  
  #Making a dataframe to keep track of correct answers
  observeEvent(input$refresh, {
    if(input$panels == "Merodiploids")
    {new_row2U = data.frame(matrix(ncol=3, nrow=1))
    names(new_row2U) <- c("Attempt", "correct", "consec")
    new_row2U[1,1] = nrow(report.dataRV_MDU$data)+1
    new_row2U[1,3] = report.dataRV_MDU$data[(nrow(report.dataRV_MDU$data)),3]
    report.dataRV_MDU$data=rbind(report.dataRV_MDU$data, new_row2U)}}
  )  
  
  #Making dataframe to house MERODIPLOID attempts  
  report.data2U <- data.frame(matrix(ncol=3, nrow=0))
  names(report.data2U) <- c("Attempt", "correct", "consec")
  report.data2U[1,1] = 1
  report.dataRV_MDU = reactiveValues(data = report.data2U)
  
  output$tracks2U = DT::renderDataTable(report.dataRV_MDU$data)
  
  report.data <- data.frame(matrix(ncol=3, nrow=0))
  names(report.data) <- c("Attempt", "correct", "consec")
  report.data[1,1] = 1
  report.dataRV = reactiveValues(data = report.data)
  
  output$tracks = DT::renderDataTable(report.dataRV$data)
  
  report.data2 <- data.frame(matrix(ncol=3, nrow=0))
  names(report.data2) <- c("Attempt", "correct", "consec")
  report.data2[1,1] = 1
  report.dataRV_GL = reactiveValues(data = report.data2)
  
  output$tracks2 = DT::renderDataTable(report.dataRV_GL$data)
  
  
  #Counting number in a row correct (diploid)
  observeEvent(input$check, {
    if(input$panels == "Merodiploids"){
      if((input$Zminus_MD == RI_MDU$data[1,11]) & (input$Zplus_MD == RI_MDU$data[1,12]) & (input$Yminus_MD == RI_MDU$data[1,13]) & (input$Yplus_MD == RI_MDU$data[1,14]))
        report.dataRV_MDU$data[(nrow(report.dataRV_MDU$data)),2] = "yes"
      else report.dataRV_MDU$data[(nrow(report.dataRV_MDU$data)),2] = "no"
      
      if(report.dataRV_MDU$data[(nrow(report.dataRV_MDU$data)),2] == "yes")
        report.dataRV_MDU$data[(nrow(report.dataRV_MDU$data)),3] = 1
      else report.dataRV_MDU$data[(nrow(report.dataRV_MDU$data)),3] = 0
      
      if(nrow(report.dataRV_MDU$data)>2 & report.dataRV_MDU$data[(nrow(report.dataRV_MDU$data)),2] == "yes")
        report.dataRV_MDU$data[(nrow(report.dataRV_MDU$data)),3] = report.dataRV_MDU$data[(nrow(report.dataRV_MDU$data))-1,3] + report.dataRV_MDU$data[(nrow(report.dataRV_MDU$data)),3]
      else if(report.dataRV_MDU$data[(nrow(report.dataRV_MDU$data)),2] == "yes")
        report.dataRV_MDU$data[(nrow(report.dataRV_MDU$data)),3] = 1
      else report.dataRV_MDU$data[(nrow(report.dataRV_MDU$data)),3] = 0
      
      output$B = renderText({
        paste("Number in a row correct = ",(report.dataRV_MDU$data[(nrow(report.dataRV_MDU$data)),3]), sep = "")})
    }
    
    
    #Checking answer (diploid)
    
    d1_MD <- c1_MD()
    d2_MD <- c2_MD()
    d3_MD <- c3_MD()
    d4_MD <- c4_MD()
    
    if(input$panels == "Merodiploids")
    {output$value1_MD <- renderText(
      {if(d1_MD == RI_MDU$data[1,11]) "Correct"
        else "Try again"})
    output$value2_MD <- renderText(
      {if(d2_MD == RI_MDU$data[1,12]) "Correct"
        else "Try again"})
    output$value3_MD <- renderText(
      {if(d3_MD == RI_MDU$data[1,13]) "Correct"
        else "Try again"})
    output$value4_MD <- renderText(
      {if(d4_MD == RI_MDU$data[1,14]) "Correct"
        else "Try again"})
    }
    else (NULL)
    
    
    
    if(input$panels == "Merodiploids")
    {if ((input$Zminus_MD == RI_MDU$data[1,11]) && (input$Zplus_MD == RI_MDU$data[1,12]) && (input$Yminus_MD == RI_MDU$data[1,13]) & (input$Yplus_MD == RI_MDU$data[1,14])) 
      output$Explain.Haplo.MD <- renderText("")
    
    
    #Logic of correct answers (diploid)  
    output$Explain.Haplo.MD <- renderUI({
      str1 <- paste("<b> EXPLANATION </b>")
      str2 <- paste("<b> LacZ/-lac: </b>", 
                    (if (RI_MDU$data[1,2] == "-" & RI_MDU$data[1,7] == "-") "Because the Promoters are '-', transcription will be OFF because RNA polymerase cannot bind." else      #promoter
                      if (RI_MDU$data[1,4] == "-" & RI_MDU$data[1,9] == "-") "Because both copies of LacZ are '-', transcription will be OFF because the genes are nonfunctional."     else           #lacZ
                        if (RI_MDU$data[1,2] == "-" & RI_MDU$data[1,4] == "+" & RI_MDU$data[1,9] == "-") "Because the Promotor for wild-type copy of LacZ is nonfunctional and the other copy of LacZ is nonfunctional, transcription will be OFF." else           #Promoter
                          if (RI_MDU$data[1,7] == "-" & RI_MDU$data[1,4] == "-" & RI_MDU$data[1,9] == "+") "Because the Promotor for wild-type copy of LacZ is nonfunctional and the other copy of LacZ is nonfunctional, transcription will be OFF." else           #Promoter
                            if ((RI_MDU$data[1,3] == "-" & RI_MDU$data[1,8] == "-") | (RI_MDU$data[1,3] == "CA" & RI_MDU$data[1,8] == "CA") | (RI_MDU$data[1,3] == "-" & RI_MDU$data[1,8] == "CA") | (RI_MDU$data[1,3] == "CA"& RI_MDU$data[1,8] == "-") & RI_MDU$data[1,2]  == "+" & RI_MDU$data[1,4] == "+") "Because the operator is not functional, the inhibitor cannot bind, thus keeping transcription ON" else        #LacO
                              if ((RI_MDU$data[1,3] == "-" & RI_MDU$data[1,8] == "-") | (RI_MDU$data[1,3] == "CA" & RI_MDU$data[1,8] == "CA") | (RI_MDU$data[1,3] == "-" & RI_MDU$data[1,8] == "CA") | (RI_MDU$data[1,3] == "CA"& RI_MDU$data[1,8] == "-") & RI_MDU$data[1,7]  == "+" & RI_MDU$data[1,9] == "+") "Because the operator is not functional, the inhibitor cannot bind, thus keeping transcription ON" else        #LacO
                                if (RI_MDU$data[1,1] == "-" & RI_MDU$data[1,6] == "-" & RI_MDU$data[1,2]  == "+" & RI_MDU$data[1,3] == "-" & RI_MDU$data[1,4] == "+") "Because the Inhibitor is not functional, transcription will remain ON" else        #LacI
                                  if (RI_MDU$data[1,1] == "-" & RI_MDU$data[1,6] == "-" & RI_MDU$data[1,7]  == "+" & RI_MDU$data[1,8] == "-" & RI_MDU$data[1,9] == "+") "Because the Inhibitor is not functional, transcription will remain ON" else        #LacI
                                    if (RI_MDU$data[1,2] == "+" & (RI_MDU$data[1,3] == "-" | RI_MDU$data[1,3]  == "CA") & RI_MDU$data[1,4]  == "+") "Because the operator for the first copy of LacZ is not functional, the inhibitor cannot bind, thus keeping transcription ON" else        #LacO
                                      if (RI_MDU$data[1,7] == "+" & (RI_MDU$data[1,8] == "-" | RI_MDU$data[1,8]  == "CA") & RI_MDU$data[1,9]  == "+") "Because the operator for the second copy of LacZ is not functional, the inhibitor cannot bind, thus keeping transcription ON" else        #LacO
                                        if (RI_MDU$data[1,1] == "-" & RI_MDU$data[1,6] == "-" & RI_MDU$data[1,2] == "+" & RI_MDU$data[1,4] != "-") "Because the Inhibitor protein is nonfunctional and the first copy of LacZ is functional, transcription remains ON" else        #LacI
                                          if (RI_MDU$data[1,1] == "-" & RI_MDU$data[1,6] == "-" & RI_MDU$data[1,7] == "+" & RI_MDU$data[1,9] != "-") "Because the Inhibitor protein is nonfunctional and the second copy of LacZ is functional, transcription remains ON" else        #LacI
                                            ("")
                    )
      )
      
      str3 <- paste("<b> LacZ/+lac: </b>", 
                    (if (RI_MDU$data[1,2] == "-" & RI_MDU$data[1,7] == "-") "Because the Promoters are '-', transcription will be OFF because RNA polymerase cannot bind." else      #promoter
                      if (RI_MDU$data[1,4] == "-" & RI_MDU$data[1,9] == "-") "Because both copies of LacZ are '-', transcription will be OFF because the genes are nonfunctional."     else           #lacZ
                        if (RI_MDU$data[1,2] == "-" & RI_MDU$data[1,7] == "+" & RI_MDU$data[1,9] == "-") "Because the LacZ under the control of the wild-type promoter is nonfunctional, transcription will be OFF." else           #Promoter
                          if (RI_MDU$data[1,7] == "-" & RI_MDU$data[1,2] == "+" & RI_MDU$data[1,4] == "-") "Because the LacZ under the control of the wild-type promoter is nonfunctional, transcription will be OFF." else           #Promoter  
                            if (RI_MDU$data[1,2] == "-" & RI_MDU$data[1,4] == "+" & RI_MDU$data[1,9] == "-") "Because the Promotor for wild-type copy of LacZ is nonfunctional and the other copy of LacZ is nonfunctional, transcription will be OFF." else           #Promoter
                              if (RI_MDU$data[1,7] == "-" & RI_MDU$data[1,4] == "-" & RI_MDU$data[1,9] == "+") "Because the Promotor for wild-type copy of LacZ is nonfunctional and the other copy of LacZ is nonfunctional, transcription will be OFF." else           #Promoter  
                                ("")
                    )
      )
      
      str4 <- paste("<b> LacY/-lac: </b>", 
                    (if (RI_MDU$data[1,2] == "-" & RI_MDU$data[1,7] == "-") "Because the Promoters are '-', transcription will be OFF because RNA polymerase cannot bind." else      #promoter
                      if (RI_MDU$data[1,5] == "-" & RI_MDU$data[1,10] == "-") "Because both copies of LacY are '-', transcription will be OFF because the genes are nonfunctional."     else           #lacY
                        if (RI_MDU$data[1,2] == "-" & RI_MDU$data[1,5] == "+" & RI_MDU$data[1,10] == "-") "Because the Promotor for wild-type copy of LacY is nonfunctional and the other copy of LacY is nonfunctional, transcription will be OFF." else           #Promoter
                          if (RI_MDU$data[1,7] == "-" & RI_MDU$data[1,5] == "-" & RI_MDU$data[1,10] == "+") "Because the Promotor for wild-type copy of LacY is nonfunctional and the other copy of LacY is nonfunctional, transcription will be OFF." else           #Promoter
                            if ((RI_MDU$data[1,3] == "-" & RI_MDU$data[1,8] == "-") | (RI_MDU$data[1,3] == "CA" & RI_MDU$data[1,8] == "CA") | (RI_MDU$data[1,3] == "-" & RI_MDU$data[1,8] == "CA") | (RI_MDU$data[1,3] == "CA"& RI_MDU$data[1,8] == "-") & RI_MDU$data[1,2]  == "+" & RI_MDU$data[1,5] == "+") "Because the operator is not functional, the inhibitor cannot bind, thus keeping transcription ON" else        #LacO
                              if ((RI_MDU$data[1,3] == "-" & RI_MDU$data[1,8] == "-") | (RI_MDU$data[1,3] == "CA" & RI_MDU$data[1,8] == "CA") | (RI_MDU$data[1,3] == "-" & RI_MDU$data[1,8] == "CA") | (RI_MDU$data[1,3] == "CA"& RI_MDU$data[1,8] == "-") & RI_MDU$data[1,7]  == "+" & RI_MDU$data[1,10] == "+") "Because the operator is not functional, the inhibitor cannot bind, thus keeping transcription ON" else        #LacO
                                if (RI_MDU$data[1,1] == "-" & RI_MDU$data[1,6] == "-" & RI_MDU$data[1,2]  == "+" & RI_MDU$data[1,3] == "-" & RI_MDU$data[1,5] == "+") "Because the Inhibitor is not functional, transcription will remain ON" else        #LacI
                                  if (RI_MDU$data[1,1] == "-" & RI_MDU$data[1,6] == "-" & RI_MDU$data[1,7]  == "+" & RI_MDU$data[1,8] == "-" & RI_MDU$data[1,10] == "+") "Because the Inhibitor is not functional, transcription will remain ON" else        #LacI
                                    if (RI_MDU$data[1,2] == "+" & (RI_MDU$data[1,3] == "-" | RI_MDU$data[1,3]  == "CA") & RI_MDU$data[1,5]  == "+") "Because the operator for the first copy of LacY is not functional, the inhibitor cannot bind, thus keeping transcription ON" else        #LacO
                                      if (RI_MDU$data[1,7] == "+" & (RI_MDU$data[1,8] == "-" | RI_MDU$data[1,8]  == "CA") & RI_MDU$data[1,10]  == "+") "Because the operator for the second copy of LacY is not functional, the inhibitor cannot bind, thus keeping transcription ON" else        #LacO
                                        if (RI_MDU$data[1,1] == "-" & RI_MDU$data[1,6] == "-" & RI_MDU$data[1,2] == "+" & RI_MDU$data[1,5] != "-") "Because the Inhibitor protein is nonfunctional and the first copy of LacY is functional, transcription remains ON" else        #LacI
                                          if (RI_MDU$data[1,1] == "-" & RI_MDU$data[1,6] == "-" & RI_MDU$data[1,7] == "+" & RI_MDU$data[1,10] != "-") "Because the Inhibitor protein is nonfunctional and the second copy of LacY is functional, transcription remains ON" else        #LacI
                                            ("")
                    )
      )
      
      str5 <- paste("<b> LacY/+lac: </b>", 
                    (if (RI_MDU$data[1,2] == "-" & RI_MDU$data[1,7] == "-") "Because the Promoters are '-', transcription will be OFF because RNA polymerase cannot bind." else      #promoter
                      if (RI_MDU$data[1,5] == "-" & RI_MDU$data[1,10] == "-") "Because both copies of LacY are '-', transcription will be OFF because the genes are nonfunctional."     else           #lacY
                        if (RI_MDU$data[1,2] == "-" & RI_MDU$data[1,7] == "+" & RI_MDU$data[1,10] == "-") "Because the LacY under the control of the wild-type promoter is nonfunctional, transcription will be OFF." else           #Promoter
                          if (RI_MDU$data[1,7] == "-" & RI_MDU$data[1,2] == "+" & RI_MDU$data[1,5] == "-") "Because the LacY under the control of the wild-type promoter is nonfunctional, transcription will be OFF." else           #Promoter  
                            if (RI_MDU$data[1,2] == "-" & RI_MDU$data[1,5] == "+" & RI_MDU$data[1,10] == "-") "Because the Promotor for wild-type copy of LacY is nonfunctional and the other copy of LacY is nonfunctional, transcription will be OFF." else           #Promoter
                              if (RI_MDU$data[1,7] == "-" & RI_MDU$data[1,5] == "-" & RI_MDU$data[1,10] == "+") "Because the Promotor for wild-type copy of LacY is nonfunctional and the other copy of LacY is nonfunctional, transcription will be OFF." else           #Promoter  
                                ("")
                    )
      )
      
      HTML(paste("<hr>", str1, "<br/>", str2, "<br/><br/>",str3, "<br/><br/>",str4, "<br/><br/>",str5))
      #HTML(paste("<hr>", str1, str2, str3, str4, str5,  sep = '<br/><br/>'))
      
    })
    }
    else ("")  
  })
  
  
  
  
  
  
  
  
#KEEPING TRACK AND CHECKING ANSWERS -- Lac/glu  
  
  #Making a dataframe to keep track of correct answers
  observeEvent(input$check, {
    
    d1 <- c1()
    
    if(input$panels == "Wildtype operon")
    {output$value1 <- renderText(
      {if(d1 == RI$data[1,3]) "Correct"
        else "Try again"})
    }
    else (NULL)
    
    #adding in new row when check is performed
    if(input$panels == "Wildtype operon")
    {new_row = data.frame(matrix(ncol=3, nrow=1))
    names(new_row) <- c("Attempt", "correct", "consec")
    new_row[1,1] = nrow(report.dataRV$data)+1
    new_row[1,3] = report.dataRV$data[(nrow(report.dataRV$data)),3]
    report.dataRV$data=rbind(report.dataRV$data, new_row)
    }
  })
  
  #Counting number in a row correct (glu/lac)
  observeEvent(input$check, {
    if(input$panels == "Wildtype operon")
    {
      if((input$transx == RI$data[1,3]))
        report.dataRV$data[(nrow(report.dataRV$data)),2] = "yes"
      else report.dataRV$data[(nrow(report.dataRV$data)),2] = "no"
      
      if(report.dataRV$data[(nrow(report.dataRV$data)),2] == "yes")
        report.dataRV$data[(nrow(report.dataRV$data)),3] = 1
      else report.dataRV$data[(nrow(report.dataRV$data)),3] = 0
      
      if(nrow(report.dataRV$data)>2 & report.dataRV$data[(nrow(report.dataRV$data)),2] == "yes")
        report.dataRV$data[(nrow(report.dataRV$data)),3] = report.dataRV$data[(nrow(report.dataRV$data))-1,3] + report.dataRV$data[(nrow(report.dataRV$data)),3]
      else if(report.dataRV$data[(nrow(report.dataRV$data)),2] == "yes")
        report.dataRV$data[(nrow(report.dataRV$data)),3] = 1
      else report.dataRV$data[(nrow(report.dataRV$data)),3] = 0
      
      output$A2 = renderText({
        paste("Number in a row correct = ",(report.dataRV$data[(nrow(report.dataRV$data)),3]), sep = "")})
    }
  })  
  
  
  
  
#KEEPING TRACK AND CHECKING ANSWERS -- Lac/glu + genotype
  
  #Making a dataframe to keep track of correct answers
  observeEvent(input$refresh, {
    if(input$panels == "Operon Regulation")
    {new_row2 = data.frame(matrix(ncol=3, nrow=1))
    names(new_row2) <- c("Attempt", "correct", "consec")
    new_row2[1,1] = nrow(report.dataRV_GL$data)+1
    new_row2[1,3] = report.dataRV_GL$data[(nrow(report.dataRV_GL$data)),3]
    report.dataRV_GL$data=rbind(report.dataRV_GL$data, new_row2)}}
  )  
  
  
  #Button to check answer (Lac/glu + genotype)
  observeEvent(input$check, {  
    
    d1_GL <- c1_GL()
    d2_GL <- c2_GL()
    d3_GL <- c3_GL()
    d4_GL <- c4_GL()
    
    
    if(input$panels == "Operon Regulation")
    {output$value1_env2 <- renderText(
      {if(d1_GL == RI_GL$data[1,7]) "Correct"
        else "Try again"})
    output$value2_env2 <- renderText(
      {if(d2_GL == RI_GL$data[1,8]) "Correct"
        else "Try again"})
    output$value3_env2 <- renderText(
      {if(d3_GL == RI_GL$data[1,9]) "Correct"
        else "Try again"})
    output$value4_env2 <- renderText(
      {if(d4_GL == RI_GL$data[1,10]) "Correct"
        else "Try again"})
    }
    else (NULL)
    
    
    #Logic of correct answers (Lac/glu + genotype)  
    if(input$panels == "Operon Regulation")
    {if ((input$PlusMin_env2 == RI_GL$data[1,7]) & (input$PlusPlus_env2 == RI_GL$data[1,8]) & (input$MinMin_env2 == RI_GL$data[1,9]) & (input$MinPlus_env2 == RI_GL$data[1,10])) output$Explain.OpReg <- renderText("")
    else 
      output$Explain.OpReg <- renderUI({
        str1 <- paste("<b> EXPLANATION </b>")
        str2 <- paste("<b> +glu / -lac: </b>", 
                      (if (RI_GL$data[1,2] == "-") "Because CAP is nonfunctional and unresponsive to glucose and no lactose is present, transcription of LacZ and Lac Y will be off/very low." else      #CAP
                        if (RI_GL$data[1,3] == "-") "Because the promoter is nonfunctional, transcription of LacZ and Lac Y will be off/very low."     else           #promoter
                          if (RI_GL$data[1,1] == "-" | RI_GL$data[1,4] == "-" | RI_GL$data[1,4] == "CA" ) "Because Inhibitor cannot bind and block RNA polymerase, transcription of LacZ and Lac Y will be basal/low." else           #LacI and LacO
                            if (RI_GL$data[1,1] == "+" & RI_GL$data[1,3] == "+" & RI_GL$data[1,4] == "+" & RI_GL$data[1,2] == "+") "Because lactose levels are low and glucose levels are high, transcription of LacZ and Lac Y will be off/very low."  else      #normal
                              ("Low levels of lactose cause transcription of LacZ and Lac Y to be off/very low." )
                      )
        )
        
        str3 <- paste("<b> +glu / +lac: </b>", 
                      (if (RI_GL$data[1,3] == "-") "Because the promoter is nonfunctional, transcription of LacZ and LacY will be off/very low."                #promoter
                       else   ("Glucose levels are high, which prevents CAP activation, but high levels of lactose cause transcription of LacZ and LacY to be basal/low." )        
                      )
        )
        
        str4 <- paste("<b> -glu / -lac: </b>", 
                      (if (RI_GL$data[1,3] == "-") "Because the promoter is nonfunctional, transcription of LacZ and LacY will be off/very low."     else 
                        if (RI_GL$data[1,2] == "-") "Because CAP is nonfunctional and unresponsive to glucose and no lactose is present, transcription of LacZ and LacY will be off/very low."     else           #promoter
                          if (RI_GL$data[1,1] == "-" | RI_GL$data[1,4] == "-" | RI_GL$data[1,4] == "CA" ) "Because Inhibitor cannot bind and block RNA polymerase and glucose levels are low (meaning CAP is activated), transcription of LacZ and LacY will be activated/very high." else           #LacI and LacO
                            if (RI_GL$data[1,1] == "+" & RI_GL$data[1,3] == "+" & RI_GL$data[1,4] == "+" & RI_GL$data[1,2] == "+") "Because lactose levels are low, transcription of LacZ and LacY will be off/very low."  else      #normal
                              ("Low levels of lactose cause transcription of LacZ and Lac Y to be off/very low." )        
                      )
        )
        
        str5 <- paste("<b> -glu / +lac: </b>", 
                      (if (RI_GL$data[1,3] == "-") "Because the promoter is nonfunctional, transcription of LacZ and LacY will be off/very low."     else 
                        if (RI_GL$data[1,2] == "-") "Because CAP is nonfunctional and unresponsive to glucose but lactose is present, transcription of LacZ and LacY will be basal/low."     else           #promoter
                          if (RI_GL$data[1,1] == "-") "Because Inhibitor is nonfunctional and high levels of lactose are present with no glucose, transcription of LacZ and LacY will be activated/high."     else           #promoter
                            ("High levels of lactose and low levels of glucose cause transcription of LacZ and LacY to be activated/high." )        
                      )
        )  
        
        
        
        HTML(paste("<hr>", str1, "<br/>", str2, "<br/><br/>",str3, "<br/><br/>",str4, "<br/><br/>",str5))
        #HTML(paste("<hr>", str1, str2, str3, str4, str5,  sep = '<br/><br/>'))
        
      })
    }
    else ("") 
    
    
    #Number in a row correct (glu/lac + genotype)
    if(input$panels == "Operon Regulation")
    {new_row2 = data.frame(matrix(ncol=3, nrow=1))
    names(new_row2) <- c("Attempt", "correct", "consec")
    new_row2[1,1] = nrow(report.dataRV_GL$data)+1
    new_row2[1,3] = report.dataRV_GL$data[(nrow(report.dataRV_GL$data)),3]
    report.dataRV_GL$data=rbind(report.dataRV_GL$data, new_row2)
    }
  })
  
  observeEvent(input$check, {
    if(input$panels == "Operon Regulation"){
      if((input$MinMin_env2 == RI_GL$data[1,9]) & (input$MinPlus_env2 == RI_GL$data[1,10]) & (input$PlusPlus_env2 == RI_GL$data[1,8]) & (input$PlusMin_env2 == RI_GL$data[1,7]))
        report.dataRV_GL$data[(nrow(report.dataRV_GL$data)),2] = "yes"
      else report.dataRV_GL$data[(nrow(report.dataRV_GL$data)),2] = "no"
      
      if(report.dataRV_GL$data[(nrow(report.dataRV_GL$data)),2] == "yes")
        report.dataRV_GL$data[(nrow(report.dataRV_GL$data)),3] = 1
      else report.dataRV_GL$data[(nrow(report.dataRV_GL$data)),3] = 0
      
      if(nrow(report.dataRV_GL$data)>2 & report.dataRV_GL$data[(nrow(report.dataRV_GL$data)),2] == "yes")
        report.dataRV_GL$data[(nrow(report.dataRV_GL$data)),3] = report.dataRV_GL$data[(nrow(report.dataRV_GL$data))-1,3] + report.dataRV_GL$data[(nrow(report.dataRV_GL$data)),3]
      else if(report.dataRV_GL$data[(nrow(report.dataRV_GL$data)),2] == "yes")
        report.dataRV_GL$data[(nrow(report.dataRV_GL$data)),3] = 1
      else report.dataRV_GL$data[(nrow(report.dataRV_GL$data)),3] = 0
      
      output$B2 = renderText({
        paste("Number in a row correct = ",(report.dataRV_GL$data[(nrow(report.dataRV_GL$data)),3]), sep = "")})
    }
  })      
  
  # output$ID = renderText(paste(c("Unique ID = ", InputID(1))))
  # output$ID2 = renderText(paste(c("Unique ID = ", InputID(1))))
  
  
  
  
  
  #TIMER FOR ACTIVITIES    
  timer <- reactiveVal(0)
  active <- reactiveVal(FALSE)
  
  output$ID <- renderText({
    paste("Timer (minutes): ", timer())
  })
  
  output$ID2 <- renderText({
    paste("Timer (minutes): ", timer())
  }) 
  
  output$ID3 <- renderText({
    paste("Timer (minutes): ", timer())
  })
  
  output$ID4 <- renderText({
    paste("Timer (minutes): ", timer())
  }) 
  
  
  observe({
    invalidateLater(60000, session)
    isolate({
      if(active())
      {
        timer(timer()+1)
      }
    })
  })
  
  
  observeEvent(input$panels != "Tutorial", {active(TRUE)})
  
  
  
  
  
  
  
  
}



shinyApp(ui, server)
#Deployed as Grad_GeneReg3


