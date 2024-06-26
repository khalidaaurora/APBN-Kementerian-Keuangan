library(shiny) # Basic Shiny R
library(shinydashboard) # Shiny Dashboard
library(shinydashboardPlus)
library(tidyverse) # Data preprocessing / wrangling
library(ggplot2) # Graph
library(summarytools) # Summary
library(readxl) # Read excel data
library(DT)
library(dplyr)
library(dashboardthemes)
library(bslib)
library(fmsb)
library(plotly)
library(shinyWidgets)
library(htmltools)

#setwd("D:/kuliah/SIM/FP UAS")
datapepaj=read_xlsx("DATA FP SIM.xlsx",sheet=1)
datapnbp=read_xlsx("DATA FP SIM.xlsx",sheet=2)
colnames(datapepaj)=c('Uraian','2018','2019','2020','2021','2022','2023')
databfung=read_xlsx("DATA FP SIM.xlsx",sheet=3)
colnames(databfung)=c('Fungsi','2018','2019','2020','2021','2022','2023')
databjen=read_xlsx("DATA FP SIM.xlsx",sheet=4)
colnames(databjen)=c('Uraian','2018','2019','2020','2021','2022','2023')
databkl=read_xlsx("DATA FP SIM.xlsx",sheet=5)
colnames(databkl)=c('Kode BA','Kementerian Negara / Lembaga','2018','2019','2020','2021','2022','2023')
databiaya=read_xlsx("DATA FP SIM.xlsx",sheet=6)
colnames(databiaya)=c('Uraian','2018','2019','2020','2021','2022','2023')
datapepajp=read_xlsx("DATA FP SIM.xlsx",sheet=7)
databfbaru=read_xlsx("DATA FP SIM.xlsx",sheet=8)
databjbaru=read_xlsx("DATA FP SIM.xlsx",sheet=9)
databklbaru=read_xlsx("DATA FP SIM.xlsx",sheet=10)
data1 <- read_excel("dari pajak.xlsx");data1
data2 <- read_excel("tahun.xlsx");data2
data3 <- read_excel("milihtahun.xlsx");data3
data4 <- read_excel("tahun1.xlsx");data4
data5<- read_excel("DARI NON PAJAK.xlsx");data5
data6 <- read_excel("milihtahun1.xlsx");data6
data7 <- read_excel("milih3.xlsx");data7


header=dashboardHeader(title="Kementerian Keuangan",
                       titleWidth = 300,
                       dropdownMenu(headerText = "Kontak Penyusun Melalui Media Sosial Berikut! ^^",type = 'message',
                                     icon = icon("instagram"),
                                     messageItem(
                                       from = "Khalida Aurora Amanda Putri",
                                       message = "Instagram",
                                       icon = icon("instagram"),
                                       href = "https://WWW.instagram.com/khalidaaurora"
                                     ),
                                     messageItem(
                                       from = "Muhammad Iqbal Febriko",
                                       message = "Instagram",
                                       icon = icon("instagram"),
                                       href = "https://WWW.instagram.com/iqbalfebrikoo"
                                     )
                       ))

sidebar <- dashboardSidebar(
  width = 200,
  sidebarMenu(
    menuItem("Beranda", tabName = "Beranda", icon = icon("home")),
    menuItem("Deskripsi", tabName = "Deskripsi", icon = icon("bars")),
    menuItem("Database", tabName = "Database", icon = icon("database")),
    menuItem("Pendapatan Negara", tabName = "PN", icon = icon("money-bills")),
    menuItem("Belanja Negara", tabName = "BN", icon = icon("cart-shopping"),
             menuSubItem("Belanja Pemerintah Pusat Menurut Jenis",tabName = "BPJ"),
             menuSubItem("Belanja Pemerintah Pusat Menurut Fungsi",tabName = "BPF"),
             menuSubItem("Belanja Kementerian / Lembaga Negara",tabName = "BKL")),
    menuItem("Pembiayaan", tabName = "Pembiayaan", icon = icon("hand-holding-dollar")),
    menuItem("Penyusun", tabName = "Penyusun", icon = icon("person"))
  ))


body <- dashboardBody(
  customTheme <- shinyDashboardThemeDIY(
    ### general
    appFontFamily = "Arial",
    appFontColor = "rgb(12,28,52)"
    ,primaryFontColor = "rgb(12,28,52)"
    ,bodyBackColor = "rgb(227,234,252)"
    ### header
    ,logoBackColor = "rgb(149,90,161)"
    ,headerButtonBackColor = "rgb(238,238,238)"
    ,headerButtonIconColor = "#1D267D"
    ,headerButtonBackColorHover = "rgb(210,210,210)"
    ,headerButtonIconColorHover = "rgb(0,0,0)"
    ,headerBackColor = "rgb(238,238,238)"
    ,headerBoxShadowColor = "#aaaaaa"
      ,headerBoxShadowSize = "2px 2px 2px"
    ### sidebar
    ,sidebarBackColor = cssGradientThreeColors(
      direction = "down"
      ,colorStart = "rgb(149,90,161)"
      ,colorMiddle = "rgb(149,105,163)"
      ,colorEnd = "rgb(3,22,56)"
      ,colorStartPos = 0
      ,colorMiddlePos = 50
      ,colorEndPos = 100
    )
    ,sidebarPadding = 0
    ,sidebarMenuBackColor = "transparent"
    ,sidebarMenuPadding = 0
    ,sidebarMenuBorderRadius = 0
    ,sidebarShadowRadius = "3px 5px 5px"
    ,sidebarShadowColor = "#aaaaaa"
      ,sidebarUserTextColor = "rgb(255,255,255)"
    ,sidebarSearchBackColor = "rgb(55,72,80)"
    ,sidebarSearchIconColor = "rgb(153,153,153)"
    ,sidebarSearchBorderColor = "rgb(55,72,80)"
    ,sidebarTabTextColor = "rgb(255,255,255)"
    ,sidebarTabTextSize = 15
    ,sidebarTabBorderStyle = "none none solid none"
    ,sidebarTabBorderColor = "rgb(149,105,163)"
    ,sidebarTabBorderWidth = 1
    ,sidebarTabBackColorSelected = cssGradientThreeColors(
      direction = "right"
      ,colorStart = "rgba(149,105,163,1)"
      ,colorMiddle = "rgba(213,195,218,1)"
      ,colorEnd = "rgba(234,225,237,1)"
      ,colorStartPos = 0
      ,colorMiddlePos = 60
      ,colorEndPos = 100
    )
    ,sidebarTabTextColorSelected = "rgb(0,0,0)"
    ,sidebarTabRadiusSelected = "0px 20px 20px 0px"
    ,sidebarTabBackColorHover = cssGradientThreeColors(
      direction = "right"
      ,colorStart = "rgba(149,105,163,1)"
      ,colorMiddle = "rgba(213,195,218,1)"
      ,colorEnd = "rgba(234,225,237,1)"
      ,colorStartPos = 0
      ,colorMiddlePos = 60
      ,colorEndPos = 100
    )
    ,sidebarTabTextColorHover = "rgb(50,50,50)"
    ,sidebarTabBorderStyleHover = "none none solid none"
    ,sidebarTabBorderColorHover = "rgb(75,126,151)"
    ,sidebarTabBorderWidthHover = 1
    ,sidebarTabRadiusHover = "0px 20px 20px 0px"
    ### boxes
    ,boxBackColor = "rgb(255,255,255)"
    ,boxBorderRadius = 5
    ,boxShadowSize = "0px 1px 1px"
    ,boxShadowColor = "rgba(0,0,0,.1)"
    ,boxTitleSize = 16
    ,boxDefaultColor = "rgb(210,214,220)"
    ,boxPrimaryColor = "rgba(255,255,255,1)"
    ,boxInfoColor = "rgb(210,214,220)"
    ,boxSuccessColor = "rgba(0,255,213,1)"
    ,boxWarningColor = "rgb(244,156,104)"
    ,boxDangerColor = "rgb(255,88,55)"
    ,tabBoxTabColor = "rgb(255,255,255)"
    ,tabBoxTabTextSize = 14
    ,tabBoxTabTextColor = "rgb(0,0,0)"
    ,tabBoxTabTextColorSelected = "rgb(0,0,0)"
    ,tabBoxBackColor = "rgb(255,255,255)"
    ,tabBoxHighlightColor = "rgba(255,255,255,1)"
    ,tabBoxBorderRadius = 5
    ### inputs
    ,buttonBackColor = "rgb(245,245,245)"
    ,buttonTextColor = "rgb(0,0,0)"
    ,buttonBorderColor = "rgb(200,200,200)"
    ,buttonBorderRadius = 5
    ,buttonBackColorHover = "rgb(235,235,235)"
    ,buttonTextColorHover = "rgb(100,100,100)"
    ,buttonBorderColorHover = "rgb(200,200,200)"
    ,textboxBackColor = "rgb(255,255,255)"
    ,textboxBorderColor = "rgb(200,200,200)"
    ,textboxBorderRadius = 5
    ,textboxBackColorSelect = "rgb(245,245,245)"
    ,textboxBorderColorSelect = "rgb(200,200,200)"
    ### tables
    ,tableBackColor = "rgb(255,255,255)"
    ,tableBorderColor = "rgb(240,240,240)"
    ,tableBorderTopSize = 1
    ,tableBorderRowSize = 1
  ),
  tabItems(
    tabItem(tabName = "Beranda",
            titlePanel(
              h1(strong("SELAMAT DATANG DI KEMENTERIAN KEUANGAN!"),
                 style="text-align:center;")),
            br(),
            carousel(width = 12,
                     id = "mycarousel",
                     carouselItem(
                       caption = "Logo",
                       tags$img(src = "https://upload.wikimedia.org/wikipedia/commons/thumb/7/73/Logo_kementerian_keuangan_republik_indonesia.png/1200px-Logo_kementerian_keuangan_republik_indonesia.png")
                     ),
                     carouselItem(
                       caption = "Menteri Keuangan",
                       tags$img(src = "https://static.republika.co.id/uploads/images/inpicture_slide/menteri-keuangan-sri-mulyani-indrawati-berharap-masyarakat-turut-mengawasi_230224204350-709.jpg")
                     ))),
    tabItem(tabName = "Deskripsi",
            div(h1(strong("Deskripsi")),style="text-align: center;"),
            box(title = strong("Tujuan"),width = 12,solidHeader = T,
                p("Dashboard ini dapat digunakan untuk melihat anggaran pendapatan, 
                  anggaran belanja, dan pembiayaan yang dilakukan oleh Kementerian Keuangan.")),
            box(title = strong("Informasi"), width = 12,solidHeader = T,
                p(style="text-align: justify;",strong("Pendapatan Negara dari Pajak : "),"Pendapatan negara yang berasal dari PPh Migas,
                  PPh Nonmigas, pajak pertambahan nilai, pajak bumi dan bangunan, cukai, pajak lainnya, bea masuk, dan bea keluar."),
                p(style="text-align: justify;",strong("Pendapatan Negara Bukan Pajak : "),"Pendapatan negara yang berasal dari
                  penerimaan sumber daya alam (SDA), bagian laba BUMN, PNBP lainnya, dan pendapatan Badan Layanan Umum (BLU). 
                  Penerimaan SDA terjadi menjadi migas dan non migas.Untuk SDA migas yaitu minyak bumi dan gas alam (migas). Untuk 
                  SDA non migas yaitu pertambangan mineral dan batu bara, kehutanan, perikanan, dan panas bumi"),
                p(style="text-align: justify;",strong("Belanja Pemerintah Pusat Berdasarkan Jenis : "),"terbagi menjadi 
                  belanja pegawai, belanja barang, belanja modal, pembayaran bunga utang dalam negeri, 
                  pembayaran belanja utang luar negeri, subsidi energi, subsidi non energi, belanja hibah, 
                  bantuan sosial, dan belanja lain-lain."),
                p(style="text-align: justify;",strong("Belanja Pemerintah Pusat Berdasarkan Fungsi :"), "pelayanan umum, pertahanan, 
                  ketertiban dan keamanan, ekonomi, perlindungan lingkungan hidup, perumahan dan fasilitas umum, 
                  kesehatan, pariwisata, agama, pendidikan, dan perlindungan sosial."),
                p(style="text-align: justify;",strong("Belanja Negara Berdasarkan Kementerian : "),"terbagi menjadi 89 kementerian dan lembaga di Indonesia seperti 
                  Majelis Permusyawaratan Rakyat (MPR), Kementerian Sekretariat Negara, Badan Intelijen Negara, dan lainnya."),
                p(style="text-align: justify;",strong("Pembiayaan Anggaran : "),"terdiri atas variabel Surat Berharga Negara (SBN) 
                  dan pinjaman dalam negeri serta luar negeri (termasuk dalam pembiayaan utang), 
                  pembiayaan investasi, pemberian pinjaman, kewajiban penjaminan, dan pembiayaan lainnya."))),
    tabItem(
      tabName = "PN",
      tabsetPanel(
        tabPanel("Perpajakan",
                 box(
                   title = "Penerimaan Perpajakan Tahun 2023", status = "primary", solidHeader = T,
                   collapsible = T, width = 6,
                   plotOutput(outputId = "graph_1", height = 500) ),
                 
                 box(
                   title = "Jumlah Penerimaan Perpajakan 6 Tahun Terakhir", status = "primary", solidHeader = T,
                   collapsible = T, width = 6,
                   plotOutput(outputId = "graph_2", height = 500) ),
                 
                 fluidRow(
                   tags$head(
                     tags$style("
              #kab1 ~ .selectize-control .option:nth-child(odd) {
              background-color: rgba(91, 114, 182, 1);}

              #kab1 ~ .selectize-control .option:nth-child(even) {
              background-color: rgba(254, 253, 253, 1);}
              ")),
                   box(title = "Pendapatan Pajak Tiap Tahun",width = 12,
                       status = "primary",solidHeader = TRUE,
                       selectInput("kab1","Pilih Tahun", unique(data3$`Tahun`))
                   )
                 ),
                 fluidRow(
                   box(title = "Pendapatan Tiap Sumber Pajak",width = 12,
                       icon = icon("line-chart"),
                       collapsible = TRUE,status = "primary",solidHeader = TRUE,
                       plotlyOutput("krims")))
                 ),
        tabPanel("Non Pajak (PNBP)",
                 box(title = "Penerimaan non-Pajak Tahun 2023", status = "primary", solidHeader = T,
                     collapsible = T, width = 6,
                     plotOutput(outputId = "graph_4", height = 500)),
                 box(title = "Jumlah Penerimaan non-Pajak 6 Tahun Terakhir", status = "primary", solidHeader = T,
                     collapsible = T, width = 6,
                     plotOutput(outputId = "graph_3", height = 500)),
                 
                 fluidRow(
                   tags$head(
                     tags$style("
              #kab2 ~ .selectize-control .option:nth-child(odd) {
              background-color: rgba(91, 114, 182, 1);}

              #kab2 ~ .selectize-control .option:nth-child(even) {
              background-color: rgba(254, 253, 253, 1);}
              ")),
                   box(title = "Pendapatan non-Pajak Tiap Tahun",width = 12,
                       status = "primary",solidHeader = TRUE,
                       selectInput("kab2","Pilih Tahun", unique(data6$`Tahun`))
                   )
                 ),
                 fluidRow(
                   box(title = "Pendapatan Tiap Sumber non-Pajak",width = 12,
                       icon = icon("line-chart"),
                       collapsible = TRUE,status = "primary",solidHeader = TRUE,
                       plotlyOutput("krims2")))
                 
        ))),
    tabItem(
      tabName = "Pembiayaan", 
      fluidRow(
        valueBox(871723.20,"Jumlah Pembiayaan Tahun 2021",
                 color = "green",icon = icon("usd")),
        valueBox(732247.30,"Jumlah Pembiayaan Tahun 2022",
                 color = "blue",icon = icon("usd")),
        valueBox(598151.40,"Jumlah Pembiayaan Tahun 2023",color = "maroon",icon = icon("usd"))
      ),
      fluidRow(
        tags$head(
          tags$style("
              #kab3 ~ .selectize-control .option:nth-child(odd) {
              background-color: rgba(91, 114, 182, 1);}

              #kab3 ~ .selectize-control .option:nth-child(even) {
              background-color: rgba(254, 253, 253, 1);}
              ")),
        box(title = "Pilih Pembiayaan",width = 12,
            status = "primary",solidHeader = TRUE,
            selectInput("kab3","Pilih ", unique(data7$`Sumber`))
        )
      ),
      fluidRow(
        box(title = "Perkembangan Pembiayaan Berdasarkan Tujuan Tiap Tahun",width = 12,
            icon = icon("line-chart"),
            collapsible = TRUE,status = "primary",solidHeader = TRUE,
            plotlyOutput("krims3")))),
    tabItem(tabName = "Database",
            tabsetPanel(
              navbarMenu("Pendapatan Negara",
                         tabPanel("Penerimaan Perpajakan",
                                  div(h3(strong("Penerimaan Perpajakan")),style="text-align: center;"),
                                  div(h4(strong("(dalam miliar)")),style="text-align: center;"),
                                  DTOutput("table1")),
                         tabPanel("Penerimaan Negara Bukan Pajak",
                                  div(h3(strong("Penerimaan Negara Bukan Pajak")),style="text-align: center;"),
                                  div(h4(strong("(dalam miliar)")),style="text-align: center;"),
                                  DTOutput("table2"))),
              navbarMenu("Belanja Negara",
                         tabPanel("Belanja Menurut Jenis",
                                  div(h3(strong("Belanja Pemerintah Pusat Menurut Jenis")),style="text-align: center;"),
                                  div(h4(strong("(dalam miliar)")),style="text-align: center;"),
                                  DTOutput("table3")),
                         tabPanel("Belanja Menurut Fungsi",
                                  div(h3(strong("Belanja Pemerintah Pusat Menurut Fungsi")),style="text-align: center;"),
                                  div(h4(strong("(dalam miliar)")),style="text-align: center;"),
                                  DTOutput("table4")),
                         tabPanel("Belanja Kementerian / Lembaga Negara",
                                  div(h3(strong("Belanja Kementerian / Lembaga Negara")),style="text-align: center;"),
                                  div(h4(strong("(dalam miliar)")),style="text-align: center;"),
                                  DTOutput("table5"))),
              tabPanel("Pembiayaan",
                       div(h3(strong("Pembiayaan Pemerintah Pusat")),style="text-align: center;"),
                       div(h4(strong("(dalam miliar)")),style="text-align: center;"),
                       DTOutput("table6"))
            )),
    tabItem(tabName="BPF",
            fluidRow(
              valueBoxOutput("vbtf",width=4),
              valueBoxOutput("vbpf",width=4),
              valueBoxOutput("vbft",width=4),
              box(title = "Pilih Tahun",width = 6,
                  solidHeader = TRUE,selectInput("Pil1","Pilih Tahun",
                                                 choices = colnames(databfung)[2:7],
                                                 selected='2023')),
              box(title = "Pilih Fungsi",width = 6,
                  solidHeader = TRUE,selectInput("Pil2","Pilih Fungsi",
                                                 choices = unique(databfbaru$`Fungsi`))),
              box(title = "Belanja Pemerintah Pusat Menurut Fungsi", width = 6,
                  plotlyOutput("bfungt")),
              box(title = "Perkembangan Belanja Negara Tiap Fungsi", width = 6,
                  plotlyOutput("bfungl")))
              ),
    tabItem(tabName = "BPJ",
            fluidRow(
              valueBoxOutput("vbtj",width=4),
              valueBoxOutput("vbpj",width=4),
              valueBoxOutput("vbjt",width=4),
              box(title = "Pilih Tahun",width = 6,
                  solidHeader = TRUE,selectInput("Pil5","Pilih Tahun",
                                                 choices = colnames(databfung)[2:7],
                                                 selected = '2023')),
              box(title = "Pilih Jenis",width = 6,
                  solidHeader = TRUE,selectInput("Pil3","Pilih Jenis",
                                                 choices = unique(databjbaru$`Jenis`))),
              box(title = "Belanja Pemerintah Pusat Menurut Jenis", width = 6,
                  plotlyOutput("bjent")),
              box(title = "Perkembangan Belanja Negara Tiap Jenis", width = 6,
                  plotlyOutput("bjenl")))
            ),
    tabItem(tabName = "BKL",
            fluidRow(
              box(title = "Pilih Kementerian / Lembaga Negara",width = 12,
                  solidHeader = TRUE,selectInput("Pil4","Pilih Kementerian / Lembaga Negara",
                                                 choices = unique(databklbaru$`Kementerian Negara/Lembaga`),
                                                 selected = 'MAHKAMAH AGUNG')),
              box(title = "Logo Kementerian / Lembaga Negara",width = 12,collapsed = FALSE,
                  collapsible = TRUE,status = "primary",solidHeader = TRUE,
                  div(imageOutput("logokl"),style="text-align: center;",
                      style = "margin-bottom:-165px;")),
              box(title = "Belanja Kementerian / Lembaga Negara", width = 12,
                  plotlyOutput("bkll")))),
    tabItem(tabName = "Penyusun",
      div(h1("PENYUSUN"),style="text-align: center;"),
      box(width = 6,
          status = NULL,
          div(imageOutput("iqbal"),style="text-align: center;",
              style = "margin-bottom:-180px;"),
          div(strong("Muhammad Iqbal Febriko"),style="text-align: center;"),
          div(strong("5003211074"),style="text-align: center;"),
          div(strong("Hiduplah Seperti Larry"),style="text-align: center;")
      ),
      box(width = 6,
          status = NULL,
          div(imageOutput("osa"),style="text-align: center;",
              style = "margin-bottom:-180px;"),
          div(strong("Khalida Aurora A. P."),style="text-align: center;"),
          div(strong("5003211022"),style="text-align: center;"),
          div(strong("Hiduplah Seperti Gerry"),style="text-align: center;")
      ),
      box(width = 12,
          div(strong("Departemen Statistika"),style="text-align: center;"),
          div(strong("Fakultas Sains dan Analitika Data"),style="text-align: center;"),
          div(strong("Institut Teknologi Sepuluh Nopember Surabaya"),style="text-align: center;"),
          div(strong("2022"),style="text-align: center;")
      )
    )
  ))

ui=dashboardPage(header = header,
                 sidebar = sidebar,
                 body = body)

server <- function(input, output, session){
  output$table1<-renderDT(datapepaj)
  output$table2<-renderDT(datapnbp)
  output$table3<-renderDT(databjen)
  output$table4<-renderDT(databfung)
  output$table5<-renderDT(databkl)
  output$table6<-renderDT(databiaya)
  bar1 = reactive({
    validate(
      need(input$Pil1 !="", "Pilih Tahun")
    )  
    filterbar1 = databfbaru%>%
      filter(`Tahun` %in% input$Pil1)
    plot_ly(x=filterbar1$Fungsi, y=filterbar1$Nilai, type = "bar",
            marker = list(color = 'rgb(12,28,52)'),
            textposition = "outside",
            insidetextfont = list(color = "white"), hoverinfo = "text",
            text = ~paste(filterbar1$Nilai, "Miliar"),
            showlegend = T)%>%
      layout(xaxis = list(showgrid = F, zeroline = F, showticklabels = T),
             yaxis = list(showgrid = F, zeroline = F, showticklabels = T),
             autosize = T)
  })
  output$bfungt = renderPlotly({
    bar1()
  })
  line1 = reactive({
    validate(
      need(input$Pil2 !="", "Pilih Fungsi")
    )
    databfbaru%>%
      filter(`Fungsi` %in% input$Pil2)%>%
      ggplot(aes(x=as.character(Tahun), y=`Nilai`, group=`Fungsi`)) +
      geom_point(colour="#EF562D",size=1.5) + geom_line(colour="#EF562D",linewidth=0.8) +
      labs(
        x="Tahun",
        y="Nilai (miliar)"
      )+theme_light()
  })
  output$bfungl = renderPlotly({
    line1()
  })
  bar2 = reactive({
    validate(
      need(input$Pil5 !="", "Pilih Tahun")
    )  
    filterbar1 = databjbaru%>%
      filter(`Tahun` %in% input$Pil5)
    plot_ly(x=filterbar1$Jenis, y=filterbar1$Nilai, type = "bar",
            marker = list(color = 'rgb(12,28,52)'),
            textposition = "outside",
            insidetextfont = list(color = "white"), hoverinfo = "text",
            text = ~paste(filterbar1$Nilai, "Miliar"),
            showlegend = T)%>%
      layout(xaxis = list(showgrid = F, zeroline = F, showticklabels = T),
             yaxis = list(showgrid = F, zeroline = F, showticklabels = T),
             autosize = T)
  })
  output$bjent = renderPlotly({
    bar2()
  })
  line2 = reactive({
    validate(
      need(input$Pil2 !="", "Pilih Jenis")
    )
    databjbaru%>%
      filter(`Jenis` %in% input$Pil3)%>%
      ggplot(aes(x=as.character(Tahun), y=`Nilai`, group=`Jenis`)) +
      geom_point(colour="#EF562D",size=1.5) + geom_line(colour="#EF562D",linewidth=0.8) +
      labs(
        x="Tahun",
        y="Nilai (miliar)"
      )+theme_light()
  })
  output$bjenl = renderPlotly({
    line2()
  })
  line3 = reactive({
    validate(
      need(input$Pil4 !="", "Pilih Kementerian / Lembaga Negara")
    )
    databklbaru%>%
      filter(`Kementerian Negara/Lembaga` %in% input$Pil4)%>%
      ggplot(aes(x=as.character(Tahun), y=`Nilai`, group=`Kementerian Negara/Lembaga`)) +
      geom_point(colour="#EF562D",size=1.5) + geom_line(colour="#EF562D",linewidth=0.8) +
      labs(
        x="Tahun",
        y="Nilai (miliar)"
      )+theme_light()
  })
  output$bkll = renderPlotly({
    line3()
  })
  output$vbtf<-renderValueBox({
    valueBox(
      value = 
        if(input$Pil1=="2018"){tags$p("1.455.324,7 Miliar", style = "font-size: 85%;")
        } else if (input$Pil1=="2019"){tags$p("1.496.313,8 Miliar", style = "font-size: 85%;")
        } else if (input$Pil1=="2020"){tags$p("1.832.950,9 Miliar", style = "font-size: 85%;")
        } else if (input$Pil1=="2021"){tags$p("2.000.703,8 Miliar", style = "font-size: 85%;")
        } else if (input$Pil1=="2022"){tags$p("2.370.023,1 Miliar", style = "font-size: 85%;")
        } else if (input$Pil1=="2023"){tags$p("2.230.025,3 Miliar", style = "font-size: 85%;")
        },
      subtitle = 
        if(input$Pil1=="2018"){
          "Total Belanja Tahun 2018"
        }
      else if (input$Pil1=="2019"){
        "Total Belanja Tahun 2019"
      }
      else if (input$Pil1=="2020"){
        "Total Belanja Tahun 2020"
      }
      else if (input$Pil1=="2021"){
        "Total Belanja Tahun 2021"
      }
      else if (input$Pil1=="2022"){
        "Total Belanja Tahun 2022"
      }
      else if (input$Pil1=="2023"){
        "Total Belanja Tahun 2023"
      },
      icon=icon("cart-shopping"),
      color="light-blue"
    )
  })
  output$vbpf<-renderValueBox({
    valueBox(
      value = 
        if(input$Pil1=="2018"){tags$p("1,5%", style = "font-size: 85%;")
        } else if (input$Pil1=="2019"){tags$p("2,82%", style = "font-size: 85%;")
        } else if (input$Pil1=="2020"){tags$p("22,5%", style = "font-size: 85%;")
        } else if (input$Pil1=="2021"){tags$p("9,15%", style = "font-size: 85%;")
        } else if (input$Pil1=="2022"){tags$p("18,46%", style = "font-size: 85%;")
        } else if (input$Pil1=="2023"){tags$p("-5,91%", style = "font-size: 85%;")
        },
      subtitle = 
        if(input$Pil1=="2018"){
          "Pertumbuhan Belanja Tahun 2018"
        }
      else if (input$Pil1=="2019"){
        "Pertumbuhan Belanja Tahun 2019"
      }
      else if (input$Pil1=="2020"){
        "Pertumbuhan Belanja Tahun 2020"
      }
      else if (input$Pil1=="2021"){
        "Pertumbuhan Belanja Tahun 2021"
      }
      else if (input$Pil1=="2022"){
        "Pertumbuhan Belanja Tahun 2022"
      }
      else if (input$Pil1=="2023"){
        "Pertumbuhan Belanja Tahun 2023"
      },
      icon=icon("chart-line"),
      color="light-blue"
    )
  })
  output$vbft<-renderValueBox({
    valueBox(
      value = 
        if(input$Pil1=="2018"){tags$p("Ekonomi", style = "font-size: 85%;")
        } else if (input$Pil1=="2019"){tags$p("Pelayanan Umum", style = "font-size: 85%;")
        } else if (input$Pil1=="2020"){tags$p("Pelayanan Umum", style = "font-size: 85%;")
        } else if (input$Pil1=="2021"){tags$p("Pelayanan Umum", style = "font-size: 85%;")
        } else if (input$Pil1=="2022"){tags$p("Ekonomi", style = "font-size: 85%;")
        } else if (input$Pil1=="2023"){tags$p("Pelayanan Umum", style = "font-size: 85%;")
        },
      subtitle = 
        if(input$Pil1=="2018"){
          "Fungsi dengan Belanja Terbanyak Tahun 2018"
        }
      else if (input$Pil1=="2019"){
        "Fungsi dengan Belanja Terbanyak Tahun 2019"
      }
      else if (input$Pil1=="2020"){
        "Fungsi dengan Belanja Terbanyak Tahun 2020"
      }
      else if (input$Pil1=="2021"){
        "Fungsi dengan Belanja Terbanyak Tahun 2021"
      }
      else if (input$Pil1=="2022"){
        "Fungsi dengan Belanja Terbanyak Tahun 2022"
      }
      else if (input$Pil1=="2023"){
        "Fungsi dengan Belanja Terbanyak Tahun 2023"
      },
      icon=icon("up-long"),
      color="light-blue"
    )
  })
  output$vbjt<-renderValueBox({
    valueBox(
      value = 
        if(input$Pil5=="2018"){tags$p("Belanja Barang", style = "font-size: 85%;")
        } else if (input$Pil5=="2019"){tags$p("Belanja Pegawai", style = "font-size: 85%;")
        } else if (input$Pil5=="2020"){tags$p("Belanja Barang", style = "font-size: 85%;")
        } else if (input$Pil5=="2021"){tags$p("Belanja Barang", style = "font-size: 85%;")
        } else if (input$Pil5=="2022"){tags$p("Belanja Lain-lain", style = "font-size: 85%;")
        } else if (input$Pil5=="2023"){tags$p("Belanja Pegawai", style = "font-size: 85%;")
        },
      subtitle = 
        if(input$Pil5=="2018"){
          "Jenis dengan Belanja Terbanyak Tahun 2018"
        }
      else if (input$Pil5=="2019"){
        "Jenis dengan Belanja Terbanyak Tahun 2019"
      }
      else if (input$Pil5=="2020"){
        "Jenis dengan Belanja Terbanyak Tahun 2020"
      }
      else if (input$Pil5=="2021"){
        "Jenis dengan Belanja Terbanyak Tahun 2021"
      }
      else if (input$Pil5=="2022"){
        "Jenis dengan Belanja Terbanyak Tahun 2022"
      }
      else if (input$Pil5=="2023"){
        "Jenis dengan Belanja Terbanyak Tahun 2023"
      },
      icon=icon("up-long"),
      color="light-blue"
    )
  })
  output$vbtj<-renderValueBox({
    valueBox(
      value = 
        if(input$Pil5=="2018"){tags$p("1.455.324,7 Miliar", style = "font-size: 85%;")
        } else if (input$Pil5=="2019"){tags$p("1.496.313,8 Miliar", style = "font-size: 85%;")
        } else if (input$Pil5=="2020"){tags$p("1.832.950,9 Miliar", style = "font-size: 85%;")
        } else if (input$Pil5=="2021"){tags$p("2.000.703,8 Miliar", style = "font-size: 85%;")
        } else if (input$Pil5=="2022"){tags$p("2.370.023,1 Miliar", style = "font-size: 85%;")
        } else if (input$Pil5=="2023"){tags$p("2.230.025,3 Miliar", style = "font-size: 85%;")
        },
      subtitle = 
        if(input$Pil5=="2018"){
          "Total Belanja Tahun 2018"
        }
      else if (input$Pil5=="2019"){
        "Total Belanja Tahun 2019"
      }
      else if (input$Pil5=="2020"){
        "Total Belanja Tahun 2020"
      }
      else if (input$Pil5=="2021"){
        "Total Belanja Tahun 2021"
      }
      else if (input$Pil5=="2022"){
        "Total Belanja Tahun 2022"
      }
      else if (input$Pil5=="2023"){
        "Total Belanja Tahun 2023"
      },
      icon=icon("cart-shopping"),
      color="light-blue"
    )
  })
  output$vbpj<-renderValueBox({
    valueBox(
      value = 
        if(input$Pil5=="2018"){tags$p("1,5%", style = "font-size: 85%;")
        } else if (input$Pil5=="2019"){tags$p("2,82%", style = "font-size: 85%;")
        } else if (input$Pil5=="2020"){tags$p("22,5%", style = "font-size: 85%;")
        } else if (input$Pil5=="2021"){tags$p("9,15%", style = "font-size: 85%;")
        } else if (input$Pil5=="2022"){tags$p("18,46%", style = "font-size: 85%;")
        } else if (input$Pil5=="2023"){tags$p("-5,91%", style = "font-size: 85%;")
        },
      subtitle = 
        if(input$Pil5=="2018"){
          "Pertumbuhan Belanja Tahun 2018"
        }
      else if (input$Pil5=="2019"){
        "Pertumbuhan Belanja Tahun 2019"
      }
      else if (input$Pil5=="2020"){
        "Pertumbuhan Belanja Tahun 2020"
      }
      else if (input$Pil5=="2021"){
        "Pertumbuhan Belanja Tahun 2021"
      }
      else if (input$Pil5=="2022"){
        "Pertumbuhan Belanja Tahun 2022"
      }
      else if (input$Pil5=="2023"){
        "Pertumbuhan Belanja Tahun 2023"
      },
      icon=icon("chart-line"),
      color="light-blue"
    )
  })
  output$logokl<-renderImage({
    if(input$Pil4=="MAJELIS PERMUSYAWARATAN RAKYAT"){
      list(src="WWW/MPR.png",height = 220, width = 160)
    }
    else if(input$Pil4=="DEWAN PERWAKILAN RAKYAT"){
      list(src="WWW/DPR.png",height = 220, width = 160)
    }
    else if(input$Pil4=="BADAN PEMERIKSA KEUANGAN"){
      list(src="WWW/BPK.png",height = 220, width = 160)
    }
    else if(input$Pil4=="MAHKAMAH AGUNG"){
      list(src="WWW/MAgung.png",height = 220, width = 160)
    }
    else if(input$Pil4=="KEJAKSAAN REPUBLIK INDONESIA"){
      list(src="WWW/kejaksaan.png",height = 220, width = 160)
    }
    else if(input$Pil4=="KEMENTERIAN SEKRETARIAT NEGARA"){
      list(src="WWW/setneg.png",height = 220, width = 160)
    }
    else if(input$Pil4=="KEMENTERIAN DALAM NEGERI"){
      list(src="WWW/dagri.png",height = 220, width = 160)
    }
    else if(input$Pil4=="KEMENTERIAN LUAR NEGERI"){
      list(src="WWW/luneg.png",height = 220, width = 160)
    }
    else if(input$Pil4=="KEMENTERIAN PERTAHANAN"){
      list(src="WWW/kemenhan.png",height = 220, width = 160)
    }
    else if(input$Pil4=="KEMENTERIAN HUKUM DAN HAK ASASI MANUSIA"){
      list(src="WWW/kumham.png",height = 220, width = 160)
    }
    else if(input$Pil4=="KEMENTERIAN KEUANGAN"){
      list(src="WWW/kemenkeu.png",height = 220, width = 160)
    }
    else if(input$Pil4=="KEMENTERIAN PERTANIAN"){
      list(src="WWW/tani.png",height = 220, width = 160)
    }
    else if(input$Pil4=="KEMENTERIAN PERINDUSTRIAN"){
      list(src="WWW/indus.png",height = 220, width = 160)
    }
    else if(input$Pil4=="KEMENTERIAN ENERGI DAN SUMBER DAYA MINERAL"){
      list(src="WWW/energi.png",height = 220, width = 160)
    }
    else if(input$Pil4=="KEMENTERIAN PERHUBUNGAN"){
      list(src="WWW/hubung.png",height = 220, width = 160)
    }
    else if(input$Pil4=="KEMENTERIAN PENDIDIKAN, KEBUDAYAAN, RISET, DAN TEKNOLOGI"){
      list(src="WWW/dikbud.png",height = 220, width = 160)
    }
    else if(input$Pil4=="KEMENTERIAN KESEHATAN"){
      list(src="WWW/kemenkes.png",height = 220, width = 160)
    }
    else if(input$Pil4=="KEMENTERIAN AGAMA"){
      list(src="WWW/menag.png",height = 220, width = 160)
    }
    else if(input$Pil4=="KEMENTERIAN KETENAGAKERJAAN"){
      list(src="WWW/naker.png",height = 220, width = 160)
    }
    else if(input$Pil4=="KEMENTERIAN SOSIAL"){
      list(src="WWW/kemensos.png",height = 220, width = 160)
    }
    else if(input$Pil4=="KEMENTERIAN LINGKUNGAN HIDUP DAN KEHUTANAN"){
      list(src="WWW/lihut.png",height = 220, width = 160)
    }
    else if(input$Pil4=="KEMENTERIAN KELAUTAN DAN PERIKANAN"){
      list(src="WWW/laut.png",height = 220, width = 160)
    }
    else if(input$Pil4=="KEMENTERIAN PEKERJAAN UMUM DAN PERUMAHAN RAKYAT"){
      list(src="WWW/kerum.png",height = 220, width = 160)
    }
    else if(input$Pil4=="KEMENTERIAN KOORDINATOR BIDANG POLITIK, HUKUM DAN KEAMANAN"){
      list(src="WWW/polhukam.png",height = 220, width = 160)
    }
    else if(input$Pil4=="KEMENTERIAN KOORDINATOR BIDANG PEREKONOMIAN"){
      list(src="WWW/koreko.png",height = 220, width = 160)
    }
    else if(input$Pil4=="KEMENTERIAN KOORDINATOR BIDANG PEMBANGUNAN MANUSIA DAN KEBUDAYAAN"){
      list(src="WWW/kemenkopmk.png",height = 220, width = 160)
    }
    else if(input$Pil4=="KEMENTERIAN PARIWISATA DAN EKONOMI KREATIF/BADAN PARIWISATA DAN EKONOMI KREATIF"){
      list(src="WWW/kemenparekraf.png",height = 220, width = 160)
    }
    else if(input$Pil4=="KEMENTERIAN BADAN USAHA MILIK NEGARA"){
      list(src="WWW/bumn.png",height = 220, width = 160)
    }
    else if(input$Pil4=="KEMENTERIAN RISET DAN TEKNOLOGI/BADAN RISET DAN INOVASI NASIONAL *)"){
      list(src="WWW/ristekbrin.png",height = 220, width = 160)
    }
    else if(input$Pil4=="KEMENTERIAN KOPERASI DAN USAHA KECIL DAN MENENGAH"){
      list(src="WWW/kopukm.png",height = 220, width = 160)
    }
    else if(input$Pil4=="KEMENTERIAN PEMBERDAYAAN PEREMPUAN DAN PERLINDUNGAN ANAK"){
      list(src="WWW/daper.png",height = 220, width = 160)
    }
    else if(input$Pil4=="KEMENTERIAN PENDAYAGUNAAN APARATUR NEGARA DAN REFORMASI BIROKRASI"){
      list(src="WWW/daapneg.png",height = 220, width = 160)
    }
    else if(input$Pil4=="BADAN INTELIJEN NEGARA"){
      list(src="WWW/BIN.png",height = 220, width = 160)
    }
    else if(input$Pil4=="BADAN SIBER DAN SANDI NEGARA"){
      list(src="WWW/BSSN.png",height = 220, width = 160)
    }
    else if(input$Pil4=="DEWAN KETAHANAN NASIONAL"){
      list(src="WWW/dketan.png",height = 220, width = 160)
    }
    else if(input$Pil4=="BADAN PUSAT STATISTIK"){
      list(src="WWW/BPS.png",height = 220, width = 160)
    }
    else if(input$Pil4=="KEMENTERIAN PERENCANAAN PEMBANGUNAN NASIONAL/BAPPENAS"){
      list(src="WWW/bappenas.png",height = 220, width = 160)
    }
    else if(input$Pil4=="KEMENTERIAN AGRARIA DAN TATA RUANG/BPN"){
      list(src="WWW/BPN.png",height = 220, width = 160)
    }
    else if(input$Pil4=="PERPUSTAKAAN NASIONAL REPUBLIK INDONESIA"){
      list(src="WWW/perpusnas.png",height = 220, width = 160)
    }
    else if(input$Pil4=="KEMENTERIAN KOMUNIKASI DAN INFORMATIKA"){
      list(src="WWW/kominfo.png",height = 220, width = 160)
    }
    else if(input$Pil4=="KEPOLISIAN NEGARA REPUBLIK INDONESIA"){
      list(src="WWW/polisi.png",height = 220, width = 160)
    }
    else if(input$Pil4=="BADAN PENGAWAS OBAT DAN MAKANAN"){
      list(src="WWW/BPOM.png",height = 220, width = 160)
    }
    else if(input$Pil4=="LEMBAGA KETAHANAN NASIONAL"){
      list(src="WWW/tanhana.png",height = 220, width = 160)
    }
    else if(input$Pil4=="BADAN KOORDINASI PENANAMAN MODAL"){
      list(src="WWW/BKPM.png",height = 220, width = 160)
    }
    else if(input$Pil4=="BADAN NARKOTIKA NASIONAL"){
      list(src="WWW/BNN.png",height = 220, width = 160)
    }
    else if(input$Pil4=="KEMENTERIAN DESA, PEMBANGUNAN DAERAH TERTINGGAL, DAN TRANSMIGRASI"){
      list(src="WWW/desa.png",height = 220, width = 160)
    }
    else if(input$Pil4=="BADAN KEPENDUDUKAN DAN KELUARGA BERENCANA NASIONAL"){
      list(src="WWW/bkkbn.png",height = 220, width = 160)
    }
    else if(input$Pil4=="KOMISI NASIONAL HAK ASASI MANUSIA"){
      list(src="WWW/komnasham.png",height = 220, width = 160)
    }
    else if(input$Pil4=="BADAN METEOROLOGI, KLIMATOLOGI DAN GEOFISIKA"){
      list(src="WWW/BMKG.png",height = 220, width = 160)
    }
    else if(input$Pil4=="KOMISI PEMILIHAN UMUM"){
      list(src="WWW/KPU.png",height = 220, width = 160)
    }
    else if(input$Pil4=="MAHKAMAH KONSTITUSI"){
      list(src="WWW/Mkons.png",height = 220, width = 160)
    }
    else if(input$Pil4=="PUSAT PELAPORAN DAN ANALISIS TRANSAKSI KEUANGAN"){
      list(src="WWW/ppatk.png",height = 220, width = 160)
    }
    else if(input$Pil4=="LEMBAGA ILMU PENGETAHUAN INDONESIA *)"){
      list(src="WWW/lipi.png",height = 220, width = 160)
    }
    else if(input$Pil4=="BADAN TENAGA NUKLIR NASIONAL *)"){
      list(src="WWW/batanuklir.png",height = 220, width = 160)
    }
    else if(input$Pil4=="BADAN PENGKAJIAN DAN PENERAPAN TEKNOLOGI *)"){
      list(src="WWW/BPPT.png",height = 220, width = 160)
    }
    else if(input$Pil4=="LEMBAGA PENERBANGAN DAN ANTARIKSA NASIONAL *)"){
      list(src="WWW/lapan.png",height = 220, width = 160)
    }
    else if(input$Pil4=="BADAN INFORMASI GEOSPASIAL"){
      list(src="WWW/bigeo.png",height = 220, width = 160)
    }
    else if(input$Pil4=="BADAN STANDARISASI NASIONAL"){
      list(src="WWW/BSN.png",height = 220, width = 160)
    }
    else if(input$Pil4=="BADAN PENGAWAS TENAGA NUKLIR"){
      list(src="WWW/bapeten.png",height = 220, width = 160)
    }
    else if(input$Pil4=="LEMBAGA ADMINISTRASI NEGARA"){
      list(src="WWW/LANRI.png",height = 220, width = 160)
    }
    else if(input$Pil4=="ARSIP NASIONAL REPUBLIK INDONESIA"){
      list(src="WWW/ANRI.png",height = 220, width = 160)
    }
    else if(input$Pil4=="BADAN KEPEGAWAIAN NEGARA"){
      list(src="WWW/BKN.png",height = 220, width = 160)
    }
    else if(input$Pil4=="BADAN PENGAWASAN KEUANGAN DAN PEMBANGUNAN"){
      list(src="WWW/bpkp.png",height = 220, width = 160)
    }
    else if(input$Pil4=="KEMENTERIAN PERDAGANGAN"){
      list(src="WWW/dagang.png",height = 220, width = 160)
    }
    else if(input$Pil4=="KEMENTERIAN PEMUDA DAN OLAH RAGA"){
      list(src="WWW/kemenpora.png",height = 220, width = 160)
    }
    else if(input$Pil4=="KOMISI PEMBERANTASAN KORUPSI"){
      list(src="WWW/KPK.png",height = 220, width = 160)
    }
    else if(input$Pil4=="DEWAN PERWAKILAN DAERAH"){
      list(src="WWW/DPD.png",height = 220, width = 160)
    }
    else if(input$Pil4=="KOMISI YUDISIAL"){
      list(src="WWW/yudis.png",height = 220, width = 160)
    }
    else if(input$Pil4=="BADAN NASIONAL PENANGGULANGAN BENCANA"){
      list(src="WWW/BNPB.png",height = 220, width = 160)
    }
    else if(input$Pil4=="BADAN PELINDUNGAN PEKERJA MIGRAN INDONESIA (BP2MI)"){
      list(src="WWW/BP2MI.png",height = 220, width = 160)
    }
    else if(input$Pil4=="LEMBAGA KEBIJAKAN PENGADAAN BARANG JASA PEMERINTAH"){
      list(src="WWW/LKPP.png",height = 220, width = 160)
    }
    else if(input$Pil4=="BADAN SAR NASIONAL"){
      list(src="WWW/basarnas.png",height = 220, width = 160)
    }
    else if(input$Pil4=="KOMISI PENGAWAS PERSAINGAN USAHA"){
      list(src="WWW/KPPU.png",height = 220, width = 160)
    }
    else if(input$Pil4=="BADAN PENGEMBANGAN WILAYAH SURAMADU"){
      list(src="WWW/BPWS.png",height = 220, width = 160)
    }
    else if(input$Pil4=="OMBUDSMAN REPUBLIK INDONESIA"){
      list(src="WWW/ombudsman.png",height = 220, width = 160)
    }
    else if(input$Pil4=="BADAN NASIONAL PENGELOLA PERBATASAN"){
      list(src="WWW/BNPP.png",height = 220, width = 160)
    }
    else if(input$Pil4=="BADAN PENGUSAHAAN KAWASAN PERDAGANGAN BEBAS DAN PELABUHAN BEBAS BATAM"){
      list(src="WWW/BPBATAM.png",height = 220, width = 160)
    }
    else if(input$Pil4=="BADAN NASIONAL PENANGGULANGAN TERORISME"){
      list(src="WWW/BNPT.png",height = 220, width = 160)
    }
    else if(input$Pil4=="SEKRETARIAT KABINET"){
      list(src="WWW/setkab.png",height = 220, width = 160)
    }
    else if(input$Pil4=="BADAN PENGAWAS PEMILIHAN UMUM"){
      list(src="WWW/bawaslu.png",height = 220, width = 160)
    }
    else if(input$Pil4=="LEMBAGA PENYIARAN PUBLIK RADIO REPUBLIK INDONESIA"){
      list(src="WWW/RRI.png",height = 220, width = 160)
    }
    else if(input$Pil4=="LEMBAGA PENYIARAN PUBLIK TELEVISI REPUBLIK INDONESIA"){
      list(src="WWW/TVRI.png",height = 220, width = 160)
    }
    else if(input$Pil4=="BADAN PENGUSAHAAN KAWASAN PERDAGANGAN BEBAS & PELABUHAN BEBAS SABANG"){
      list(src="WWW/sabang.png",height = 220, width = 160)
    }
    else if(input$Pil4=="BADAN KEAMANAN LAUT"){
      list(src="WWW/BKL.png",height = 220, width = 160)
    }
    else if(input$Pil4=="KEMENTERIAN KOORDINATOR BIDANG KEMARITIMAN DAN INVESTASI"){
      list(src="WWW/maritim.png",height = 220, width = 160)
    }
    else if(input$Pil4=="BADAN EKONOMI KREATIF"){
      list(src="WWW/bekraf.png",height = 220, width = 160)
    }
    else if(input$Pil4=="BADAN PEMBINAAN IDEOLOGI PANCASILA"){
      list(src="WWW/BPIP.png",height = 220, width = 160)
    }
    else if(input$Pil4=="LEMBAGA PERLINDUNGAN SAKSI DAN KORBAN"){
      list(src="WWW/LPSK.png",height = 220, width = 160)
    }
    else if(input$Pil4=="BADAN RISET DAN INOVASI NASIONAL"){
      list(src="WWW/BRIN.png",height = 220, width = 160)
    }else{}
  },deleteFile = FALSE)
  
  output$iqbal <- renderImage({
    list(src="WWW/iqbal.jpeg",height = 200, width = 150)
  },deleteFile = F)
  
  output$osa <- renderImage({
    list(src="WWW/osa.png",height = 200, width = 150)
  },deleteFile = F)
  output$graph_1 = renderPlot({ ggplot(data1) +
      aes(x = `Sumber Pajak`, y = `2023`) +labs(
        x="Sumber Pajak",
        y="Nilai (Miliar)"
      )+
      geom_col(fill = "#112446") +
      theme_classic()})
  
  output$graph_2= renderPlot({ggplot(data2) +
      aes(
        x = TAHUN,
        y = `JUMLAH PENDAPATAN NEGARA DARI PAJAK (M)`
      ) +labs(
        x="Tahun",
        y="Jumlah Pendapatan Negara dari Pajak (Miliar)"
      )+
      geom_line(colour = "#EF562D") +
      theme_classic()})
  output$krims= renderPlotly({
    pilih2 <- input$kab1
    adk<- data3 %>%
      filter(Tahun==pilih2); adk
    ggplot(data3$pilih2) +
      aes(x = data3$jumlah, y = data3$`Sumber Pajak`) +
      labs(
        x="Nilai (triliun)",
        y="Sumber Pajak"
      )+
      geom_col(fill = "#112446") +
      theme_minimal()
  })
  
  output$graph_3=renderPlot({ggplot(data4) +
      aes(
        x = TAHUN,
        y = `JUMLAH PENDAPATAN NEGARA DARI NON PAJAK (M)`
      ) +labs(
        x="Tahun",
        y="Jumlah Pendapatan Negara dari Non-Pajak (Miliar)"
      )+
      geom_line(colour = "#EF562D") +
      theme_minimal()})
  
  output$graph_4=renderPlot({ggplot(data5) +
      aes(x = `Sumber Pajak`, y = `2023`) +
      labs(
        x="Sumber Pajak",
        y="Nilai (miliar)"
      )+
      geom_col(fill = "#112446") +
      theme_minimal()})
  
  output$krims2= renderPlotly({
    pilih1 <- input$kab2
    adkt<- data6 %>%
      filter(Tahun==pilih1); adkt
    ggplot(data6$pilih1) +
      aes(x = data6$jumlah, y = data6$`Sumber Pajak`) +labs(
        x="Nilai (triliun)",
        y="Sumber non-Pajak"
      )+
      geom_col(fill = "#112446") +
      theme_minimal()
  })
  
  line4 = reactive({
    validate(
      need(input$kab3 !="", "Pilih Sumber")
    )
    data7%>%
      filter(`Sumber` %in% input$kab3)%>%
      ggplot(aes(x=as.character(Tahun), y=`Jumlah`, group=`Sumber`)) +
      geom_point(colour="#EF562D",size=1.5) + geom_line(colour="#EF562D",linewidth=0.8) +
      labs(
        x="Tahun",
        y="Nilai (Triliun)"
      )+theme_light()
  })
  output$krims3 = renderPlotly({
    line4()
  })
  
  }
shinyApp(ui, server)




