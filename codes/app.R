
library(shiny)
library(shinydashboard)
library(readr)
library(readxl)
library(NbClust)
library(clValid)
library(fresh)
library(cluster)
library(wesanderson)
library(later)

###############################
### function for silhouette ###
###############################

sigraph <- function(data) {
  k <- c(2:10)
  nb <- NbClust(data, min.nc = 2, max.nc = 10, index = "silhouette", method = "kmeans")
  s <- as.vector(nb$All.index)
  par(bg = "#FAEFD1")
  plot(k, s,xlab =  "Cluster number k",
       ylab = "Silhouette Score",
       main = "Silhouette Plot", cex.main=1,
       col = "#354823", cex = 0.9 ,
       lty=1 , type="o" , lwd=1, pch=4,
       bty = "l",
       las = 1, cex.axis = 0.8, tcl  = -0.2)
  abline(v=which(s==max(s)) + 1, lwd=1, col="#81A88D", lty="dashed")
}

#############################
### function for ch index ###
#############################

calingraph <- function(data) {
  k <- c(2:10)
  nb <- NbClust(data, min.nc = 2, max.nc = 10, index = "ch", method = "kmeans")
  ch <- as.vector(nb$All.index)
  par(bg = "#FAEFD1")
  plot(k, ch,xlab =  "Cluster number k",
       ylab = "Caliński - Harabasz Score",
       main = "Caliński - Harabasz Plot", cex.main=1,
       col = "#354823", cex = 0.9 ,
       lty=1 , type="o" , lwd=1, pch=4,
       bty = "l",
       las = 1, cex.axis = 0.8, tcl  = -0.2)
  abline(v=which(ch==max(ch)) + 1, lwd=1, col="#81A88D", lty="dashed")
}

#############################
### function for db index ###
#############################

dbgraph <- function(data) {
  k <- c(2:10)
  nb <- NbClust(data, min.nc = 2, max.nc = 10, index = "db", method = "kmeans")
  db <- as.vector(nb$All.index)
  par(bg = "#FAEFD1")
  plot(k, db,xlab =  "Cluster number k",
       ylab = "Davies-Bouldin Score",
       main = "Davies-Bouldin Plot", cex.main=1,
       col = "#354823", cex = 0.9 ,
       lty=1 , type="o" , lwd=1, pch=4,
       bty = "l",
       las = 1, cex.axis = 0.8, tcl  = -0.2)
  abline(v=which(db==min(db)) + 1, lwd=1, col="#81A88D", lty="dashed")
}


###############################
### function for dunn index ###
###############################

dunngraph <- function(data) {
  k <- c(2:10)
  dunnin <- c()
  for (i in 2:10) {
    dunnin[i] <- dunn(distance = dist(data), clusters = kmeans(data, i)$cluster)
  }
  dunnin <- dunnin[2:10]
  par(bg = "#FAEFD1")
  plot(k, dunnin, xlab =  "Cluster number k",
       ylab = "Dunn Index",
       main = "Dunn Plot", cex.main=1,
       col = "#354823", cex = 0.9 ,
       lty=1 , type="o" , lwd=1, pch=4,
       bty = "l",
       las = 1, cex.axis = 0.8, tcl  = -0.2)
  abline(v=which(dunnin==max(dunnin)) + 1, lwd=1, col="#81A88D", lty="dashed")
}

################
### Datasets ###
################

### wine dataset ###
wine <- read_csv("datasets/wine.data", col_names = FALSE)
wine <- wine[-1]
wine <- scale(wine)
### column dataset ###
column3c <- column_3C <- read_table("datasets/column_3C.dat", 
                                    col_names = FALSE)
column <-  column3c[-7]
data.pca <- prcomp(column, center = TRUE, scale. = TRUE)
column <- predict(data.pca)[,1:2]
column <- as.data.frame(column)
### ecoli dataset ###
ecoli <- ecoli <- read_table("datasets/ecoli.data", 
                             col_names = FALSE)
ecoli <- ecoli [-1]
ecoli <- ecoli [-8]
ecoli <- scale(ecoli)

### iris ###
iris <- get(data("iris", package = "datasets"))
iris <- iris[1:4]
data.pca <- prcomp(iris, center = TRUE, scale. = TRUE)
summary(data.pca)
iris <- predict(data.pca)[,1:2]
iris <- as.data.frame(iris)

### haberman ###
haberman <- read_csv("datasets/haberman.data", 
                     col_names = FALSE)
haberman <- scale(haberman[1:3])

### wdbc ###
wdbc <- read_csv("datasets/wdbc.data", col_names = FALSE)
wdbc <- wdbc[1:12]
wdbc <- wdbc[3:12]
data.pca <- prcomp(wdbc, center = TRUE, scale. = TRUE) # 2 pc is ok
wdbc <- predict(data.pca)[,1:2]
wdbc <- as.data.frame(wdbc)

### breast_tissue ###
breast_tissue <- read_excel("datasets/breastissue.xlsx")
breast_tissue <- breast_tissue[-1] 
data.pca <- prcomp(breast_tissue, center = TRUE, scale. = TRUE)
breast_tissue <- predict(data.pca)[,1:2]
breast_tissue <- as.data.frame(breast_tissue)

###################
## appendicitis ###
###################

appen <- read_csv("datasets/appendicitis.csv")
appen <- appen[-8]
data.pca <- prcomp(appen, center = TRUE, scale. = TRUE)
appen <- as.data.frame(predict(data.pca)[,1:3])

################
### userknow ###
################

userknow <-  userknow <- read_excel("datasets/userknow.xlsx")
userknow <-  userknow[-6]
userknow <- scale(userknow)

##############################################################################

#############
### Theme ###
#############

mytheme <- create_theme(
  adminlte_color(
    light_blue = "#02401B"
  ),
  adminlte_sidebar(
    width = "400px",
    dark_bg = "#FAEFD1",
    dark_hover_bg = "#81A88D",
    dark_color = "#273046"
  ),
  adminlte_global(
    content_bg = "#FAEFD1",
    box_bg = "#81A88D", 
    info_box_bg = "#02401B"
  )
)

###########
### App ###
###########

ui <- dashboardPage(skin = "blue",
                    dashboardHeader(title = "k determination",  tags$li(class = "dropdown",
                                                                        tags$a(href="https://github.com/ozturkfemre/kdetermination.app/blob/main/README.md", target="_blank", 
                                                                               tags$img(height = "30px", alt="SNAP Logo", src="https://t3.ftcdn.net/jpg/01/08/24/94/360_F_108249487_w8UD8P6YOYuh6Pr9NUw08juskfxpIBk4.jpg")
                                                                        ))),
                    dashboardSidebar(
                      sidebarMenu(
                        menuItem("wine", tabName = "wine", icon = icon("glass", lib = "glyphicon")),
                        menuItem("column", tabName = "column",icon = icon("columns")),
                        menuItem("ecoli", tabName = "ecoli", icon = icon("certificate", lib = "glyphicon")),
                        menuItem("iris", tabName = "iris", icon = icon("leaf", lib = "glyphicon")),
                        menuItem("haberman", tabName = "haberman", icon = icon("transfer", lib = "glyphicon")),
                        menuItem("wdbc", tabName = "wdbc", icon = icon("unchecked", lib = "glyphicon")),
                        menuItem("breast_tissue", tabName = "breast_tissue", icon = icon("window-close")),
                        menuItem("appendicitis", tabName = "appen", icon = icon("plus")),
                        menuItem("userknow", tabName = "userknow", icon = icon("user-secret"))
                      )),
                    dashboardBody(
                      use_theme(mytheme),
                      tabItems(
                        tabItem("wine",
                                box(plotOutput("silhouette_plot1"), width = 8, collapsible = T, collapsed = T, title = "Silhouette Plot", status = "primary", solidHeader = T),
                                box(plotOutput("ch_plot1"), width = 8, collapsible = T, collapsed = T, title = "Calinski-Harabasz Plot", status = "primary", solidHeader = T),
                                box(plotOutput("db_plot1"), width = 8, collapsible = T, collapsed = T, title = "Davies-Bouldin Plot", status = "primary", solidHeader = T),
                                box(plotOutput("dunn_plot1"), width = 8, collapsible = T, collapsed = T, title = "Dunn Plot", status = "primary", solidHeader = T)),
                        tabItem("column",
                                box(plotOutput("silhouette_plot2"), width = 8, collapsible = T, collapsed = T, title = "Silhouette Plot", status = "primary", solidHeader = T),
                                box(plotOutput("ch_plot2"), width = 8, collapsible = T, collapsed = T, title = "Calinski-Harabasz Plot", status = "primary", solidHeader = T),
                                box(plotOutput("db_plot2"), width = 8, collapsible = T, collapsed = T, title = "Davies-Bouldin Plot", status = "primary", solidHeader = T),
                                box(plotOutput("dunn_plot2"), width = 8, collapsible = T, collapsed = T, title = "Dunn Plot", status = "primary", solidHeader = T)),
                        tabItem("ecoli",
                                box(plotOutput("silhouette_plot3"), width = 8, collapsible = T, collapsed = T, title = "Silhouette Plot", status = "primary", solidHeader = T),
                                box(plotOutput("ch_plot3"), width = 8, collapsible = T, collapsed = T, title = "Calinski-Harabasz Plot", status = "primary", solidHeader = T),
                                box(plotOutput("db_plot3"), width = 8, collapsible = T, collapsed = T, title = "Davies-Bouldin Plot", status = "primary", solidHeader = T),
                                box(plotOutput("dunn_plot3"), width = 8, collapsible = T, collapsed = T, title = "Dunn Plot", status = "primary", solidHeader = T)),
                        tabItem("iris",
                                box(plotOutput("silhouette_plot4"), width = 8, collapsible = T, collapsed = T, title = "Silhouette Plot", status = "primary", solidHeader = T),
                                box(plotOutput("ch_plot4"), width = 8, collapsible = T, collapsed = T, title = "Calinski-Harabasz Plot", status = "primary", solidHeader = T),
                                box(plotOutput("db_plot4"), width = 8, collapsible = T, collapsed = T, title = "Davies-Bouldin Plot", status = "primary", solidHeader = T),
                                box(plotOutput("dunn_plot4"), width = 8, collapsible = T, collapsed = T, title = "Dunn Plot", status = "primary", solidHeader = T)),
                        tabItem("haberman",
                                box(plotOutput("silhouette_plot5"), width = 8, collapsible = T, collapsed = T, title = "Silhouette Plot", status = "primary", solidHeader = T),
                                box(plotOutput("ch_plot5"), width = 8, collapsible = T, collapsed = T, title = "Calinski-Harabasz Plot", status = "primary", solidHeader = T),
                                box(plotOutput("db_plot5"), width = 8, collapsible = T, collapsed = T, title = "Davies-Bouldin Plot", status = "primary", solidHeader = T),
                                box(plotOutput("dunn_plot5"), width = 8, collapsible = T, collapsed = T, title = "Dunn Plot", status = "primary", solidHeader = T)),
                        tabItem("wdbc",
                                box(plotOutput("silhouette_plot6"), width = 8, collapsible = T, collapsed = T, title = "Silhouette Plot", status = "primary", solidHeader = T),
                                box(plotOutput("ch_plot6"), width = 8, collapsible = T, collapsed = T, title = "Calinski-Harabasz Plot", status = "primary", solidHeader = T),
                                box(plotOutput("db_plot6"), width = 8, collapsible = T, collapsed = T, title = "Davies-Bouldin Plot", status = "primary", solidHeader = T),
                                box(plotOutput("dunn_plot6"), width = 8, collapsible = T, collapsed = T, title = "Dunn Plot", status = "primary", solidHeader = T)),
                        tabItem("breast_tissue",
                                box(plotOutput("silhouette_plot7"), width = 8, collapsible = T, collapsed = T, title = "Silhouette Plot", status = "primary", solidHeader = T),
                                box(plotOutput("ch_plot7"), width = 8, collapsible = T, collapsed = T, title = "Calinski-Harabasz Plot", status = "primary", solidHeader = T),
                                box(plotOutput("db_plot7"), width = 8, collapsible = T, collapsed = T, title = "Davies-Bouldin Plot", status = "primary", solidHeader = T),
                                box(plotOutput("dunn_plot7"), width = 8, collapsible = T, collapsed = T, title = "Dunn Plot", status = "primary", solidHeader = T)),
                        tabItem("appen",
                                box(plotOutput("silhouette_plot8"), width = 8, collapsible = T, collapsed = T, title = "Silhouette Plot", status = "primary", solidHeader = T),
                                box(plotOutput("ch_plot8"), width = 8, collapsible = T, collapsed = T, title = "Calinski-Harabasz Plot", status = "primary", solidHeader = T),
                                box(plotOutput("db_plot8"), width = 8, collapsible = T, collapsed = T, title = "Davies-Bouldin Plot", status = "primary", solidHeader = T),
                                box(plotOutput("dunn_plot8"), width = 8, collapsible = T, collapsed = T, title = "Dunn Plot", status = "primary", solidHeader = T)),
                        tabItem("userknow",
                                box(plotOutput("silhouette_plot9"), width = 8, collapsible = T, collapsed = T, title = "Silhouette Plot", status = "primary", solidHeader = T),
                                box(plotOutput("ch_plot9"), width = 8, collapsible = T, collapsed = T, title = "Calinski-Harabasz Plot", status = "primary", solidHeader = T),
                                box(plotOutput("db_plot9"), width = 8, collapsible = T, collapsed = T, title = "Davies-Bouldin Plot", status = "primary", solidHeader = T),
                                box(plotOutput("dunn_plot9"), width = 8, collapsible = T, collapsed = T, title = "Dunn Plot", status = "primary", solidHeader = T))
                      )
                    )
                    
)


server <- function(input,output){
  ### wine ###
  output$silhouette_plot1 <- renderPlot({
    sigraph(wine)
  })
  output$ch_plot1 <- renderPlot({
    calingraph(wine)
  })
  output$db_plot1 <- renderPlot({
    dbgraph(wine)
  })
  output$dunn_plot1 <- renderPlot({
    dunngraph(wine)
  })
  ### column ###
  output$silhouette_plot2 <- renderPlot({
    sigraph(column)
  })
  output$ch_plot2 <- renderPlot({
    calingraph(column)
  })
  output$db_plot2 <- renderPlot({
    dbgraph(column)
  })
  output$dunn_plot2 <- renderPlot({
    dunngraph(column)
  })
  ### ecoli ###
  
  output$silhouette_plot3 <- renderPlot({
    sigraph(ecoli)
  })
  output$ch_plot3 <- renderPlot({
    calingraph(ecoli)
  })
  output$db_plot3 <- renderPlot({
    dbgraph(ecoli)
  })
  output$dunn_plot3 <- renderPlot({
    dunngraph(ecoli)
  })
  
  ############
  ### iris ###
  ############
  
  output$silhouette_plot4 <- renderPlot({
    sigraph(iris)
  })
  output$ch_plot4 <- renderPlot({
    calingraph(iris)
  })
  output$db_plot4 <- renderPlot({
    dbgraph(iris)
  })
  output$dunn_plot4 <- renderPlot({
    dunngraph(iris)
  })
  
  ################
  ### haberman ###
  ################
  
  output$silhouette_plot5 <- renderPlot({
    sigraph(haberman)
  })
  output$ch_plot5 <- renderPlot({
    calingraph(haberman)
  })
  output$db_plot5 <- renderPlot({
    dbgraph(haberman)
  })
  output$dunn_plot5 <- renderPlot({
    dunngraph(haberman)
  })
  
  ############
  ### wdbc ###
  ############
  
  output$silhouette_plot6 <- renderPlot({
    sigraph(wdbc)
  })
  output$ch_plot6 <- renderPlot({
    calingraph(wdbc)
  })
  output$db_plot6 <- renderPlot({
    dbgraph(wdbc)
  })
  output$dunn_plot6 <- renderPlot({
    dunngraph(wdbc)
  })
  
  #####################
  ### breast_tissue ###
  #####################
  
  output$silhouette_plot7 <- renderPlot({
    sigraph(breast_tissue)
  })
  output$ch_plot7 <- renderPlot({
    calingraph(breast_tissue)
  })
  output$db_plot7 <- renderPlot({
    dbgraph(breast_tissue)
  })
  output$dunn_plot7 <- renderPlot({
    dunngraph(breast_tissue)
  })
  
  #############
  ### appen ###
  #############
  
  output$silhouette_plot8 <- renderPlot({
    sigraph(appen)
  })
  output$ch_plot8 <- renderPlot({
    calingraph(appen)
  })
  output$db_plot8 <- renderPlot({
    dbgraph(appen)
  })
  output$dunn_plot8 <- renderPlot({
    dunngraph(appen)
  })
  
  ################
  ### userknow ###
  ################
  
  output$silhouette_plot9 <- renderPlot({
    sigraph(userknow)
  })
  output$ch_plot9 <- renderPlot({
    calingraph(userknow)
  })
  output$db_plot9 <- renderPlot({
    dbgraph(userknow)
  })
  output$dunn_plot9 <- renderPlot({
    dunngraph(userknow)
  })
  
}



runApp(shinyApp(ui,server),launch.browser=T) 

