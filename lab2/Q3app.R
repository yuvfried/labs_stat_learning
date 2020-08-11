#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(RColorBrewer)

# K-means Algorithm
# helpers:
initial_centroids <- function(data,k){
  init_means <- data[sample(nrow(data), k), ]
  init_means <- data.frame(init_means, row.names = 1:k)
  return(init_means)
}

l2_dist <- function(point, centroid){
  return(sum((point-centroid)^2))
}

# function implements assginment step
# centroids is df whose nrow=k and ncol=ncol(data)
assignment <- function(data,centroids){
  k <- nrow(centroids)
  # compute distance for between any pair of observation-centroid
  dists <- matrix(nrow=nrow(data),ncol=nrow(centroids))
  for (j in 1:k){
    dists[,j] <- apply(data,1, function(x) l2_dist(x,centroids[j,]))
  }
  # assign to the cluster which's centroid is closest
  return(apply(dists,1,which.min))
}

# function implements update step
update_centroids <- function(data, clusters){
  k <- length(unique(clusters))
  centroids <- aggregate(data,
                         by=list(clusters),FUN=mean)[,-1]
  return(centroids)
}

# implementation of K-means
#centroids is a df of init_centroids
my_kmeans <- function(data, centroids){
  i=0
  while (TRUE) {
    clusters <- assignment(data,centroids)
    new_centroids <- update_centroids(data, clusters)
    if (all(new_centroids==centroids)){
      return(list(centroids=centroids, clusters=clusters))
    } else {
      centroids <- new_centroids
    }
  }
  
}


# Data Pre-Processing
min_max_scaling <- function(feature_vector){
  mini <- min(feature_vector)
  maxi <- max(feature_vector)
  return((feature_vector - mini)/ (maxi-mini))
}
normalization <- function(df, eps){
  # remove close to zero
  df <- df[,colMeans(df) > eps]
  # scaling
  df <- apply(df,2,min_max_scaling)
  # centerize
  # fucntion to center from:
  # https://www.gastonsanchez.com/visually-enforced/how-to/2014/01/15/Center-data-in-R/
  df <- df - rep(colMeans(df), rep.int(nrow(df), ncol(df)))
  
  return(df)
}

# Load Data
med_dat = read.delim("gtex.gct",skip = 2 , row.names=c(1), header = TRUE)
genes = med_dat[,1]
med_dat = med_dat [,-1]
tissues = colnames(med_dat)
df <- t(med_dat)
df_norm <- normalization(df, exp(-7))
pca <- prcomp(df_norm)
score_vectors <- pca$x



# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("K-means of Genes"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          sliderInput("k",
                      "Number of Clusters:",
                      min = 1,
                      max = 53,
                      value = 3),
          sliderInput("first_pcs",
                      "Number of first PC for k-means:",
                      min = 1,
                      max = 53,
                      value = 3),
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("scatterPlot"),
           dataTableOutput("assign")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    output$scatterPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
    k <- input$k
    first_pcs <- input$first_pcs
    first_scores = as.data.frame(score_vectors[,1:first_pcs])
    set.seed(100)
    init_centers <- initial_centroids(data = first_scores,k)
    cl<- my_kmeans(data=first_scores,centroids = init_centers)
    
    use_range = c(min (score_vectors), max(score_vectors))
    score_vectors <- as.data.frame(score_vectors)
    clusters <- cl$clusters
    centroids <- cl$centroids
    # but we didn't find a way to expand it.
    ggplot(score_vectors) + 
      geom_point(aes(score_vectors[,1],score_vectors[,2], 
                     colour=factor(clusters)), show.legend = FALSE) +
      geom_point(data=centroids,
                 aes(PC1,PC2,colour=levels(factor(clusters)))
                 ,size=3, shape=4) +
      scale_colour_brewer('Centroids', palette = 'Set1') +
      xlim(use_range) + 
      ylim(use_range) + 
      ggtitle("Clustering according to 2 Lead PC's") +
      xlab("Score Vector 1") + 
      ylab("Score Vector 2")
    })
    
    output$assign <- renderDataTable({
      k <- input$k
      first_pcs <- input$first_pcs
      first_scores = as.data.frame(score_vectors[,1:first_pcs])
      set.seed(100)
      init_centers <- initial_centroids(data = first_scores,k)
      cl<- my_kmeans(data=first_scores,centroids = init_centers)
      
      use_range = c(min (score_vectors), max(score_vectors))
      score_vectors <- as.data.frame(score_vectors)
      clusters <- cl$clusters
      centroids <- cl$centroids
      data.frame(tissue_ind=1:53, cluster=clusters)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
