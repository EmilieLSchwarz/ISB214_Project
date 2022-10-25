# Introduction to PCA
# Emilie Schwarz 
# 24 October

# Just seeing if this is working 


# example of interactive PCA graph 
library(pacman)
p_load(plotly,datasets,dplyr, ggplot2)
data(iris)

axis = list(showline=FALSE,
            zeroline=FALSE,
            gridcolor='#ffff',
            ticklen=4,
            titlefont=list(size=13))


fig <- iris %>%
  plot_ly()
fig <- fig %>%
  add_trace(
    type = 'splom',
    dimensions = list(
      list(label='sepal length', values=~Sepal.Length),
      list(label='sepal width', values=~Sepal.Width),
      list(label='petal length', values=~Petal.Length),
      list(label='petal width', values=~Petal.Width)
    ),
    color = ~Species, colors = c('#636EFA','#EF553B','#00CC96') ,
    marker = list(
      size = 7,
      line = list(
        width = 1,
        color = 'rgb(230,230,230)'
      )
    )
  )
fig <-  fig %>% style(diagonal = list(visible = FALSE))
fig <- fig %>%
  layout(
    hovermode='closest',
    dragmode= 'select',
    plot_bgcolor='rgba(240,240,240, 0.95)',
    xaxis=list(domain=NULL, showline=F, zeroline=F, gridcolor='#ffff', ticklen=4),
    yaxis=list(domain=NULL, showline=F, zeroline=F, gridcolor='#ffff', ticklen=4),
    xaxis2=axis,
    xaxis3=axis,
    xaxis4=axis,
    yaxis2=axis,
    yaxis3=axis,
    yaxis4=axis
  )

fig


# hello franziska


#using scatter sd whatever that means 
data("iris")

X <- subset(iris, select = -c(Species))

prin_comp <- prcomp(X, rank. = 3)

components <- prin_comp[["x"]]
components <- data.frame(components)
components$PC2 <- -components$PC2
components$PC3 <- -components$PC3
components = cbind(components, iris$Species)

tot_explained_variance_ratio <- summary(prin_comp)[["importance"]]['Proportion of Variance',]
tot_explained_variance_ratio <- 100 * sum(tot_explained_variance_ratio)

tit = 'Total Explained Variance = 99.48'

fig <- plot_ly(components, x = ~PC1, y = ~PC2, z = ~PC3, color = ~iris$Species, colors = c('#8FB7F3','#0D4BA7','#D4E5FB') ) %>%
  add_markers(size = 12)


fig <- fig %>%
  layout(
    title = tit,
    scene = list(bgcolor = "#F6FAFF")
  )

fig

##last version 
X <- subset(iris, select = -c(Species))
prin_comp <- prcomp(X, rank = 2)
components <- prin_comp[["x"]]
components <- data.frame(components)
components <- cbind(components, iris$Species)
components$PC2 <- -components$PC2
explained_variance <- summary(prin_comp)[["sdev"]]
explained_variance <- explained_variance[1:2]
comp <- prin_comp[["rotation"]]
comp[,'PC2'] <- - comp[,'PC2']
loadings <- comp
for (i in seq(explained_variance)){
  loadings[,i] <- comp[,i] * explained_variance[i]
}

features = c('sepal_length', 'sepal_width', 'petal_length', 'petal_width')

fig <- plot_ly(components, x = ~PC1, y = ~PC2, color = ~iris$Species, colors = c('#8FB7F3','#0D4BA7','#D6E4F9'), type = 'scatter', mode = 'markers') %>%
  layout(
    legend=list(title=list(text='color')),
    plot_bgcolor = "#e5ecf6",
    xaxis = list(
      title = "0"),
    yaxis = list(
      title = "1"))
for (i in seq(4)){
  fig <- fig %>%
    add_segments(x = 0, xend = loadings[i, 1], y = 0, yend = loadings[i, 2], line = list(color = 'black'),inherit = FALSE, showlegend = FALSE) %>%
    add_annotations(x=loadings[i, 1], y=loadings[i, 2], ax = 0, ay = 0,text = features[i], xanchor = 'center', yanchor= 'bottom')
}

fig