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