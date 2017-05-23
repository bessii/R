
library("AzureML")
ws <- workspace()
dat <- download.intermediate.dataset(
  ws,
  experiment = "7b179202fca944f2a83e78d6f3fda9ab.f-id.3eac40ac6d984f219ddb60363c8675fb",
  node_id = "507139fb-37da-4895-b191-021f7b6fca22-885",
  port_name = "Results dataset",
  data_type_id = "GenericCSV"
)

head(dat)



bar.plot <- function(x, cut = 200){
  require(ggplot2)
  if(is.factor(diabetes[, x]) | is.character(diabetes[, x]) & (x != 'readmitted')){
    colList = c('readmitted', x)
    diabetes[, colList] = lapply(diabetes[, colList], as.factor)
    sums <- summary(diabetes[, x], counts = n())
    msk <- names(sums[which(sums > cut)])
    tmp <- diabetes[diabetes[, x] %in% msk, colList]
    capture.output(
      if(strsplit(x, '[-]')[[1]][1] == x){
        g <- ggplot(tmp, aes_string(x)) +
          geom_bar() +
          facet_grid(. ~ readmitted) +
          ggtitle(paste('Readmissions by level of', x))
        print(g) 
      } 
    )    
  } 
}


violin.plot <- function(x){
  if(is.numeric(diabetes[, x])){
    ggplot(diabetes, aes_string('readmitted', x)) +
      geom_violin() +
      ggtitle(paste('Readmissions by', x))
  }
}


box.plot <- function(x){
  if(is.numeric(diabetes[, x])){
    ggplot(diabetes, aes_string('readmitted', x)) +
      geom_boxplot() +
      ggtitle(paste('Readmissions by', x))
  }
}

hist.plot <- function(x){
  if(is.numeric(diabetes[, x])){
    capture.output(
      ggplot(diabetes, aes_string(x)) +
        geom_histogram() +
        facet_grid(readmitted ~ .) +
        ggtitle(paste('Readmissions by', x))
    )
  }
}


diabetes <- dat
col.names = names(diabetes)
col.names = c(col.names, names(diabetes))
lapply(col.names, bar.plot)
lapply(col.names, box.plot)
lapply(col.names, hist.plot) 


