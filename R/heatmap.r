



#' perform hierarchical clustering of a dataset
#'
#'@param df input dataframe
#'@param id_var a string identifying the id variables to be clustered
#'@param measure_var a string identifying the measures of the id variable
#'@return an object of class dendrogram
#'
#'@export
#'
#'
dendro_function <- function(df, row_var, column_var){

  df.wide <- df %>% tidyr::pivot_wider(id_cols =.data[[row_var]], names_from = .data[[column_var]],
                                values_from = value, values_fill = list("value"=0))

  cells <- df.wide%>%dplyr:: pull(.data[[row_var]])
  data.mat <- as.matrix(df.wide %>% dplyr::select(-.data[[row_var]]))
  data.mat <- apply( data.mat, 2, as.numeric )
  rownames(data.mat) <- cells
  dendro <- as.dendrogram(hclust(dist(data.mat)))
  return(dendro)


}


#' Generate a heatmap where the row and or columns are optionally clustered
#'
#' @param df a dataframe from which the heatmap should be generated
#' @param value_var a string indicating the variable that contains the value for each cell of the heatmap
#' @param row_var a string indicating the variable that will be on the rows of the heatmap
#' @param column_var a string representing the variable that will be on the columns of the heamtap
#' @param normalize one of three strings ("rows", "columns" or "none") indicating which if any dimension to normalize
#' @param cluster_rows a boolean indicating if the rows should be clustered
#' @param cluster_cols a boolean indicating if the columns should be clustered
#' @param row.order r an ordered charactered vector of length of the number of distinct row values in the dataframe representing how that variable should ordered on the plot. The order starts from the bottom of the y-axis.
#' @param col.order an ordered charactered vector of length of the number of distinct column values in the dataframe representing how that variable should ordered on the plot. The order starts from the left of the x-axis
#' @param text a boolean indicating if text indicating the cell value should be placed in each entry of the heatmap
#' @param color_low a string of a color or hexadecimal value indicating the color at the low end of the gradient
#' @param color_high a string of a color or a hexadecimal value indicating the color at the high end of the gradient
#'
#' @return a list containing the ggplot heatmap, an ordered character vector of the row order used,and ordered character vector of the column order used
#' @export
clustered_heatmap <- function(df, value_var,
                              row_var,
                              column_var,
                              normalize="none",
                              cluster_rows = F,
                              cluster_cols = F,
                              row.order=NULL,
                              col.order=NULL,
                              text=F,
                              color_low = "#b35806",
                              color_high = "#542788",
                              text_size = 8){

  if(normalize=="rows"){
    norm.sum <- df %>% dplyr::group_by(.data[[row_var]]) %>%
      dplyr::summarize(total = sum(.data[[value_var]], na.rm=T))

     df<- df %>% dplyr::left_join(norm.sum) %>%
      mutate(value = .data[[value_var]]/total)


  }else if(normalize=="columns"){
    norm.sum <- df %>% dplyr::group_by(.data[[column_var]]) %>%
      summarize(total = sum(.data[[value_var]], na.rm=T))

    df<- df %>% dplyr::left_join(norm.sum) %>%
      mutate(value = .data[[value_var]]/total)


  }
  else{
    df <- df %>%
            dplyr::rename(value = .data[[value_var]])
  }

  df <-  df %>%
            dplyr::select(.data[[row_var]], .data[[column_var]], value)

  if(cluster_rows){
    dendro_rows <- dendro_function(df, row_var, column_var)
    row.order <- order.dendrogram(dendro_rows)
    dendro.rows.plot <- ggdendrogram(data=dendro_rows, rotate=TRUE, labels=F) +
      theme(axis.text.y = element_blank(),
            axis.text.x = element_blank())
    rows <-unique(df %>% pull(.data[[row_var]]))
    df <- df %>% mutate( rows = factor( .data[[row_var]],
                                        levels = rows[row.order],
                                        ordered = TRUE))

  }else{
    if(is.null(row.order)){
      row.order <-unique(df %>% pull(.data[[row_var]]))
    }
    df <- df %>% mutate(rows = factor(.data[[row_var]], levels=row.order, ordered=T))
  }

  if(cluster_cols){


    dendro_cols <- dendro_function(df, column_var, row_var)
    col.order <- order.dendrogram(dendro_cols)
    dendro.cols.plot <- ggdendrogram(data=dendro_cols, rotate=F, labels=F, leaf_labels=F) +
      theme(axis.text.y = element_blank(),
            axis.text.x = element_blank())

    cols <- unique(df %>% pull(.data[[column_var]]))
    df <- df %>% mutate(cols = factor( .data[[column_var]],
                   levels = rev(cols[col.order]),
                   ordered = TRUE))
    col.order <- cols[col.order]
  }else{
    if(is.null(col.order)){
      row.order <-unique(df %>% dplyr::pull(.data[[column_var]]))
    }

    df <- df %>% dplyr::mutate(cols = factor(.data[[column_var]], levels=col.order, ordered=T))
  }



  #pal <- wes_palette("Zissou1", 100, type = "continuous")
 heatmap.plot <-ggplot2:: ggplot(df, aes(x=cols, y=rows) )+
   ggplot2::geom_tile(aes(fill=value)) +
   #scale_fill_gradientn(name=value_var, colours = rainbow(5)) +
    ggplot2::scale_fill_gradient2(name= value_var,
                      midpoint =max(df$value, na.rm=T)/2, high=color_high, low=color_low) +
   ggplot2::theme(axis.text.y = element_text(size = text_size),
         axis.text.x = element_text(size=text_size,angle=90, vjust=.3),
         legend.position = "top") +
   ggplot2::xlab(column_var) +
   ggplot2::ylab(row_var) +
   ggplot2::scale_y_discrete(position="right")

 if(text){
   heatmap.plot <- heatmap.plot + ggplot2::geom_text(aes(label=round(value, 2)), color="white")
 }


  return(list(plot=heatmap.plot,row.order= row.order, col.order=col.order))
}






















