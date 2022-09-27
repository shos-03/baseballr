#' @title **Pitch charts with ggplot2**
#' @description 
#' This function allows you to create pitch charts with ggplots given a data ball coordinates and The top and bottom of the strike zone.(The limits of the strike zone can be omitted)
#' 
#' @param data A data that includes ball coordinates.
#' @param x_value The x coordinates. Typically plate_x. 
#' @param z_value The y coordinates. Typically plate_z. 
#' @param sz_top The top of strike zone. Typically sz_top. If left blank, defaults to average height.
#' @param sz_bot The top of strike zone. Typically sz_bot. If left blank, defaults to average height.
#' @param lim Length from center to edge. Defaults to NA.
#' @param color_value The categorical variable that you want the geom_points to base the color on. Defaults to blue.
#' @param density Chooses between a 2d density plot or a point plot. Defaults to FALSE.
#' @param bin_size Size of bins used if use a density plot. Defaults to 8.
#' @param scall_fill_palette You can use RColorBrewer's palette if use a density plot. Defaults to Blues.
#' @param point_size Set the size of geom_point if used. Defaults to 10.
#' @param point_alpha Set the alpha of geom_point if used. Defaults to 0.75.
#' @param div9 Whether to divide the strike zone into 9. Defaults to TRUE.
#' @param base Whether to show the home base. Defaults to FALSE
#' @param title The prot title.
#' @return The pitch chart.
#' @examples \dontrun{
#' ggpitchchart(df, x_value = "plate_x", z_value = "plate_z", sz_top = "sz_top", sz_bot = "sz_bot", color_value = "pitch_type", base = TRUE, title = "Ohtani, 9.24")
#' ggpitchchart(df, x_value = "plate_x", z_value = "plate_z", sz_top = "sz_top", sz_bot = "sz_bot", lim = 2, density = TRUE, div9 = FALSE, title = "Ohtani, 9.24")
#' }
#' @importFrom ggplot2 ggplot geom_point geom_rect coord_fixed theme geom_density_2d_filled xlim ylim xlab ylab aes scale_fill_brewer labs geom_polygon
#' @export

ggpitchchart <- function(data,
                         x_value = "plate_x",
                         z_value = "plate_z",
                         sz_top = NULL,
                         sz_bot = NULL,
                         lim = NA,
                         color_value = NULL,
                         density = FALSE,
                         bin_size = 8,
                         scall_fill_palette = "Blues",
                         point_size = 10,
                         point_alpha = 0.75,
                         div9 = TRUE,
                         base = FALSE,
                         title = NULL) {
  
  x1 <- x2 <- y1 <- y2 <- x3 <- y3 <- NULL
  
  strike_zones <- data.frame(
    x1 = rep(c(-0.83, -0.25, 0.25), each = 3),
    x2 = rep(c(-0.25, 0.25, 0.83), each = 3),
    y1 = rep(c(-0.83, -0.25, 0.25), 3),
    y2 = rep(c(-0.25, 0.25, 0.83), 3)
  )
  
  
  plot <- ggplot()+
    xlim(-lim, lim) + ylim(-lim, lim)+
    xlab("") + ylab("") +
    coord_fixed(ratio = 1) +
    theme(panel.background = element_blank(), axis.text = element_blank(), axis.ticks = element_blank(),
          plot.title = element_text(face = "bold", size = 25))
  
  if(!is.null(title)){
    plot <- plot +
      labs(title = title)
  }
  
  if(base){
    base <- data.frame(
      x3 = c(0, -1, -0.83, 0.83, 1),
      y3 = c(-2.4, -2.1, -1.66, -1.66, -2.1))
    
    plot <- plot +
      geom_polygon(base, mapping = aes(x = x3, y = y3), fill = "gray")
  }
  
  if(!density){
    plot <- plot+
      theme(legend.position = "right", legend.text = element_text(size = 17), legend.title = element_text(size = 17))
    
    if(div9){
      plot <- plot+
        geom_rect(data = strike_zones, aes(xmin = x1, xmax = x2, ymin = y1, ymax = y2), fill = "white", color = "gray20")
    }else{
      plot <- plot+
        geom_rect(aes(xmin = -0.83, xmax = 0.83, ymin = -0.83, ymax = 0.83), fill = NA, color = "gray20")
    }
    
    if(is.null(color_value)){
      if(!is.null(sz_top) && !is.null(sz_bot)){
        plot <- plot+
          geom_point(data = data, mapping = aes_string(x = x_value, y = paste("(1.66*(",z_value,"-",sz_bot,")/(",sz_top,"-",sz_bot,")-0.83)")),color = "blue", alpha = point_alpha, size = point_size)
      }else{
        plot <- plot+
          geom_point(data = data, mapping = aes_string(x = x_value, y = paste("(1.66*(",z_value,"-1.57)/(3.39-1.57)-0.83)")), color = "blue", alpha = point_alpha, size = point_size)
      }
    }else{
      
      if(!is.null(sz_top) && !is.null(sz_bot)){
        plot <- plot+
          geom_point(data = data, mapping = aes_string(x = x_value, y = paste("(1.66*(",z_value,"-",sz_bot,")/(",sz_top,"-",sz_bot,")-0.83)"), color = color_value), alpha = point_alpha, size = point_size)
      }else{
        plot <- plot+
          geom_point(data = data, mapping = aes_string(x = x_value, y = paste("(1.66*(",z_value,"-1.57)/(3.39-1.57)-0.83)"), color = color_value), alpha = point_alpha, size = point_size)
      }
    }
    
  }else{
    plot <- plot+
      theme(legend.position = "none")
    
    if(!is.null(sz_top) && !is.null(sz_bot)){
      plot <- plot+
        geom_density_2d_filled(data = data, aes_string(x = x_value, y = paste("(1.66*(",z_value,"-",sz_bot,")/(",sz_top,"-",sz_bot,")-0.83)")), bins = bin_size, alpha = 0.5)+
        scale_fill_brewer(palette = scall_fill_palette)
    }else{
      plot <- plot+
        geom_density_2d_filled(data = data,aes(x = x_value,y = paste("(1.66*(",z_value,"-1.57)/(3.39-1.57)-0.83)")),bins = bin_size, alpha = 0.5)+
        scale_fill_brewer(palette = scall_fill_palette)
    }
    
    if(div9){
      plot <- plot+
        geom_rect(data = strike_zones, aes(xmin = x1, xmax = x2, ymin = y1, ymax = y2), fill = NA, color = "gray20")
    }else{
      plot <- plot+
        geom_rect(aes(xmin = -0.83, xmax = 0.83, ymin = -0.83, ymax = 0.83), fill = NA, color = "gray20")
    }
    
  }
  
  return(plot)
}