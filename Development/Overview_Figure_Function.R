Playlist_Comparison_Figure <- function(data, input, order_selection="By Value") {
  
  if (order_selection=="By Date of Creation") {
    ggplot(data,aes(fct_reorder(Playlist, (Order)),y=!!sym(input))) +
      geom_col(fill="#1DB954",color="#29FF74",alpha=.5) +
      coord_flip() +
      labs(title=paste(input, "By Playlist",sep = " "),
           subtitle = "From oldest (top) to newest (bottom)") +
      theme_minimal() +
      theme(panel.grid = element_blank(),
            plot.title = element_text(colour = "white",hjust = .5),
            plot.subtitle = element_text(colour = "white",hjust = .5),
            axis.text.x = element_text(color="white"),
            axis.title = element_blank(),
            axis.text.y = element_text(color="white"),
            plot.background = element_rect(fill = "#212121"))
  } else {
    ggplot(data,aes(fct_reorder(Playlist,!!sym(input)),y=!!sym(input))) +
      geom_col(fill="#1DB954",color="#29FF74",alpha=.5) +
      coord_flip() +
      labs(title=paste(input, "By Playlist",sep = " ")) +
      theme_minimal() +
      theme(panel.grid = element_blank(),
            plot.title = element_text(colour = "white",hjust = .5),
            plot.subtitle = element_text(colour = "white",hjust = .5),
            axis.text.x = element_text(color="white"),
            axis.title = element_blank(),
            axis.text.y = element_text(color="white"),
            plot.background = element_rect(fill = "#212121"))
  }
}

