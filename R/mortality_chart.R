# mortality_chart.R

# Define a function to create the plot with customizable labels and colors
mortality_chart <- function(labels_to_color) {
  events <- data.frame(
    type = c("D", "M", "D", "M", "M", "D", "M", "M", "D", "M"),
    age = c(1.22, 17.62, 0.07, 17.22, 22.47, 16.41, 23.47, 19.63, 59.60, 27.42),
    start = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
  )
  
  plot(0, 0, type = "n", xlim = c(0, 80), ylim = c(1, nrow(events)), 
       xlab = "Age in years", ylab = "", axes = FALSE)
  axis(1, at = seq(0, 80, by = 10))
  
  for (i in 1:nrow(events)) {
    segments(events$start[i], i, events$age[i], i)
    
    # Default color is black
    label_color <- "black"
    
    # Check if the current label should be colored
    for (label in labels_to_color) {
      if (events$type[i] == label$type && events$age[i] == label$age) {
        label_color <- label$color
      }
    }
    
    # Add label with determined color
    text(events$age[i], i, paste0("(", events$type[i], ") ", events$age[i]), pos = 4, col = label_color)
  }
  
  legend("bottomright", legend = c("(M) = Marriage", "(D) = Death"), bty = "n")
}