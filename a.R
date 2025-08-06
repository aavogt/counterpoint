# %%
library(pacman)
p_load(tidyverse, ggplot2)

# Load the data
cantus <- c(60, 62, 64, 62, 57, 60)
d <- read_csv("6.csv", col_names = FALSE)

# Rename columns for clarity
colnames(d) <- paste0("V", 1:6)

# Reshape the data into long format
d_long <- d %>%
  mutate(Row = row_number()) %>%
  pivot_longer(cols = starts_with("V"), names_to = "Variable", values_to = "Value")

# Create a data frame for the cantus values
cantus_df <- tibble(
  Variable = paste0("V", 1:6),
  Slope = NA,
  Time = 1:6,
  Value = cantus
)

d_long2 <- d_long %>%
  group_by(Row) %>%
  group_map(~ {
    mutate(.x,
      Time = as.numeric(str_remove(Variable, "V")),
      Slope = diff(c(Value, NA)), # Calculate slope using the whole vector
      Row = .y$Row,
      y = head(Value, -1),
      yend = tail(Value, -1)
    )
  }) %>%
  bind_rows() %>%
  ungroup()

print(head(d_long2))

# %%
pdf()
# Create the parallel coordinate plot with y jitter and cantus overlay
# Create the parallel coordinate plot with geom_segment and slope as color
plot(
  ggplot(d_long2 %>% filter(!is.na(Slope)), aes(x = Time, group = Row, xend = Time + 1, y = y, yend = yend, col = Slope)) +
    geom_segment(alpha = 0.5) +
    # geom_line(data = cantus_df, aes(x = Time, y = Value, group = 1), color = "red", linewidth = 1.5) +
    theme_minimal() +
    labs(title = "Parallel Coordinate Plot with Slope Color and Cantus Overlay", x = "Variables", y = "Values", col = "Slope")
)

# Interpolate data between sample times
interpolated_data <- d_long %>%
  group_by(Row) %>%
  # arrange(Variable, .by_group=T) %>%
  mutate(
    Time = as.numeric(str_remove(Variable, "V")),
    y = list(approx(Time, Value, n = 100)$y),
    x = list(approx(Time, Value, n = 100)$x),
  ) %>%
  unnest(cols = c(y, x))

# Create the hexbin heatmap
plot(
  ggplot(interpolated_data %>% filter(x <= 4, x >= 1.5), aes(x = x, y = y)) +
    geom_hex(bins = 30) +
    scale_fill_viridis_c() +
    theme_minimal() +
    geom_line(data = cantus_df, aes(
      x = as.numeric(str_remove(Variable, "V")),
      y = Value, group = 1
    ), color = "red", linewidth = 1.5) +
    labs(title = "Hexbin Heatmap of Interpolated Data", x = "Time", y = "Value")
)

# %%
dev.off()
