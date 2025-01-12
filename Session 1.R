######################
#     FINANCE 4      #
#   TA SESSION #1    #
#  Timur Magzhanov   #
######################

##### PACKAGES #####
install.packages("dplyr")
install.packages("datasets")
install.packages("knitr")
install.packages("kableExtra")
install.packages("tidyr")
install.packages("tinytex")
install.packages("fixest")
install.packages("stargazer")
install.packages("ggplot2")
install.packages("ggthemes")
install.packages("showtext")
install.packages("curl")

##### LIBRARIES #####
library("dplyr") 
library("datasets")
library(knitr)          # For generating and displaying tables
library(kableExtra)     # For beautiful tables
library(tinytex)        # For PDF compilation from LaTeX
library(fixest)         # For high dim fixed effects
library(stargazer)
library(ggplot2)
library(ggthemes)       # awesome themes for ggplot2
library(showtext)       # text styles on plots
library(curl)           # for text styles auxiliary

##### DESCRIPTIVE STATISTICS #####
# Load example dataset
data("mtcars")        

# General descriptive statistics for all numeric columns
general_stats <- mtcars %>%
  summarise(across(
    everything(),
    list(
      Mean = ~mean(.),
      SD = ~sd(.),
      Median = ~median(.),
      Min = ~min(.),
      Max = ~max(.)
    )
  )) %>%
  pivot_longer(
    everything(),
    names_to = c("Variable", ".value"),
    names_sep = "_"
  )

# Compute group-level descriptive statistics by number of cylinders
group_stats <- mtcars %>%
  group_by(cyl) %>%
  summarise(across(
    everything(),
    list(
      Mean = ~mean(.),
      SD = ~sd(.),
      Median = ~median(.),
      Min = ~min(.),
      Max = ~max(.)
    )
  )) %>%
  pivot_longer(
    cols = -c(cyl),
    names_to = c("Variable", ".value"),
    names_sep = "_"
  ) %>%
  arrange(cyl, Variable)

# Display general stats in the console
cat("\n\nGeneral Descriptive Statistics:\n")
general_table_console <- kable(general_stats, format = "markdown", caption = "General Descriptive Statistics for mtcars")
print(general_table_console)

# Display group stats in the console
cat("\n\nGroup Statistics by Cylinders:\n")
group_table_console <- kable(group_stats, format = "markdown", caption = "Group Statistics by Cylinders")
print(group_table_console)

# Export general stats to LaTeX
general_table_latex <- kable(general_stats, format = "latex", booktabs = TRUE) %>%
  kable_styling(latex_options = c("striped", "hold_position"), full_width = FALSE) %>%
  add_header_above(c(" " = 1, "General Descriptive Statistics" = 5))

# Export group stats to LaTeX
group_table_latex <- kable(group_stats, format = "latex", booktabs = TRUE) %>%
  kable_styling(latex_options = c("striped", "hold_position"), full_width = FALSE) %>%
  add_header_above(c(" " = 2, "Group Statistics by Cylinders" = 5))

# Save separate LaTeX files 
#writeLines(general_table_latex, "general_table.tex")
#writeLines(group_table_latex, "group_table.tex")


##### LATEX -> PDF #####

# Create a LaTeX code as a string (example)
latex_code <- "
\\documentclass{article}
\\usepackage{booktabs}
\\usepackage{longtable}
\\begin{document}

\\section*{Example Table}
Here is an example table created in R and exported to LaTeX:

\\begin{longtable}{lrrr}
\\toprule
Variable & Mean & SD & Median \\\\
\\midrule
mpg       & 20.09 & 6.03 & 19.20 \\\\
disp      & 230.72 & 123.94 & 196.30 \\\\
hp        & 146.69 & 68.56 & 123.00 \\\\
\\bottomrule
\\end{longtable}

\\end{document}
"

# Save LaTeX code to a .tex file
writeLines(latex_code, "example.tex")

# Compile the LaTeX file into a PDF
tinytex::pdflatex("example.tex")

# Wrap the tables in a full LaTeX document
latex_document <- paste0("
\\documentclass{article}
\\usepackage{booktabs}
\\usepackage{longtable}
\\usepackage{geometry}
\\usepackage[table]{xcolor} % Added this line for cell coloring
\\geometry{a4paper, margin=1in}
\\begin{document}

\\section*{General Descriptive Statistics}
", general_table_latex, "

\\newpage

\\section*{Group Statistics by Cylinders}
", group_table_latex, "

\\end{document}
")

# Save LaTeX document to file
writeLines(latex_document, "descriptive_statistics.tex")

# Compile the LaTeX document into a PDF
tinytex::pdflatex("descriptive_statistics.tex")

##### HIGH DIMENSIONAL FIXED EFFECTS [1] #####
# visit this for more: 
# https://cran.r-project.org/web/packages/fixest/vignettes/fixest_walkthrough.html

# Load the trade data
data(trade)

# Define four expanding model specifications
model_ols_1 <- feols(log(Euros) ~ log(dist_km) | Origin, data = trade)               # Only Origin
model_ols_2 <- feols(log(Euros) ~ log(dist_km) | Origin + Destination, data = trade) # Add Destination
model_ols_3 <- feols(log(Euros) ~ log(dist_km) | Origin + Destination + Product, data = trade) # Add Product
model_ols_4 <- feols(log(Euros) ~ log(dist_km) | Origin + Destination + Product + Year, data = trade) # Add Year

# Combine models into a list
models <- list(
  model_ols_1,
  model_ols_2,
  model_ols_3,
  model_ols_4
)

# Generate a LaTeX table for the models
etable_latex <- etable(
  models,
  #headers = c(
  #  "Origin FE", 
  #  "Origin + Destination FE", 
  #  "Origin + Destination + Product FE", 
  #  "Origin + Destination + Product + Year FE"
  #),
  cluster = ~Origin + Destination,
  tex = TRUE
)

# Save the LaTeX table to a PDF-friendly document
latex_doc <- c(
  "\\documentclass{article}\n",
  "\\usepackage{booktabs}\n",
  "\\usepackage{geometry}\n",
  "\\geometry{a4paper, margin=1in}\n",
  "\\begin{document}\n",
  "\\section*{OLS Models with Expanding Fixed Effects}\n",
  etable_latex,
  "\\end{document}"
)

# Save and compile the document
writeLines(latex_doc, "ols_expanding_fixed_effects.tex")
tinytex::pdflatex("ols_expanding_fixed_effects.tex")

# Display the table in the console
etable_console <- etable(
  models,
  headers = c(
    "Origin FE", 
    "Origin + Destination FE", 
    "Origin + Destination + Product FE", 
    "Origin + Destination + Product + Year FE"
  ),
  cluster = ~Origin + Destination
)

# Use knitr::kable for a beautiful display in the console
kable(etable_console, align = "c", caption = "OLS Models with Expanding Fixed Effects")

##### HIGH DIMENSIONAL FIXED EFFECTS [2] #####
# Set up models
model_pois <- fepois(Euros ~ log(dist_km) | Origin + Destination + Product + Year, data = trade)
model_ols <- feols(log(Euros) ~ log(dist_km) | Origin + Destination + Product + Year, data = trade)
model_negbin <- fenegbin(Euros ~ log(dist_km) | Origin + Destination + Product + Year, data = trade)

# Generate the etable LaTeX table
etable_latex <- etable(
  list(
    "Poisson" = model_pois,
    "OLS" = model_ols,
    "Negative Binomial" = model_negbin
  ),
  cluster = ~Origin + Destination,
  tex = TRUE,        # Output LaTeX code
  title = "High-Dimensional Fixed Effects Results"
)

latex_doc <- c(
  "\\documentclass{article}",
  "\\usepackage{booktabs}",
  "\\usepackage{geometry}",
  "\\geometry{a4paper, margin=1in}",
  "\\begin{document}",
  "\\section*{High-Dimensional Fixed Effects Results}",
  etable_latex,  # Insert the etable LaTeX table directly
  "\\end{document}"
)

# Write the LaTeX document to a file
writeLines(latex_doc, "high_dim_fixed_effects.tex")

# Compile the LaTeX document into a PDF
tinytex::pdflatex("high_dim_fixed_effects.tex")

# Generate a nice table for console output
etable_console <- etable(
  list(model_pois, model_ols, model_negbin),
  headers = c("Poisson", "OLS", "Negative Binomial"),
  cluster = ~Origin + Destination,
  tex = FALSE
)

# Display the table in the console
kable(etable_console, align = "c", caption = "High-Dimensional Fixed Effects Results")

##### DATA VISUALISATION #####

# 1. Distribution of trade flows (Histogram)
ggplot(trade, aes(x = Euros)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black", alpha = 0.7) +
  labs(title = "Distribution of Trade Flows",
       x = "Trade Flows (Euros)",
       y = "Frequency") +
  theme_minimal()

# 2. Density Plot of Trade Flows
ggplot(trade, aes(x = Euros)) +
  geom_density(fill = "skyblue", alpha = 0.5) +
  labs(title = "Density Plot of Trade Flows",
       x = "Trade Flows (Euros)",
       y = "Density") +
  theme_minimal()

# 3. Log-Transformed Distribution of Trade Flows
ggplot(trade, aes(x = log1p(Euros))) +
  geom_histogram(bins = 30, fill = "orange", color = "black", alpha = 0.7) +
  labs(title = "Log-Scaled Distribution of Trade Flows",
       x = "Log Trade Flows (log(Euros))",
       y = "Frequency") +
  theme_minimal()

# 4. Log-Transformed Density Plot
ggplot(trade, aes(x = log1p(Euros))) +
  geom_density(fill = "orange", alpha = 0.5) +
  labs(title = "Log-Scaled Density Plot of Trade Flows",
       x = "Log Trade Flows (log(Euros))",
       y = "Density") +
  theme_minimal()

# Add the Economist font "Roboto Slab"
font_add_google(name = "Roboto Slab", family = "Roboto Slab")
showtext_auto()

# Define the Economist-style muted color palette
economist_colors <- c("BE" = "#0073C2", "DE" = "#D55E00", "FR" = "#009E73",
                      "IT" = "#F0E442", "NL" = "#CC79A7", "ES" = "#56B4E9")

# Selected countries for density plot
# Germany (DE), France (FR), Netherlands (NL)
# Belgium (BE), Italy (IT), and Spain (ES)
selected_countries <- c("BE", "DE", "FR", "IT", "NL", "ES")

# Filter and transform the data
trade_filtered <- trade %>%
  filter(Destination %in% selected_countries) %>%
  mutate(log_Euros = log(Euros))

# Create the density plot with updates
plot <- ggplot(trade_filtered, aes(x = log_Euros, fill = Destination)) +
  geom_density(alpha = 0.7, size = 0.5, color = NA) +  # Removed outlines for densities
  scale_fill_manual(values = economist_colors) +
  labs(
    title = "Density of Log Trade Flows by Country",
    subtitle = "Trade flows for selected European countries",
    x = "Log Trade Flows",
    y = "Density",
    caption = "Source: Trade Data | Chart by @yourname"
  ) +
  theme(
    # General plot appearance
    text = element_text(family = "Roboto Slab"),
    plot.margin = margin(t = 1, r = 1, b = 1, l = 1, unit = "cm"),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y = element_line(color = "#dcdbd8"),
    panel.grid.minor.y = element_blank(),
    # Axis text and title sizes
    axis.text = element_text(size = 24, color = "gray8"),
    axis.title = element_text(size = 28, color = "gray8"),
    axis.line.x = element_line(color = "gray8"),
    axis.ticks.y = element_blank(),
    # Title and subtitle sizes
    plot.title = element_text(size = rel(2.2), hjust = 0, face = "bold"),
    plot.subtitle = element_text(size = rel(1.8), hjust = 0),
    plot.caption = element_text(hjust = 0, size = 14, color = "#4B4B4B"),
    # Legend customization
    legend.title = element_blank(),
    legend.position = c(-0, 0.78),  # Fine-tuned closer to the axis
    legend.direction = "vertical",   # Legend in a vertical format
    legend.justification = "left",   # Aligned to the left side
    legend.key = element_blank(),    # Removed legend item frames
    legend.text = element_text(size = 24, margin = margin(l = 3))
  )
# Save the plot as a standalone image
ggsave(plot = plot, filename = "density_plot_beauty.png", width = 8, height = 5)


##### NONLINEAR MODELS #####

