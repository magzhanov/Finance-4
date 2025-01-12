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

##### LIBRARIES #####
library("dplyr") 
library("datasets")
library(knitr)          # For generating tables
library(kableExtra)     # For beautiful tables
library(tinytex)        # For PDF compilation from LaTeX

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

##### HIGH DIMENSIONAL FIXED EFFECTS #####


##### NONLINEAR MODELS #####

