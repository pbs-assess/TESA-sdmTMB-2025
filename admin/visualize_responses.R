# Load required libraries
library(readxl)
library(ggplot2)
library(dplyr)
library(tidyr)
library(forcats)
library(stringr)

# Read the survey responses
responses <- read_excel("admin/TESA-sdmTMB-2025-Responses.xlsx")

# Set a clean theme
theme_set(theme_minimal(base_size = 12))

# ============================================================================
# COLUMN H: Familiarity with sdmTMB
# ============================================================================

# Column 8: "My familiarity with sdmTMB is"
sdmtmb_familiarity <- responses[[8]]

# Create ordered factor
familiarity_levels <- c(
  "I've never heard of it before",
  "I've heard of it but have never used it",
  "Have used it a few times before but have a lot to learn",
  "I've used it a bit but want to wrap my head around some advanced feature(s) or better understand the models",
  "I understand it quite well already"
)

familiarity_data <- data.frame(
  response = factor(sdmtmb_familiarity, levels = familiarity_levels)
) %>%
  filter(!is.na(response)) %>%
  count(response, .drop = FALSE)

# Create plot for Column H
p_familiarity <- ggplot(familiarity_data, aes(x = response, y = n)) +
  geom_col(fill = "#2C7BB6", alpha = 0.8) +
  geom_text(aes(label = n), vjust = -0.5, size = 4) +
  labs(
    title = "Familiarity with sdmTMB",
    x = NULL,
    y = "Number of respondents"
  ) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(face = "bold", size = 14)
  ) +
  ylim(0, max(familiarity_data$n) * 1.15)

ggsave("admin/plot_h_sdmtmb_familiarity.png", p_familiarity,
       width = 10, height = 6, dpi = 300)

print("Created: admin/plot_h_sdmtmb_familiarity.png")

# ============================================================================
# COLUMNS I-P: Familiarity with tools and concepts
# ============================================================================

# Extract columns 9-16
tools_cols <- 9:16

# Define the order of familiarity levels
familiarity_order <- c(
  "Never heard of it",
  "Have heard of it, don't know what it means",
  "Have a vague understanding",
  "Have a moderate understanding",
  "Understand well enough to confidently work with it"
)

# Extract and reshape the data
tools_data <- responses[, tools_cols] %>%
  setNames(c("GLMs", "mgcv/GAMs", "glmmTMB/lme4", "Finite element meshes",
             "Gaussian random fields", "SPDE approach", "TMB/RTMB",
             "INLA/inlabru")) %>%
  pivot_longer(cols = everything(), names_to = "tool", values_to = "familiarity") %>%
  filter(!is.na(familiarity)) %>%
  mutate(
    familiarity = factor(familiarity, levels = familiarity_order),
    tool = factor(tool, levels = c("GLMs", "mgcv/GAMs", "glmmTMB/lme4",
                                    "Finite element meshes", "Gaussian random fields",
                                    "SPDE approach", "TMB/RTMB", "INLA/inlabru"))
  )

# Calculate counts for each combination
tools_summary <- tools_data %>%
  count(tool, familiarity, .drop = FALSE) %>%
  replace_na(list(n = 0))

# Create stacked bar plot
p_tools <- ggplot(tools_summary, aes(x = tool, y = n, fill = familiarity)) +
  geom_col(position = "stack", alpha = 0.9) +
  geom_text(aes(label = ifelse(n > 0, n, "")),
            position = position_stack(vjust = 0.5),
            size = 3, color = "white", fontface = "bold") +
  scale_fill_brewer(palette = "RdYlBu", direction = -1, name = "Familiarity level") +
  labs(
    title = "Familiarity with Tools and Concepts",
    x = NULL,
    y = "Number of respondents"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "bottom",
    legend.direction = "vertical",
    plot.title = element_text(face = "bold", size = 14)
  ) +
  guides(fill = guide_legend(reverse = TRUE))

ggsave("admin/plot_i_p_tools_familiarity.png", p_tools,
       width = 12, height = 8, dpi = 300)

print("Created: admin/plot_i_p_tools_familiarity.png")

# ============================================================================
# COLUMNS Q-AF: Interest levels in topics
# ============================================================================

# Extract columns 17-32
interest_cols <- 17:32

# Define the order of interest levels
interest_order <- c(
  "Not interested",
  "Slightly interested",
  "Moderately interested",
  "Very interested"
)

# Create shortened topic names
topic_names <- c(
  "Index standardization",
  "Mesh design",
  "Forecasting",
  "Simulating datasets",
  "Anisotropy/barriers",
  "Multiple surveys",
  "Priors/PC priors",
  "Multivariate data/tinyVAST",
  "Time/space-varying coef.",
  "Model checking",
  "Penalized smoothers",
  "Delta/hurdle models",
  "Poisson-link delta",
  "Presence-only data",
  "DFO use cases",
  "Q&A sessions"
)

# Extract and reshape the data
interest_data <- responses[, interest_cols] %>%
  setNames(topic_names) %>%
  pivot_longer(cols = everything(), names_to = "topic", values_to = "interest") %>%
  filter(!is.na(interest)) %>%
  mutate(
    interest = factor(interest, levels = interest_order),
    topic = factor(topic, levels = topic_names)
  )

# Calculate counts
interest_summary <- interest_data %>%
  count(topic, interest, .drop = FALSE) %>%
  replace_na(list(n = 0))

# Calculate mean interest score (for ordering topics by popularity)
interest_scores <- interest_data %>%
  mutate(score = as.numeric(interest)) %>%
  group_by(topic) %>%
  summarise(mean_score = mean(score, na.rm = TRUE)) %>%
  arrange(desc(mean_score))

# Reorder topics by mean interest
interest_summary <- interest_summary %>%
  mutate(topic = factor(topic, levels = interest_scores$topic))

# Create stacked bar plot
p_interest <- ggplot(interest_summary, aes(x = topic, y = n, fill = interest)) +
  geom_col(position = "stack", alpha = 0.9) +
  geom_text(aes(label = ifelse(n > 0, n, "")),
            position = position_stack(vjust = 0.5),
            size = 2.5, color = "white", fontface = "bold") +
  scale_fill_manual(
    values = c("Not interested" = "#D73027",
               "Slightly interested" = "#FEE090",
               "Moderately interested" = "#91BFDB",
               "Very interested" = "#4575B4"),
    name = "Interest level"
  ) +
  labs(
    title = "Interest in Workshop Topics (ordered by mean interest)",
    x = NULL,
    y = "Number of respondents"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 10),
    legend.position = "bottom",
    plot.title = element_text(face = "bold", size = 14)
  ) +
  guides(fill = guide_legend(reverse = TRUE))

ggsave("admin/plot_q_af_topic_interest.png", p_interest,
       width = 14, height = 8, dpi = 300)

print("Created: admin/plot_q_af_topic_interest.png")

# ============================================================================
# Additional: Create a heatmap view of interest levels
# ============================================================================

# Calculate percentage "Very interested" or "Moderately interested" for each topic
high_interest <- interest_data %>%
  mutate(high_interest = interest %in% c("Very interested", "Moderately interested")) %>%
  group_by(topic) %>%
  summarise(
    pct_high_interest = mean(high_interest, na.rm = TRUE) * 100,
    n_total = n()
  ) %>%
  arrange(desc(pct_high_interest))

p_high_interest <- ggplot(high_interest,
                          aes(x = reorder(topic, pct_high_interest),
                              y = pct_high_interest)) +
  geom_col(fill = "#4575B4", alpha = 0.8) +
  geom_text(aes(label = sprintf("%.0f%%", pct_high_interest)),
            hjust = -0.2, size = 3.5) +
  coord_flip() +
  labs(
    title = "Topics Ranked by High Interest",
    subtitle = "Percentage of respondents who are moderately or very interested",
    x = NULL,
    y = "Percentage moderately/very interested"
  ) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(size = 11, color = "gray30")
  ) +
  ylim(0, 105)

ggsave("admin/plot_q_af_high_interest_ranking.png", p_high_interest,
       width = 10, height = 8, dpi = 300)

print("Created: admin/plot_q_af_high_interest_ranking.png")

# Print summary statistics
cat("\n=== SUMMARY STATISTICS ===\n\n")
cat("Total respondents:", nrow(responses), "\n\n")

cat("sdmTMB Familiarity:\n")
print(table(sdmtmb_familiarity))
cat("\n")

cat("Top 5 most interesting topics (% moderately/very interested):\n")
print(head(high_interest, 5))
cat("\n")

cat("All plots saved to admin/ folder\n")
