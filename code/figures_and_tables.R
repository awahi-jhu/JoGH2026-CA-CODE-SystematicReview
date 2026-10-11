################################################################################
# figures_and_tables.R
# Reproduces Table 1 (systematic review columns), Figures 2-3 and Figures S1-S3 for:
# Wahi A, et al. Consolidating cause-of-death evidence for children and adolescents
# through an updated systematic review and known investigator-tracing. J Glob Health.
#
# Run from the repository's main folder (the folder containing /code, /data, /forms).
# Public data include systematic-review data points only; investigator-traced data
# are not shared, so Table 1, Figures 2-3 and Figures S1-S2 reproduce the
# systematic-review portion of the published results.
################################################################################

library(dplyr)
library(tidyr)
library(stringr)
library(forcats)
library(scales)
library(ggplot2)
library(RColorBrewer)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(patchwork)

dir.create("output", showWarnings = FALSE)

## 1. Load data ------------------------------------------------------------------
nn.data     <- read.csv("data/cod_neonates_0-27days.csv")  %>% mutate(age_gp = "nn")
pn.data     <- read.csv("data/cod_1-59months.csv")         %>% mutate(age_gp = "pn")
age9.data   <- read.csv("data/cod_5-9years.csv")           %>% mutate(age_gp = "5to9")
age14.data  <- read.csv("data/cod_10-14years.csv")         %>% mutate(age_gp = "10to14")
age19f.data <- read.csv("data/cod_15-19years_female.csv")  %>% mutate(age_gp = "15to19f")
age19m.data <- read.csv("data/cod_15-19years_male.csv")    %>% mutate(age_gp = "15to19m")

# Study details for all age groups combined
sr_new <- bind_rows(
  lapply(list(nn.data, pn.data, age9.data, age14.data, age19f.data, age19m.data),
         function(d) d %>% select(age_gp, article_id:totdeaths)))

# Age-group labels used in tables and figures
age_labels <- c(nn = "Neonates", pn = "1–59 months", `5to9` = "5–9 years",
                `10to14` = "10–14 years", `15to19f` = "15–19 years (F)",
                `15to19m` = "15–19 years (M)")

## 2. Add regions ----------------------------------------------------------------
dat2 <- read.csv("data/region_classification.csv")
names(dat2)[names(dat2) == "ISO3Code"] <- "iso3"
dat2$UNICEFReportRegion1[dat2$UNICEFReportRegion1 == "Europe and Central Asia"] <- "Europe and central Asia"
dat2$UNICEFReportRegion2[dat2$UNICEFReportRegion2 == "West and Central Africa"] <- "West and central Africa"
dat2$UNICEFReportRegion2[dat2$UNICEFReportRegion2 == "Eastern Europe and Central Asia"] <- "Eastern Europe and central Asia"
dat2$Region <- dat2$UNICEFReportRegion1
dat2$Region[which(dat2$UNICEFReportRegion2 != "")] <-
  dat2$UNICEFReportRegion2[which(dat2$UNICEFReportRegion2 != "")]

sr_new <- merge(sr_new, dat2[, c("iso3", "Region")], by = "iso3")

nrow(sr_new)   # should be 197

## 3. World map (used by Figures 2 and S3) ---------------------------------------
world <- ne_countries(scale = "medium", returnclass = "sf") %>%
  filter(name != "Antarctica")
world_keep <- world %>% select(iso_a3, name, geometry)

## 4. TABLE 1 (systematic review columns) ----------------------------------------
table1 <- sr_new %>%
  mutate(age_gp = factor(age_labels[age_gp], levels = age_labels)) %>%
  group_by(age_gp) %>%
  summarise(studies     = n_distinct(article_id),
            data_points = n_distinct(strata_id),
            deaths      = sum(totdeaths, na.rm = TRUE))
table1

## 5. FIGURE 2 -------------------------------------------------------------------
wmap <- sr_new %>%
  mutate(nationalrep = as.integer(nationalrep)) %>%
  distinct(strata_id, .keep_all = TRUE)

# Panel A: national studies (Ethiopia excluded, as in the published figure)
nat_counts <- wmap %>%
  filter(nationalrep == 1, countryname != "Ethiopia", iso3 != "ETH") %>%
  group_by(iso3, countryname) %>%
  summarise(n_national = n_distinct(article_id), .groups = "drop")

nat_map <- world_keep %>%
  inner_join(nat_counts, by = c("iso_a3" = "iso3")) %>%
  mutate(nat_bucket = case_when(
    n_national == 1 ~ "1 study",
    n_national == 2 ~ "2 studies",
    n_national %in% c(3, 4) ~ "3–4 studies",
    n_national >= 5 ~ "≥5 studies"),
    nat_bucket = factor(nat_bucket,
                        levels = c("1 study", "2 studies", "3–4 studies", "≥5 studies")))

nat_cols <- c("1 study" = "#B3CDE3", "2 studies" = "#FBB4AE",
              "3–4 studies" = "#ccebc5", "≥5 studies" = "#FFFFCC")

p_nat <- ggplot() +
  geom_sf(data = world, fill = "grey85", color = "grey20", linewidth = 0.45) +
  geom_sf(data = nat_map, aes(fill = nat_bucket), color = "black", linewidth = 0.65) +
  scale_fill_manual(values = nat_cols, name = "National studies", drop = FALSE) +
  coord_sf(expand = FALSE) +
  theme_void(base_size = 13) +
  theme(legend.position = "right",
        legend.title = element_text(size = 14, face = "bold"),
        legend.text  = element_text(size = 12),
        plot.margin  = margin(8, 12, 20, 12))

# Panel B: subnational study sites (country centroids, jittered)
subnat_sites <- wmap %>%
  filter(nationalrep == 0) %>%
  inner_join(world_keep, by = c("iso3" = "iso_a3")) %>%
  st_as_sf() %>%
  st_centroid(of_largest_polygon = TRUE)

set.seed(123)
coords <- st_coordinates(subnat_sites)
coords_jit <- coords + matrix(runif(nrow(coords) * 2, -3, 3), ncol = 2)
subnat_sites$geometry <- st_sfc(
  lapply(seq_len(nrow(coords_jit)), function(i) st_point(coords_jit[i, ])),
  crs = st_crs(world))

world_grey <- world %>% filter(!(iso_a3 %in% unique(subnat_sites$iso3)))

p_subnat <- ggplot() +
  geom_sf(data = world_grey, fill = "grey85", color = "grey20", linewidth = 0.45) +
  geom_sf(data = world, fill = NA, color = "grey20", linewidth = 0.45) +
  geom_sf(data = subnat_sites, shape = 21, fill = "red", color = "black",
          alpha = 0.9, size = 2.2, stroke = 0.35) +
  theme_void(base_size = 13) +
  theme(legend.position = "none", plot.margin = margin(8, 12, 12, 12))

# Panel C: heat map of deaths by country and age group
td_country_summary <- sr_new %>%
  mutate(age_gp = factor(age_labels[age_gp], levels = age_labels)) %>%
  group_by(countryname, age_gp) %>%
  summarise(total_deaths = sum(totdeaths, na.rm = TRUE), .groups = "drop") %>%
  group_by(countryname) %>%
  mutate(total_by_country = sum(total_deaths)) %>%
  ungroup()

age_totals <- td_country_summary %>%
  group_by(age_gp) %>%
  summarise(total_age = sum(total_deaths), .groups = "drop")

x_labels <- setNames(
  paste0(str_replace(as.character(age_totals$age_gp), " \\(", "\n("),
         "\n(", comma(age_totals$total_age), ")"),
  as.character(age_totals$age_gp))

breaks <- c(0, 100, 500, 1000, 2000, 5000, 10000, 50000, 100000,
            200000, 300000, 400000, 600000)
bin_labels <- c("0–100", "101–500", "501–1,000", "1,001–2,000", "2,001–5,000",
                "5,001–10,000", "10,001–50,000", "50,001–100,000", "100,001–200,000",
                "200,001–300,000", "300,001–400,000", "400,001–600,000")
color_bins <- setNames(c("#ffffb2", "#fed976", "#feb24c", "#fd8d3c", "#fc4e2a", "#e31a1c",
                         "#bd0026", "#800026", "#4d0000", "#2a0000", "#140000", "#000000"),
                       bin_labels)

td_country_summary <- td_country_summary %>%
  mutate(death_bin = cut(total_deaths, breaks = breaks,
                         include.lowest = TRUE, labels = bin_labels))

hmap <- ggplot(td_country_summary,
               aes(x = age_gp, y = fct_reorder(countryname, total_by_country),
                   fill = death_bin)) +
  geom_tile(color = "white") +
  geom_text(aes(label = comma(total_deaths),
                color = ifelse(total_deaths > 10000, "white", "black")), size = 3.5) +
  scale_fill_manual(values = color_bins, name = "Deaths (Binned)") +
  scale_color_identity() +
  scale_x_discrete(labels = x_labels) +
  labs(x = "Age Group (Total deaths)", y = "Country") +
  theme_minimal(base_size = 9) +
  theme(axis.text.x = element_text(size = 10, hjust = 0.5, vjust = 1, margin = margin(t = 6)),
        axis.text.y = element_text(size = 10),
        axis.title  = element_text(size = 12))

# Combine panels and save
fig2 <- (p_nat / p_subnat / hmap) +
  plot_layout(heights = c(1, 1, 1.6)) +
  plot_annotation(tag_levels = "A") &
  theme(plot.tag = element_text(size = 18, face = "bold"))

ggsave("output/Figure2.tif", fig2, device = "tiff", dpi = 600,
       width = 12, height = 26, units = "in", compression = "lzw", bg = "white")

## 6. FIGURE 3 -------------------------------------------------------------------
prep_long <- function(df, label) {
  df %>%
    select(totdeaths:Undetermined) %>%
    select(-Other) %>%
    pivot_longer(cols = -totdeaths, names_to = "cause", values_to = "deaths") %>%
    mutate(percent = 100 * deaths / totdeaths,
           age_group = label)
}

all_long <- bind_rows(
  prep_long(nn.data,     "A"),
  prep_long(pn.data,     "B"),
  prep_long(age9.data,   "C"),
  prep_long(age14.data,  "D"),
  prep_long(age19f.data, "E"),
  prep_long(age19m.data, "F"))

# Panels: A neonates, B 1-59 months, C 5-9 years, D 10-14 years,
#         E 15-19 years female, F 15-19 years male
all_causes <- sort(unique(all_long$cause))
base_colors <- colorRampPalette(brewer.pal(8, "Pastel1"))(length(all_causes) - 1)
pastel_colors <- c(setNames(base_colors, setdiff(all_causes, "Undetermined")),
                   Undetermined = "grey70")

box_age <- ggplot(all_long, aes(x = cause, y = percent, fill = cause)) +
  geom_boxplot(na.rm = FALSE, color = "black", size = 0.5, fatten = 0.5, outlier.size = 0.8) +
  facet_wrap(~ age_group, ncol = 2, scales = "free_x") +
  scale_fill_manual(values = pastel_colors, guide = "none") +
  scale_y_continuous(labels = percent_format(scale = 1), limits = c(0, 100)) +
  labs(x = "Causes", y = "Percent of Total Deaths") +
  theme_minimal(base_size = 13) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        strip.text = element_text(face = "bold", size = 16, hjust = 0),
        panel.spacing = unit(1.5, "lines"))

ggsave("output/Figure3.tif", box_age, device = "tiff", dpi = 600,
       width = 10, height = 8.8, units = "in", compression = "lzw", bg = "white")


## 7. FIGURE S1: data points by region -------------------------------------------
region_counts <- sr_new %>%
  count(Region) %>%
  mutate(percent = n / sum(n) * 100,
         label  = ifelse(percent >= 5, paste0(n, " (", round(percent), "%)"), ""),
         Region = paste0(Region, ": ", n, " (", round(percent), "%)"))

figS1 <- ggplot(region_counts, aes(x = "", y = n, fill = Region)) +
  geom_col(width = 1, color = "white") +
  coord_polar(theta = "y") +
  geom_text(aes(label = label), position = position_stack(vjust = 0.5), size = 4) +
  scale_fill_brewer(palette = "Pastel1") +
  theme_void() +
  theme(legend.title = element_text(size = 14, face = "bold"),
        legend.text  = element_text(size = 12))

ggsave("output/FigureS1.png", figS1, width = 10, height = 8.8, dpi = 300, bg = "white")

## 8. FIGURE S2: data points by surveillance mid-year ----------------------------
figS2 <- ggplot(sr_new, aes(x = year_mid)) +
  geom_bar(fill = "steelblue") +
  scale_x_continuous(breaks = seq(min(sr_new$year_mid, na.rm = TRUE),
                                  max(sr_new$year_mid, na.rm = TRUE), by = 2)) +
  labs(x = "Mid-Year of Surveillance", y = "Number of data points") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 30, hjust = 0.7, size = 10),
        axis.text.y = element_text(size = 10),
        axis.title  = element_text(size = 12))

ggsave("output/FigureS2.png", figS2, width = 10, height = 8.8, dpi = 300, bg = "white")

## 9. FIGURE S3: empirical data availability and gaps by age group --------------
hm <- read.csv("data/high_mortality_countries.csv")

status_cols <- c("Empirical COD data available"       = "#66C2A5",
                 "High-mortality (no empirical data)" = "#FDB863",
                 "Other"                              = "grey90")

gap_map <- function(sr_flag, hm_flag) {
  data_yes <- hm$iso3[which(hm[[sr_flag]] == 1)]
  data_gap <- hm$iso3[which(hm[[hm_flag]] == 1 & hm[[sr_flag]] == 0)]
  world_status <- world_keep %>%
    mutate(status = case_when(iso_a3 %in% data_yes ~ "Empirical COD data available",
                              iso_a3 %in% data_gap ~ "High-mortality (no empirical data)",
                              TRUE ~ "Other"))
  ggplot(world_status) +
    geom_sf(aes(fill = status), color = "grey20", linewidth = 0.45) +
    scale_fill_manual(values = status_cols,
                      breaks = c("Empirical COD data available",
                                 "High-mortality (no empirical data)"),
                      name = "Empirical CoD data\navailability") +
    coord_sf(expand = FALSE) +
    theme_void(base_size = 13) +
    theme(legend.position = "right",
          legend.title = element_text(size = 11, face = "bold"),
          legend.text  = element_text(size = 9),
          plot.margin  = margin(8, 12, 12, 12))
}

ggsave("output/FigureS3_neonates.png",  gap_map("sr_nn",    "hm_nn"),
       width = 10, height = 8.8, dpi = 300, bg = "white")
ggsave("output/FigureS3_1-59months.png", gap_map("sr_pn",    "hm_pn"),
       width = 10, height = 8.8, dpi = 300, bg = "white")
ggsave("output/FigureS3_5-19years.png",  gap_map("sr_5to19", "hm_5to19"),
       width = 10, height = 8.8, dpi = 300, bg = "white")
