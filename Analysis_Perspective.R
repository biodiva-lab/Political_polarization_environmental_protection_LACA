# ==============================================================================
# ANÁLISE INSTITUCIONAL ALC: REGIME, LEGITIMAÇÃO IDEOLÓGICA E DESEMPENHO
# Versão: V-Dem v16 | Layout adaptado ao estilo "Tabela + Heatmap"
# ==============================================================================

library(vdemdata)
library(tidyverse)
library(scales)
library(patchwork)
library(ggh4x)
# ==============================================================================
# PASSO 1: MATRIZ EXAUSTIVA - PAÍSES DA AMÉRICA LATINA E CARIBE
# ==============================================================================
df_mandatos_alc <- tribble(
  ~country_name,      ~execme,            ~ano_inicio, ~ano_fim,
  "Argentina",        "Fernández",        2019,        2023, 
  "Argentina",        "Milei",            2024,        2026, 
  "Bolivia",          "Morales/Áñez",     2015,        2020, 
  "Bolivia",          "Arce",             2021,        2026, 
  "Brazil",           "Bolsonaro",        2019,        2022, 
  "Brazil",           "Lula",             2023,        2026, 
  "Chile",            "Piñera",           2018,        2022, 
  "Chile",            "Boric",            2022,        2026, 
  "Colombia",         "Duque",            2018,        2022, 
  "Colombia",         "Petro",            2022,        2026, 
  "Costa Rica",       "Alvarado",         2018,        2022, 
  "Costa Rica",       "Chaves",           2022,        2026, 
  "Cuba",             "Castro",           2008,        2018, 
  "Cuba",             "Díaz-Canel",       2018,        2026, 
  "Dominican Republic","Medina",          2012,        2020, 
  "Dominican Republic","Abinader",        2020,        2026, 
  "Ecuador",          "Lasso",            2021,        2023, 
  "Ecuador",          "Noboa",            2024,        2026, 
  "El Salvador",      "Bukele",           2019,        2026, 
  "Guatemala",        "Giammattei",       2020,        2024, 
  "Guatemala",        "Arévalo",          2024,        2026, 
  "Honduras",         "Hernández",        2014,        2022, 
  "Honduras",         "Castro",           2022,        2026, 
  "Mexico",           "L. Obrador",       2018,        2024, 
  "Mexico",           "Sheinbaum",        2024,        2026, 
  "Nicaragua",        "Ortega",           2017,        2021, 
  "Nicaragua",        "Ortega (Reeleito)",2022,        2026, 
  "Panama",           "Cortizo",          2019,        2024, 
  "Paraguay",         "Peña",             2023,        2026, 
  "Peru",             "Boluarte",         2023,        2026, 
  "Uruguay",          "Lacalle Pou",      2020,        2026, 
  "Venezuela",        "Maduro II",        2019,        2026  
)

# ==============================================================================
# PASSO 2: CRUZAMENTO COM A BASE DO V-DEM E AGREGAÇÃO
# ==============================================================================
df_vdem <- vdemdata::vdem %>%
  filter(year >= 2008) %>%
  select(country_name, year, 
         v2x_regime, 
         v2exl_legitideolcr_0, v2exl_legitideolcr_1, 
         v2exl_legitideolcr_2, v2exl_legitideolcr_3, v2exl_legitideolcr_4,
         v2dlencmps_osp, v2cltrnslw_osp, v2clrspct_osp, v2xcs_ccsi) 

df_mandatos <- df_vdem %>%
  inner_join(
    df_mandatos_alc, 
    by = join_by(country_name, between(year, ano_inicio, ano_fim))
  ) %>%
  group_by(country_name, execme, ano_inicio, ano_fim) %>%
  summarise(
    v2x_regime     = round(mean(v2x_regime, na.rm = TRUE)),
    nac            = mean(v2exl_legitideolcr_0, na.rm = TRUE),
    soc_com        = mean(v2exl_legitideolcr_1, na.rm = TRUE),
    cons           = mean(v2exl_legitideolcr_2, na.rm = TRUE),
    auton          = mean(v2exl_legitideolcr_3, na.rm = TRUE),
    relig          = mean(v2exl_legitideolcr_4, na.rm = TRUE),
    v2dlencmps_osp = mean(v2dlencmps_osp, na.rm = TRUE),
    v2cltrnslw_osp = mean(v2cltrnslw_osp, na.rm = TRUE),
    v2clrspct_osp  = mean(v2clrspct_osp, na.rm = TRUE),
    v2xcs_ccsi     = mean(v2xcs_ccsi, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(across(where(is.numeric), ~ifelse(is.nan(.), NA, .)))

# ==============================================================================
# PASSO 3: LIMPEZA E CRIAÇÃO DO TIPO DE DISCURSO
# ==============================================================================
df_clean <- df_mandatos %>%
  mutate(
    regime_pt = case_when(
      v2x_regime == 0 ~ "0: Autocracia Fechada",
      v2x_regime == 1 ~ "1: Autocracia Eleitoral",
      v2x_regime == 2 ~ "2: Democracia Eleitoral",
      v2x_regime == 3 ~ "3: Democracia Liberal",
      TRUE ~ "N/D"
    )
  ) %>%
  rowwise() %>%
  mutate(
    tipo_discurso = paste(
      c(
        if (!is.na(soc_com) && round(soc_com, 1) >= 0.5) "Socialista" else NULL,
        if (!is.na(nac) && round(nac, 1) >= 0.5) "Nacionalista" else NULL,
        if (!is.na(cons) && round(cons, 1) >= 0.5) "Conservador" else NULL,
        if (!is.na(relig) && round(relig, 1) >= 0.5) "Religioso" else NULL,
        if (!is.na(auton) && round(auton, 1) >= 0.5) "Autonomista" else NULL
      ),
      collapse = " + " 
    ),
    tipo_discurso = if_else(tipo_discurso == "", "Sem Ideologia Dominante", tipo_discurso)
  ) %>%
  ungroup()

# ==============================================================================
# EDIÇÃO MANUAL NO FORMATO LARGO
# ==============================================================================
# PARE AQUI se estiver executando linha por linha!
# Altere os valores de 'tipo_discurso' na tabela interativa e feche a janela.
df_clean <- edit(df_clean)
# ==============================================================================
# PASSO 4: ORDENAÇÃO
# ==============================================================================
df_clean <- df_clean %>%
  mutate(
    ordem_discurso = case_when(
      tipo_discurso == "Socialista" ~ 1,                
      str_detect(tipo_discurso, "Socialista") ~ 2,      
      tipo_discurso == "Conservador" ~ 3,               
      str_detect(tipo_discurso, "Conservador") ~ 4,     
      tipo_discurso == "Sem Ideologia Dominante" ~ 6,   
      TRUE ~ 5                                          
    ),
    # Vamos manter APENAS o nome do presidente e anos no rótulo, 
    # pois o nome do país vai virar o título do grupo (facet)
    label_y_text = paste0(execme, " (", ano_inicio, "-", ano_fim, ")")
  ) %>%
  # 1. ORDENAÇÃO HIERÁRQUICA: (País alfabético, Cronologia)
  arrange(country_name, ano_inicio) %>%
  # IMPORTANTE: Criamos um fator usando inique() para manter a ordem da tabela
  mutate(
    label_y = factor(label_y_text, levels = rev(unique(label_y_text)))
  ) %>%
  # 2. NORMALIZAÇÃO
  mutate(
    across(c(v2dlencmps_osp, v2cltrnslw_osp, v2clrspct_osp), 
           ~ scales::rescale(.x, to = c(0, 1), from = c(0, 4)))
  )

# ==============================================================================
# PASSO 5: PIVOT LONGER (PREPARAÇÃO PARA O PLOT)
# ==============================================================================
df_long_desempenho <- df_clean %>%
  pivot_longer(
    cols = c(v2dlencmps_osp, v2cltrnslw_osp, v2clrspct_osp, v2xcs_ccsi),
    names_to = "variavel_vdem",
    values_to = "valor_norm"
  ) %>%
  mutate(
    variavel_pt = case_match(
      variavel_vdem,
      "v2dlencmps_osp" ~ "Bens Públicos",
      "v2cltrnslw_osp" ~ "Aplicação da Lei",
      "v2clrspct_osp"  ~ "Capacidade Estatal",
      "v2xcs_ccsi"     ~ "Sociedade Civil"
    ),
    variavel_pt = factor(variavel_pt, levels = c("Bens Públicos", 
                                                 "Aplicação da Lei", 
                                                 "Capacidade Estatal", 
                                                 "Sociedade Civil")),
    grupo_ideologico = case_when(
      tipo_discurso == "Socialista" ~ "1. Socialista (Puro)",
      str_detect(tipo_discurso, "Socialista") ~ "2. Socialista (Misto)",
      tipo_discurso == "Conservador" ~ "3. Conservador (Puro)",
      str_detect(tipo_discurso, "Conservador") ~ "4. Conservador (Misto)",
      tipo_discurso == "Sem Ideologia Dominante" ~ "6. Sem Ideologia Dominante",
      TRUE ~ "5. Outras Ideologias"
    )
  )
# ==============================================================================
# PASSO 6: MONTAGEM DO GRÁFICO (GGPLOT2) USANDO FACET_GRID (TRADUZIDO)
# ==============================================================================
p_facet_discurso <- ggplot(df_long_desempenho, aes(x = valor_norm, y = label_y, fill = grupo_ideologico)) +
  geom_vline(xintercept = 0.5, color = "grey30", linewidth = 0.5) +
  
  # 1. BARRAS MAIS FINAS: Aumenta a sensação de espaço entre presidentes do mesmo país
  geom_col(width = 0.85) +
  
  scale_fill_manual(
    # 'values' mantém o texto em português para puxar os dados corretamente da sua base
    values = c(
      "1. Socialista (Puro)" = "#e31a1c",       
      "2. Socialista (Misto)" = "#fb9a99",      
      "3. Conservador (Puro)" = "#1f78b4",      
      "4. Conservador (Misto)" = "#a6cee3",     
      "5. Outras Ideologias" = "#fdbf6f",       
      "6. Sem Ideologia Dominante" = "grey70"             
    ),
    # 'labels' substitui APENAS o texto visual da legenda para o inglês
    labels = c(
      "1. Socialista (Puro)" = "1. Strict Left",       
      "2. Socialista (Misto)" = "2. Hybrid Left",      
      "3. Conservador (Puro)" = "3. Strict Right",      
      "4. Conservador (Misto)" = "4. Hybrid Right)",     
      "5. Outras Ideologias" = "5. Other ",       
      "6. Sem Ideologia Dominante" = "6. No Dominant Ideology"
    ),
    name = "Ideological Discourse" 
  ) +
  scale_x_continuous(limits = c(0, 1), breaks = c(0, 0.5, 1)) +
  
  # TRADUÇÃO DAS COLUNAS (FACETS)
  # O 'labeller' traduz visualmente os nomes das variáveis sem alterar a base de dados
  facet_grid(
    country_name ~ variavel_pt, 
    scales = "free_y", 
    space = "free_y", 
    switch = "y",
    labeller = labeller(variavel_pt = c(
      "Bens Públicos" = "Public Goods",
      "Aplicação da Lei" = "Law Enforcement",
      "Capacidade Estatal" = "State Capacity",
      "Sociedade Civil" = "Civil Society"
    ))
  ) + 
  
  # TÍTULOS TRADUZIDOS
  labs(
    title = "The Impact of Ideological Discourse on Institutional Performance in LAC",
    subtitle = "Comparison of the four institutional dimensions. Colors indicate the ideological leaning of the executive.",
    x = "Normalized Score",
    y = NULL
  ) +
  
  theme_linedraw() + 
  theme(
    plot.title = element_text(face = "bold", size = 16, margin = margin(b = 5)),
    plot.subtitle = element_text(color = "grey40", face = "italic", margin = margin(b = 20)),
    
    strip.text.x = element_text(face = "bold", size = 10, color = "black", margin = margin(b = 10)),
    
    # 2. MARGEM DIREITA AUMENTADA (r = 40): Afasta o nome do país do nome do presidente
    strip.text.y.left = element_text(face = "bold",color="black" ,size = 10, angle = 0, hjust = 1, vjust = 0.5, margin = margin(r = 40)),
    strip.placement = "outside", 
    
    panel.spacing.y = unit(1.0, "lines"), 
    panel.spacing.x = unit(1.0, "lines"),
    
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_blank(), 
    panel.grid.minor.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    
    panel.border = element_blank(), 
    axis.line.x = element_line(color = "grey20", linewidth = 0.5), 
    axis.line.y = element_line(color = "grey20", linewidth = 0.5), 
    
    axis.text.y = element_text(face = "bold", size = 9, color = "#333333"),
    axis.text.x = element_text(size = 8),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    legend.text = element_text(size = 10),
    
    strip.background = element_blank()
  )

print(p_facet_discurso)

# ==============================================================================
# PASSO 7: SALVAR O GRÁFICO AUTOMATICAMENTE
# ==============================================================================
if (exists("choose.dir", envir = as.environment("package:utils"))) {
  pasta_destino <- utils::choose.dir(caption = "Selecione a pasta para salvar o gráfico")
} else if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
  pasta_destino <- rstudioapi::selectDirectory(caption = "Selecione a pasta para salvar o gráfico")
} else {
  pasta_destino <- readline(prompt = "Digite ou cole o caminho completo da pasta: ")
}

caminho_arquivo <- file.path(pasta_destino, "discurso_desempenho_ALC.png")

ggsave(
  filename = caminho_arquivo,
  plot = p_facet_discurso,
  width = 14,       
  # 3. ALTURA AUMENTADA: Estica o gráfico, afastando os presidentes verticalmente
  height = 15,      
  dpi = 300,        
  bg = "white"      
)%>%
  cat("\n[SUCESSO] Gráfico salvo com sucesso em:\n", caminho_arquivo, "\n")






# ==============================================================================
# REQUIRED PACKAGES
# ==============================================================================
library(vdemdata)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggnewscale)
library(terra)
library(stringr)
library(scales)

# ==============================================================================
# FIGURE 1: SHIFTS IN POLITICAL GOVERNANCE AND BURNED AREA DYNAMICS
# ==============================================================================

# 1. Data Preparation
# Filter V-Dem data for Brazil (2001-2023)
vdem_br <- vdem |>
  filter(country_name == "Brazil", year >= 2001, year <= 2023) |>
  select(year = year, rule_of_law = v2x_rule)

print("Select the MapBiomas CSV file...")
fire_path <- file.choose()

colnames(fire_data_wide)

# Read MapBiomas file
fire_data_wide <- read.csv(fire_path, fileEncoding = "UTF-8-BOM", check.names = FALSE)

# Clean data (Data is already in long format from GEE)
fire_data <- fire_data_wide |>
  select(year = ano, burned_area_ha = area_fogo_ha) |>
  mutate(
    year = as.numeric(year),
    burned_area_ha = as.numeric(burned_area_ha),
    fire_millions_ha = burned_area_ha / 1000000 
  ) |>
  filter(year >= 2001, year <= 2023)

# Climate Data (ENSO - Oceanic Niño Index)
climate_data <- data.frame(
  year = 2001:2023,
  oni_index = c(-0.2, 1.0, 0.3, 0.4, 0.1, 0.6, -0.6, -0.7, 0.5, 1.1, -0.7, -0.1, 
                -0.2, 0.4, 2.3, 0.5, -0.1, 0.4, 0.5, -0.4, -0.8, -0.9, 1.5)
)

# Merge datasets and set the political spectrum
final_data <- vdem_br |>
  inner_join(fire_data, by = "year") |>
  inner_join(climate_data, by = "year") |>
  mutate(
    political_spectrum = case_when(
      year %in% c(2003:2015, 2023) ~ "Left-wing / Center-Left",
      year %in% c(2001:2002, 2016:2018) ~ "Right-wing / Center-Right",
      year %in% 2019:2022 ~ "Far-right"
    ),
    political_spectrum = factor(political_spectrum, levels = c(
      "Left-wing / Center-Left", "Right-wing / Center-Right", "Far-right"
    ))
  )

# 2. Plot Construction
max_fire <- max(final_data$fire_millions_ha, na.rm = TRUE)
max_vdem <- max(final_data$rule_of_law, na.rm = TRUE)
scaling_factor <- max_fire / max_vdem
y_axis_ceiling <- ceiling(max_fire / 5) * 5 

fig1_plot <- ggplot(final_data, aes(x = year)) +
  # Thermal Anomaly (ENSO)
  geom_rect(
    aes(xmin = year - 0.5, xmax = year + 0.5, 
        ymin = -2.5, ymax = -0.5, fill = oni_index)
  ) +
  scale_fill_gradient2(
    name = "Thermal Anomaly\n(ONI Index °C):", 
    low = "#1d3557", mid = "#f1faee", high = "#9e2a2b", midpoint = 0,
    limits = c(-2.5, 2.5),
    guide = guide_colorbar(
      title.position = "top", title.hjust = 0.5,
      barwidth = 14, barheight = 0.6 
    )
  ) +
  new_scale_fill() +
  # Political Spectrum Bars
  geom_col(aes(y = fire_millions_ha, fill = political_spectrum), 
           alpha = 0.85, width = 0.7) +
  scale_fill_manual(
    name = "Political Spectrum:", 
    values = c(
      "Left-wing / Center-Left"   = "#d62828",
      "Right-wing / Center-Right" = "#457b9d",
      "Far-right"                 = "#343a40"
    )
  ) +
  # Rule of Law Line and Points (V-Dem)
  geom_line(aes(y = rule_of_law * scaling_factor, color = "Rule of Law"), 
            linewidth = 1.3, linetype = "dashed", group = 1) +
  geom_point(aes(y = rule_of_law * scaling_factor, color = "Rule of Law"), 
             size = 3, shape = 17) +
  scale_color_manual(name = "", values = c("Rule of Law" = "#003049")) +
  # Axis Scaling
  scale_y_continuous(
    name = "Burned Area (Million Hectares)",
    limits = c(-2.5, y_axis_ceiling * 1.05),
    breaks = seq(0, y_axis_ceiling, by = 5),
    sec.axis = sec_axis(~ . / scaling_factor, name = "Rule of Law Index (V-Dem)")
  ) +
  scale_x_continuous(breaks = seq(2001, 2023, by = 2), expand = expansion(add = c(3, 1))) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.5) +
  # Climate Label
  annotate("text", x = 1997.0, y = -1.5, label = "ENSO Cycle\n(Climate)", 
           size = 3.5, fontface = "bold", hjust = 0, color = "black") +
  labs(
    title = "Shifts in Political Governance and Burned Area Dynamics in Brazil",
    x = "Year"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "top", 
    legend.box = "horizontal",
    legend.margin = margin(b = 15), 
    legend.title = element_text(face = "bold", size = 11),
    plot.title = element_text(face = "bold", size = 16),
    axis.title.y.left = element_text(color = "black", face = "bold", margin = margin(r = 12)),
    axis.title.y.right = element_text(color = "#003049", face = "bold", margin = margin(l = 12)),
    axis.text.x = element_text(angle = 0, hjust = 0.5, face = "bold", size = 12),
    axis.text.y = element_text(size = 12),
    panel.grid.minor = element_blank() 
  )

print(fig1_plot)

# ==============================================================================
# FIGURE 2: BURNED AREA PROPORTION BY FIRE PATCH SIZE
# ==============================================================================

print("Select ALL downloaded .tif files (hold Ctrl or Shift to select multiple)...")
tif_files <- choose.files() 

if(length(tif_files) == 0) stop("No files selected!")

process_tif <- function(path) {
  file_year <- as.numeric(str_extract(basename(path), "[0-9]{4}"))
  fire_raster <- rast(path)
  pixel_freq <- freq(fire_raster)
  
  df <- as.data.frame(pixel_freq) |>
    rename(size_class = value, pixel_count = count) |>
    mutate(
      year = file_year,
      area_hectares = pixel_count * 0.09, 
      category = case_when(
        size_class %in% 1:4 ~ "Small (Up to 50 ha)",
        size_class %in% 5:6 ~ "Medium (50 to 500 ha)",
        size_class %in% 7:8 ~ "Large and Mega (> 500 ha)",
        TRUE ~ "Other"
      )
    ) |>
    filter(category != "Other") |>
    group_by(year, category) |>
    summarise(total_area_ha = sum(area_hectares), .groups = "drop")
  
  return(df)
}

print("Starting raster processing...")
pb <- txtProgressBar(min = 0, max = length(tif_files), style = 3)

data_list <- list()
for (i in seq_along(tif_files)) {
  data_list[[i]] <- process_tif(tif_files[i])
  setTxtProgressBar(pb, i) 
}
close(pb) 

# Merge processed data and adjust factors
fire_sizes_data <- bind_rows(data_list) |>
  mutate(category = factor(category, levels = c(
    "Large and Mega (> 500 ha)",
    "Medium (50 to 500 ha)", 
    "Small (Up to 50 ha)"
  )))

fig2_plot <- ggplot(fire_sizes_data, aes(x = year, y = total_area_ha, fill = category)) +
  geom_col(position = "fill", width = 0.8, alpha = 0.95) +
  scale_fill_manual(
    name = "Fire Patch Size:",
    values = c(
      "Large and Mega (> 500 ha)" = "#d62828", 
      "Medium (50 to 500 ha)"     = "#f4a261", 
      "Small (Up to 50 ha)"       = "#e9c46a"  
    )
  ) +
  scale_y_continuous(labels = percent_format(accuracy = 1)) +
  scale_x_continuous(
    breaks = seq(min(fire_sizes_data$year), max(fire_sizes_data$year), by = 1)
  ) +
  labs(
    title = "Burned Area Proportion by Fire Patch Size in Brazil",
    x = "Year",
    y = "Proportion of Burned Area (%)"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "top",
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    axis.text.x = element_text(angle = 45, hjust = 1, face = "bold"),
    axis.text.y = element_text(size = 12),
    axis.title = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )

print(fig2_plot)

# ==============================================================================
# HIGH-RESOLUTION EXPORT (300 DPI)
# ==============================================================================
# ggsave(filename = "C:/Seu/Caminho/Figure_1.png", plot = fig1_plot, width = 14, height = 8.5, dpi = 300)
# ggsave(filename = "C:/Seu/Caminho/Figure_2.png", plot = fig2_plot, width = 12, height = 7, dpi = 300)


#############END
