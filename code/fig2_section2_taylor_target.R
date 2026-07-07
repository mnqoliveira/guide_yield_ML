# Monique Oliveira
# monique.oliveira@embrapa.br
# 2026-06-29

# Libraries ---------------------------------------------------------------
libraries <- c("here", "dplyr", "tidyr", "data.table",
               "tdr", "openair", "ggrepel")

lapply(libraries, require, character.only = TRUE)

# Update function ---------------------------------------------------------

target_diagram <- function (data, 
                            xlab = expression("RMSEc" %.% "sign(" * sigma^"*" * ")"), 
                            ylab = "MBE", 
                            type = "quantile", 
                            cols = "brewer1", cex = 2, 
                            key.title = fill_var,
                            cuts = seq(0.25, 1, 0.25), fill_var, ...) {
    data <- tdr:::prepareData(data)
    circle <- tdr:::makeCircles(data, type, cuts)
    radius <- unique(circle$r)
    labels <- data.frame(x = 0, y = -round(radius, 3), 
                         lbs = signif(radius, 2)) %>%
        distinct()
    
    ggplot(data = data, aes(x = nrmsec * sign(difSD), y = nmbe,
                            fill = !!sym(fill_var),
                            color = !!sym(fill_var))) +
        geom_path(aes(x = x, y = y, fill = NULL, color = NULL), 
                  data = circle, col = "gray85") +
        geom_vline(xintercept = 0, col = "gray85") +
        geom_hline(yintercept = 0, col = "gray85") +
        # geom_text(aes(x = x, y = y, label = lbs, vjust = 1, fill = NULL, color = NULL),
        #           size = 3, data = labels) +
        ggrepel::geom_text_repel(data = labels,
                                 aes(x = x, y = y, label = lbs, 
                                     fill = NULL, color = NULL), size = 3) +
        geom_point(pch = 21, size = cex * 1.5) +
        scale_fill_manual(values = openColours(cols, n = nlevels(data[[fill_var]]))) +
        scale_color_manual(values = openColours(cols, n = nlevels(data[[fill_var]]))) +
        xlab(xlab) + ylab(ylab) + coord_fixed() +
        labs(fill = key.title, color = key.title) +
        guides(fill = guide_legend(nrow = 2),
               color = guide_legend(nrow = 2)) +
        theme_bw() +
        theme(legend.position = "bottom",
              legend.title = element_text())
    
}

# Data --------------------------------------------------------------------
data_l <- list()
for (file_it in list.files(here("data/mcphee"), full.names = TRUE)){
    
    name_it <- gsub(".*/", "", file_it)
    temp <- read.csv(file_it)
    
    data_l[[name_it]] <- temp
    
}

out <- rbindlist(data_l) %>%
    rename_all(tolower) %>%
    select(dataset, obs, pred) %>%
    rename(model = dataset) %>%
    group_by(model) %>%
    mutate(it = row_number(),
           model = paste0("M0", model)) %>%
    ungroup() %>%
    pivot_wider(id_cols = c("it", "obs"),
                names_from = "model", values_from = "pred") %>%
    mutate(M04 = mean(obs) + runif(n(), min = 0.001, max = 0.003))

# Taylor ------------------------------------------------------------------
taylor_data <- out %>%
    pivot_longer(starts_with("M0"),
                 names_to = "model", values_to = "yield") %>%
    select(-it)

openair::TaylorDiagram(taylor_data, obs = "obs", mod = "yield", 
              group = "model", 
              cols = "Set1", key.pos = "bottom", 
              text.obs = "Observed",
              annotate = "Centered\nRMS error",
              key.title = "Model",
              xlab = "Standard deviation",
              ylab = "Standard deviation")

ggsave(here("figures/fig2_taylor_mcphee.png"), bg = "white")

# Target ------------------------------------------------------------------
models_matrix <- out %>%
    select(-it) %>%
    as.matrix()

obs_vector <- pull(out, obs)

errModel <- applyStats(models_matrix, obs_vector)
errModel$Model <-  factor(colnames(models_matrix))
target_diagram(errModel, fill_var = "Model")
ggsave(here("figures/fig3_target_mcphee.png"))