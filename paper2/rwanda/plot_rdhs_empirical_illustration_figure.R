#!/usr/bin/env Rscript

# Manuscript-ready Figure 3 from aggregate, frozen Paper 2 validation outputs only.
# No respondent-level microdata are read by this script.

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(ggplot2)
})

out_dir <- "paper2/validation/generated"
v1 <- read_csv(file.path(out_dir, "RDHS_validation_V1_prevalence.csv"), show_col_types=FALSE)
v2 <- read_csv(file.path(out_dir, "RDHS_validation_V2_regression.csv"), show_col_types=FALSE)

p1 <- v1 %>%
  filter(method != "reference_full_design") %>%
  transmute(
    wave,
    method = ifelse(method=="M1_unweighted_SRS", "M1", "M2"),
    outcome = "Insurance prevalence",
    se_ratio = se_ratio_vs_reference
  )

p2 <- v2 %>%
  filter(method != "reference_full_design_logistic") %>%
  mutate(
    method = ifelse(method=="M1_unweighted_logistic", "M1", "M2"),
    outcome = case_when(
      grepl("Primary$", term) ~ "Education: primary",
      grepl("Secondary$", term) ~ "Education: secondary",
      grepl("Higher$", term) ~ "Education: higher",
      TRUE ~ term
    )
  ) %>%
  select(wave,method,outcome,se_ratio=se_ratio_vs_reference)

d <- bind_rows(p1,p2) %>%
  mutate(
    wave=factor(wave,levels=c("RDHS 2019-20","RDHS 2025")),
    method=factor(method,levels=c("M1","M2")),
    outcome=factor(
      outcome,
      levels=rev(c(
        "Insurance prevalence",
        "Education: primary",
        "Education: secondary",
        "Education: higher"
      ))
    )
  )

p <- ggplot(d,aes(x=se_ratio,y=outcome,shape=method)) +
  geom_vline(xintercept=1,linetype="dashed",linewidth=.45) +
  geom_point(size=2.8,position=position_dodge(width=.34)) +
  facet_wrap(~wave,ncol=2) +
  scale_shape_manual(values=c(M1=2,M2=0)) +
  scale_x_continuous(
    limits=c(.5,1.03),
    breaks=seq(.5,1,.1)
  ) +
  labs(
    x="Standard-error ratio relative to full survey design",
    y=NULL,
    shape=NULL
  ) +
  theme_bw(base_size=10) +
  theme(
    legend.position="none",
    panel.grid.minor=element_blank(),
    strip.background=element_blank(),
    strip.text=element_text(face="bold")
  )

ggsave(file.path(out_dir,"Figure3_RDHS_validation_precision.eps"),
       p,width=7.1,height=3.9,units="in",device="eps")
ggsave(file.path(out_dir,"Figure3_RDHS_validation_precision.pdf"),
       p,width=7.1,height=3.9,units="in",device=cairo_pdf)
ggsave(file.path(out_dir,"Figure3_RDHS_validation_precision.png"),
       p,width=7.1,height=3.9,units="in",dpi=600)
