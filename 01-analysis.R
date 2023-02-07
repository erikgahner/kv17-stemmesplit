library("tidyverse")
library("haven")
library("survey")
library("interplot")
library("scales")
library("stargazer")
library("extrafont")

landspartier <- read_sav("2017landspartier.sav") %>% 
  mutate(listenavn = case_when(
    listenavn %in% c("SF-Socialistisk Folkeparti", "Socialistisk Folkeparti") ~ "SF - Socialistisk Folkeparti",
    TRUE ~ listenavn
  ))

hops <- read_sav("Hops.sav") %>% select(-komnavn) %>% rename(komnr = Komnr, nationalgrad = H_ops)

kv09 <- read_sav("KV09_UK_100701.sav")
komdata <- kv09 %>%
  mutate(komnr = KomCPR) %>%
  group_by(komnr) %>%
  summarise(
    merged = mean(Samlaegkom),
    indbyg = mean(log(Indbyg09)),
    .groups = "drop"
  )

st <- read_sav("KV17.sav") %>% select(-c("vaegt_np", "vaegt_sp"))
kv17 <- read_sav("KV17_rev.sav")


Sys.setlocale("LC_TIME", "da_DK.UTF-8")
options(OutDec= ",")

theme_set(
  theme_bw()
)

landspartier_wide <- landspartier %>% 
  pivot_wider(names_from = listenavn, values_from = listenavn) %>% 
  group_by(komnr) %>% 
  summarize_all(~ max(as.character(.), na.rm = TRUE)) %>% 
  unite("opstilletparti", `Socialdemokratiet`:`Udenfor partierne`, sep = "; ") %>% 
  mutate(opstilletparti = str_remove_all(opstilletparti, "NA; ")) %>% 
  select(komnr, opstilletparti)

NROW(kv17) # 5386

# 4381
NROW(filter(kv17, !labelled::to_character(kv17$spm_15) %in% c("Ønsker ikke at svare", "Ville stemme blankt", "Ved ikke", "Ville ikke stemme"),
            !labelled::to_character(kv17$spm_14)  %in% c("Ønsker ikke at svare", "Stemte blankt", "Ved ikke", "Ville ikke stemme")))

kv17 <- kv17 %>% 
  mutate(nationalt_parti = labelled::to_character(spm_15)) %>% 
  left_join(landspartier_wide, by = "komnr") %>% 
  mutate(partimulighed = ifelse(str_detect(opstilletparti, nationalt_parti), 1, 0))

kv <- left_join(kv17, st, by = "RespondentSerial")
kv <- left_join(kv, komdata, by = "komnr")
kv <- left_join(kv, hops, by = "komnr")

# 4381
NROW(filter(kv17, !labelled::to_character(kv17$spm_15) %in% c("Ønsker ikke at svare", "Ville stemme blankt", "Ved ikke", "Ville ikke stemme"),
            partimulighed == 1))

kv <- kv %>% 
  mutate(
    mand = ifelse(koen == 1, 1, 0),
    alder = spm_41,
    uddannelse = uddan_ds,
    
    indkomst = ifelse(spm_51 == 99, NA, spm_51),
    polintr = ifelse(spm_26 == 5, NA, 4 - spm_26),
    partiid = ifelse(spm_9 == 1, 1, 0),
    flyttet = ifelse(spm_1 == 1, 1, 0),
    arbejde = ifelse(spm_47 == 1, 1, 0),
    pendler = 1 - arbejde,
    opvokset = ifelse(spm_48 == 1, 1, 0),
    splitvote = ifelse(FV17_parti == KV17_parti, 0, 1),
    storkommune = ifelse(indbyg >= median(indbyg), 1, 0)
    ) %>% 
  filter(partimulighed == 1) 

table(labelled::to_character(kv$KV17_parti))
table(labelled::to_character(kv$FV17_parti))

cor(kv$indbyg, kv$splitvote, use = "pairwise.complete.obs")
cor(kv$indbyg, kv$splitvote, use = "pairwise.complete.obs")

table(kv$nationalt_parti[kv$partimulighed != 1])

kv %>% 
  select(partimulighed, opstilletparti, nationalt_parti, KV17_parti) %>% 
  filter(partimulighed != 1)

table(kv$partimulighed)

kv_med <- kv %>% 
  filter(partimulighed == 1) %>% 
  mutate(komnavn = labelled::to_character(skommune)) %>% 
  group_by(komnavn) %>% 
  summarise(split_med = mean(splitvote, na.rm = TRUE),
            indbyg = mean(indbyg, na.rm = TRUE))

kv_total <- kv %>% 
  mutate(komnavn = labelled::to_character(skommune)) %>% 
  group_by(komnavn) %>% 
  summarise(split_total = mean(splitvote, na.rm = TRUE))

kv %>% 
  filter(partimulighed == 1) %>% 
  filter(labelled::to_character(skommune) == "Samsø", splitvote == 1) %>% 
  select(KV17_parti, FV17_parti)

left_join(kv_med, kv_total, by = "komnavn") %>% 
  mutate(dif = split_total - split_med)

kv %>% 
  filter(partimulighed == 1) %>% 
  mutate(komnavn = labelled::to_character(skommune)) %>% 
  group_by(komnavn) %>% 
  summarise(split = mean(splitvote, na.rm = TRUE),
            indbyg = mean(indbyg, na.rm = TRUE)) %>%
  select(-komnavn) %>% 
  cor(use = "pairwise.complete.obs")

kv %>% 
  filter(partimulighed == 1) %>% 
  mutate(komnavn = labelled::to_character(skommune)) %>% 
  group_by(komnavn) %>% 
  summarise(split = mean(splitvote, na.rm = TRUE),
            indbyg = mean(indbyg, na.rm = TRUE)) %>% 
  ggplot(aes(indbyg, split)) +
  geom_point()

kv %>% 
  filter(partimulighed == 1) %>% 
  mutate(komnavn = labelled::to_character(skommune)) %>% 
  group_by(komnavn) %>% 
  summarise(split = weighted.mean(splitvote, w = vaegt_np, na.rm = TRUE),
            indbyg = mean(indb_2018/1000, na.rm = TRUE),
            .groups = "drop") %>%
  mutate(komnavn_label = case_when(
    indbyg < 9 | indbyg > 150 | split > 0.45 | split < 0.13 ~ komnavn,
    TRUE ~ NA_character_
  )) %>% 
  ggplot(aes(indbyg, split, label = komnavn_label)) +
  geom_point() +
  ggrepel::geom_text_repel() +
  scale_y_continuous("Andel split stemmer", labels = scales::percent_format(decimal.mark = ",")) +
  scale_x_continuous("Indbyggere (tusinde)", trans = "log10") 

ggsave("andelsplitstemmer.png", width = 7, height = 5)


kv %>% 
  select(splitvote, indbyg) %>% 
  mutate(indbyg = log(indbyg)) %>% 
  cor(use = "pairwise.complete.obs")

kv %>% 
  mutate(komnavn = labelled::to_character(skommune)) %>% 
  group_by(komnavn) %>% 
  summarise(split = mean(splitvote, na.rm = TRUE),
            indbyg = mean(indbyg, na.rm = TRUE)) %>%
  mutate(komnavn = fct_reorder(komnavn, split)) %>% 
  ggplot(aes(komnavn, split)) +
  geom_point() +
  coord_flip()

kv %>% 
  group_by(storkommune) %>% 
  summarise(split = mean(splitvote, na.rm = TRUE))


kv %>% 
  group_by(storkommune) %>% 
  summarise(split = mean(splitvote, na.rm = TRUE))

ggplot(kv, aes(x = storkommune, y = ))


names(kv)[str_detect(names(kv), "vaegt")]


dclus1 <- svydesign(id=~RespondentSerial, weights=~vaegt_np, data=
                      kv %>% filter(!FV17_parti %in% c(11, 13) & !KV17_parti %in% c(11, 13)) %>% 
                      mutate(FV17_parti_names = str_sub(labelled::to_character(FV17_parti), 1, 1),
                             KV17_parti_names = str_sub(labelled::to_character(KV17_parti), 1, 1))
                    )


summary(dclus1)

unique(labelled::to_character(kv$FV17_parti[kv$FV17_parti == 12]))
unique(labelled::to_character(kv$KV17_parti[kv$KV17_parti == 6]))

tbl <- svytable(~ KV17_parti_names + FV17_parti_names, dclus1)
round(prop.table(tbl) * 100, 1)

unique(labelled::to_character(kv$uddannelse[kv$uddannelse == 1]))

reg_1 <- lm(I(splitvote*100) ~ factor(skommune) + mand + alder + indkomst + uddannelse, data = kv)
reg_2 <- lm(I(splitvote*100) ~ factor(skommune) + mand + alder + indkomst + uddannelse + flyttet + pendler, data = kv)
reg_3 <- lm(I(splitvote*100) ~ factor(skommune) + mand + alder + indkomst + uddannelse + flyttet + pendler + partiid, data = kv)
reg_4 <- lm(I(splitvote*100) ~ factor(skommune) + mand + alder + indkomst + uddannelse + flyttet + pendler + partiid + polintr, data = kv)
reg_5 <- lm(I(splitvote*100) ~ factor(skommune) + mand + alder + indkomst + uddannelse + flyttet + pendler + partiid*polintr, data = kv)

stargazer(reg_1, reg_2, reg_3, reg_4, reg_5, type = "text", 
          no.space = TRUE, digits= 2,
          keep.stat = c("rsq", "n"),
          decimal.mark = ",",
          keep = c("uddannelse", "flyttet", "pendler","partiid", "polintr"),
          covariate.labels = c("Uddannelse", "Tilflytter", "Pendler", "Parti ID", "Politisk interesse",
                               "Parti ID * Politisk interesse"),
          out = "regression.htm")
options(OutDec= ",")

marg_effekter <- interplot(m = reg_5, var1 = "partiid", var2 = "polintr", plot=FALSE) 

marg_effekter %>% 
  ggplot(aes(x = polintr, y = coef, ymin = lb, ymax = ub)) +
  geom_point() +
  labs(y = "Effekt af partiidentifikation på split vote") +
  geom_errorbar(width = 0) + 
  geom_hline(yintercept = 0) +
  theme(text=element_text(family="Times New Roman")) +
  scale_x_continuous("", labels = c("Lav politisk\ninteresse", "", "", "Høj politisk\ninteresse")) +
  scale_y_continuous(labels=function(x) format(x, decimal.mark = ",", scientific = FALSE))

ggsave("fig2.png", height = 4, width = 6)

kv %>%
  select(splitvote, mand, alder, uddannelse, indkomst, polintr, partiid, flyttet, arbejde, opvokset, merged, nationalgrad, indbyg) %>%
  mutate(indbyg = log(indbyg)) %>%
  data.frame(.) %>%
  stargazer(.,  median = TRUE, iqr = TRUE, type = "text",
            covariate.labels = c("Split stemme", "Mand", "Alder", "Uddannelse", "Indkomst", "Politisk interesse", "Timing", "Parti ID", "Tilflytter", "Arbejde i kommune", "Opvokset i kommune", "Ny kommune", "Nationaliseringsgrad", "Indbyggere (log)"),
            out = "tabel-deskriptiv.htm")

stargazer(lm(splitvote ~ mand + alder + uddannelse, data = kv), type = "text")

ggplot(kv, aes(x = Split_total)) +
  geom_bar(aes(y = (..count..)/sum(..count..))) +
  scale_x_continuous("", breaks = 1:5, labels = c("FV = RV = KV", 
                                                  "FV = RV \u2260 KV",
                                                  "FV \u2260 RV = KV",
                                                  "FV = KV ≠ RV",
                                                  "FV ≠ KV ≠ RV")) +
  scale_y_continuous("", labels=percent) +
  theme(text=element_text(family="Times New Roman"))

ggsave("fig1.png", height = 4, width = 8)

reg_1 <- lm(splitvote ~ mand + alder + uddannelse + indkomst, data = kv)
reg_2 <- lm(splitvote ~ mand + alder + uddannelse + indkomst + polintr + partiid, data = kv)
reg_3 <- lm(splitvote ~ mand + alder + uddannelse + indkomst + polintr + partiid + flyttet + arbejde + opvokset, data = kv)
reg_4 <- lm(splitvote ~ mand + alder + uddannelse + indkomst + polintr + partiid + flyttet + arbejde + opvokset + merged + nationalgrad + log(indbyg), data = kv)

stargazer(reg_1, reg_2, reg_3, reg_4, type = "text", 
          covariate.labels = c("Mand", "Alder", "Uddannelse", "Indkomst", "Politisk interesse", "Parti ID", "Tilflytter", "Arbejde i kommune", "Opvokset i kommune", "Ny kommune", "Nationaliseringsgrad", "Indbyggere (log)"),
          align = TRUE, column.sep.width = "0pt", no.space = TRUE, digits= 2,
          out = "tabel-regression.htm")

reg_pid_polintr <- lm(splitvote ~ partiid*polintr + mand + alder + uddannelse + indkomst + flyttet + arbejde + opvokset + merged + nationalgrad + log(indbyg), data = kv)
reg_pid_uddannelse <- lm(splitvote ~ partiid*uddannelse + mand + alder + polintr + indkomst + flyttet + arbejde + opvokset + merged + nationalgrad + log(indbyg), data = kv)
reg_pid_nationalgrad <- lm(splitvote ~ partiid*nationalgrad + mand + alder + polintr + indkomst + uddannelse + flyttet + arbejde + opvokset + merged + log(indbyg), data = kv)
reg_pid_indbyg <- lm(splitvote ~ partiid*log(indbyg) + mand + alder + polintr + indkomst + uddannelse + flyttet + arbejde + opvokset + merged + nationalgrad, data = kv)

stargazer(reg_pid_polintr, reg_pid_uddannelse, reg_pid_nationalgrad, reg_pid_indbyg, type = "text",
          align = TRUE, column.sep.width = "0pt", no.space = TRUE, digits= 2,
          out = "tabel-regression-pid.htm")


marg_effekter <- rbind(
  interplot(m = reg_pid_polintr, var1 = "polintr", var2 = "partiid", plot=FALSE) %>% mutate(condition = "Politisk interesse")
) %>% 
  mutate(Partitilknytning = ifelse(partiid == 1, "Ja", "Nej"))

marg_effekter %>% 
  ggplot(aes(x = Partitilknytning, y = coef, ymin = lb, ymax = ub)) +
  geom_point() +
  labs(y = "Effekt") +
  geom_errorbar(width = 0) + 
  facet_wrap(~ condition) + 
  geom_hline(yintercept = 0) +
  theme(text=element_text(family="Times New Roman")) +
  scale_y_continuous(labels=function(x) format(x, decimal.mark = ",", scientific = FALSE))

ggsave("fig2.png", height = 4, width = 5)
