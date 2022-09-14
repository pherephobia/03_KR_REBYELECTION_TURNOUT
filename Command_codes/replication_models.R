# Project Information ---------------------------------------------------------
## Name: 재보궐선거 투표율의 결정요인 분석 (1989-2022)
## Author: Sanghoon Park
## Updated: 2022-09-14
## Log: 

## Package imports -------------------------------------------------------------

pacman::p_load(ezpickr, tidyverse, googlesheets4)
library(showtext)
font_add_google(name = "Nanum Gothic",
                family = "nanumgothic")
showtext_auto(TRUE)

### Theme set ------------------------------------------------------------------

theme_nice <- function() {
  theme_minimal(base_family = "nanumgothic") +
    theme(panel.grid.minor = element_blank(),
          plot.background = element_rect(fill = "white", color = NA),
          plot.title = element_text(face = "bold", size = 25),
          axis.title = element_text(face = "bold"),
          strip.text = element_text(face = "bold", size = 28, hjust = 0),
          strip.background = element_rect(fill = "grey80", color = NA),
          legend.title = element_text(face = "bold", size = 20),
          legend.text = element_text(face = "bold", size = 20),
          text = element_text(family = "nanumgothic", size = 32, lineheight = 0.4))
}
theme_set(theme_nice())

## Data imports ----------------------------------------------------------------

reby_election <- pick("Data/reby_election.csv")

reby_election |> group_by(ELEC_DATE) |>  count() |> ungroup() -> reby_num

reby_election <- reby_election |> 
  left_join(reby_num)

reby_election <- 
  reby_election %>%
  mutate(
    lnELEC_TURNOUT = 
      100 * (ELEC_TURNOUT/(100-ELEC_TURNOUT)),
    lnPRE_TURNOUT = 
      100 * (PRE_TURNOUT/(100-PRE_TURNOUT)),
    ELEC_ILLEGAL = 
      case_when(
        ELEC_REASON %in% c("당선무효", "선거무효", "의원직상실", "피선거권상실") ~ 1L,
        ELEC_REASON %in% c("사망", "사직", "사퇴") ~ 0L,
        T ~ NA_integer_
      ),
    ELEC_partisan = case_when(
      ELEC_RULING == 1L ~ 2L,
      ELEC_OPPOSITION == 1L ~ 3L,
      T ~ 1L
    ),
    lnGenDiff =  log(GENERAL_DIFF+1),
    lnPresDiff = log(PRESIDENT_DIFF+1),
    revn = 1/n,
    identical = if_else(ELEC_PARTY == PRE_PARTY, 1L, 0L),
    identical2 = if_else(ELEC_ELECTED == PRE_ELECTED, NA_integer_,
                         if_else(ELEC_PARTY == PRE_PARTY,  1L, 0L)),
    DIFF = ELEC_SHARE - PRE_SHARE,
    ELEC_REASON_RE0 = factor(ELEC_REASON,
                             levels = c("사망", "사직", "사퇴", "선거무효", "의원직상실", "피선거권상실", "당선무효")),
    ELEC_REASON_RE1 = factor(ELEC_REASON_RE,
                             levels = c(1, 2, 3),
                             labels = c("일신상의 사퇴", "비위로 자진사퇴", "비위로 강제사퇴")),
    ELEC_REASON_RE2 = if_else(ELEC_REASON_RE %in% 3L, 1L, 0L),
    ELEC_REASON_RE2 = factor(ELEC_REASON_RE2,
                             levels = c(0, 1),
                             labels = c("일신상/비위로 자진사퇴",
                                        "비위로 인한 강제사퇴")),
    ELEC_REASON_RE3 = if_else(ELEC_REASON_RE %in% 3L, 1L,
                              if_else(ELEC_REASON_RE %in% 1L, 0L,
                                      NA_integer_)),
    ELEC_REASON_RE3 = factor(ELEC_REASON_RE3,
                             levels = c(0, 1),
                             labels = c("일신상의 사퇴", "비위로 강제사퇴")),
    ELEC_REASON_RE4 = if_else(ELEC_REASON_RE %in% c(2, 3), 1L, 0L),
    ELEC_REASON_RE4 = factor(ELEC_REASON_RE4,
                             levels = c(0, 1),
                             labels = c("일신상의 사퇴", "비위로 강제/자진사퇴")
    ),
    dff_pres = PRESIDENT_POSITIVE - PRESIDENT_NEGATIVE,
    METROPOL = factor(METROPOL,
                      levels = c(1, 2, 3),
                      labels = c("광역시 및 특별시", "중소도시", "읍면군 지역")),
    perct = revn*100,
    compete_region = if_else(ELEC_RULING == 1L, 1L,
                             if_else(ELEC_OPPOSITION == 1L, 2L, 3L)),
    compete_region2 = factor(compete_region,
                             levels = c(3, 1, 2),
                             labels = c("경합지역", "여당 유리 지역", "야당 유리 지역")),
    ELEC_ILLEGAL2 = factor(ELEC_ILLEGAL,
                           levels = c(0, 1),
                           labels = c("일신상의 이유", 
                                      "비위")),
    PREVOTE = factor(PREVOTE, levels = c(0, 1),
                     labels = c("사전투표제 시행 전",
                                "사전투표제 시행 후"))
  )

# 그림 1 종속변수: 재·보궐 선거 투표율의 분포 --------------------------------------------

reby_election %>% drop_na(ELEC_TURNOUT) |> pull(ELEC_TURNOUT) |> summary()
reby_election %>% drop_na(ELEC_TURNOUT) |> 
  ggplot(aes(ELEC_TURNOUT*0.01)) + 
  geom_density(
    aes(y = ..scaled..),
    color = futurevisions::futurevisions("mars")[1],
    fill = futurevisions::futurevisions("mars")[1],
    alpha = 0.4
  ) + 
  scale_y_continuous() + 
  scale_x_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, 0.1),
    labels = scales::percent_format()) + 
  geom_vline(xintercept = round(min(reby_election$ELEC_TURNOUT*0.01, na.rm = T), 1),
             linetype = "dashed") +
  geom_text(
    aes(x = round(min(reby_election$ELEC_TURNOUT*0.01, na.rm = T), 1) + 0.02, y = 0.5),
    label = paste0("재보궐 선거 최소 투표율: ", 
                   round(min(reby_election$ELEC_TURNOUT, na.rm = T), 1), "%"),
    angle = 90, size = 8) +
  geom_vline(xintercept = round(mean(reby_election$ELEC_TURNOUT*0.01, na.rm = T), 1),
             linetype = "dashed") +
  geom_text(
    aes(x = round(mean(reby_election$ELEC_TURNOUT*0.01, na.rm = T), 1) + 0.02, y = 0.5),
    label = paste0("재보궐 선거 평균 투표율: ", 
                   round(mean(reby_election$ELEC_TURNOUT, na.rm = T), 1), "%"),
    angle = 90, size = 8) +
  geom_vline(xintercept = round(max(reby_election$ELEC_TURNOUT*0.01, na.rm = T), 1),
             linetype = "dashed") +
  geom_text(
    aes(x = round(max(reby_election$ELEC_TURNOUT*0.01, na.rm = T), 1) + 0.02, y = 0.5),
    label = paste0("재보궐 선거 최대 투표율: ", 
                   round(max(reby_election$ELEC_TURNOUT, na.rm = T), 1), "%"),
    angle = 90, size = 8) +
  labs(y = "밀도\n", x = "\n재보궐 선거 투표율(%)")
ggsave("Documents/Figures/Fig1_dv_turnout.png", width = 5, height = 3)


# 투표참여모델 -----------------------------------------------------------------

## 일신상의 사직/사퇴 vs. 비위로 인한 자진 사직/사퇴 vs. 비위로 인한 강제사퇴 ----

lm(ELEC_TURNOUT ~ 
     ELEC_REASON_RE1  + perct + lnGenDiff + PRESIDENT_POSITIVE + I(PRESIDENT_POSITIVE^2) +
     PREVOTE +  compete_region2 + ELEC_COMPETE  + PRE_TURNOUT,
   data = reby_election) -> model2A

## 일신상의 사직/사퇴 vs. 비위로 인한 사직/사퇴 -------------------------------

lm(ELEC_TURNOUT ~ 
     ELEC_REASON_RE4  + perct + lnGenDiff + PRESIDENT_POSITIVE + I(PRESIDENT_POSITIVE^2) + 
     PREVOTE + compete_region2 + ELEC_COMPETE  + PRE_TURNOUT, 
   data = reby_election) -> model2D

## 재보궐 시행사유 원변수 ------------------------------------------------------

lm(ELEC_TURNOUT ~ 
     ELEC_REASON_RE0  + perct + lnGenDiff + PRESIDENT_POSITIVE + I(PRESIDENT_POSITIVE^2) + 
     PREVOTE + compete_region2 + ELEC_COMPETE  + PRE_TURNOUT, 
   data = reby_election) -> model2E

model2A$AIC <- AIC(model2A);model2A$BIC <- BIC(model2A)
model2D$AIC <- AIC(model2D);model2D$BIC <- BIC(model2D)
model2E$AIC <- AIC(model2E);model2E$BIC <- BIC(model2E)
texreg::screenreg(list(model2A, model2D, model2E),
                  custom.coef.names = c("(상수항)",
                                        "재보궐1:자진사퇴",
                                        "재보궐1:강제사퇴",
                                        "선거중요도",
                                        "Ln(인접 총선일+1)",
                                        "대통령 국정지지도",
                                        "대통령 국정지지도^2",
                                        "사전투표제",
                                        "여당 유리지역",
                                        "야당 유리지역",
                                        "선거 경합도",
                                        "직전 총선 투표율",
                                        "재보궐2:비위사퇴",
                                        "재보궐:사직",
                                        "재보궐:사퇴",
                                        "재보궐:선거무효",
                                        "재보궐:의원직상실",
                                        "재보궐:피선거권상실",
                                        "재보궐:당선무효"),
                  custom.model.names = c("모델 1", "모델 2", "모델 3"),
                  reorder.coef = c(2, 3, 13, 14, 15, 16, 17, 18, 19, 4, 12, 11, 5, 6, 7,
                                   8, 9, 10, 1),
                  custom.gof.rows = list("AIC" = c(model2A$AIC, model2D$AIC, model2E$AIC),
                                         "BIC" = c(model2A$BIC, model2D$BIC, model2E$BIC)))

## 재보궐 시행사유 예측확률 그리기 --------------------------------------------

beta_model2A<- MASS::mvrnorm(n = 4000, mu = coef(model2A),
                             Sigma = vcov(model2A))

beta_model2D <- MASS::mvrnorm(n = 4000, mu = coef(model2D),
                              Sigma = vcov(model2D))

## 모수 부트스트래핑 -----------------------------------------------------------

### 재보궐 시행사유의 프로필 만들기 --------------------------------------------

### 일신상의 사직/사퇴 vs. 비위로 인한 사직/사퇴 결과에 대해서 예측값 계산

sim_model2D1 <- 
  rbind(
    1, 
    0,
    mean(reby_election$perct, na.rm = T),
    mean(reby_election$lnGenDiff, na.rm = T),
    mean(reby_election$PRESIDENT_POSITIVE, na.rm = T),
    I(mean(reby_election$PRESIDENT_POSITIVE, na.rm = T)^2),
    0, 0, 0, 
    mean(reby_election$ELEC_COMPETE, na.rm = T),
    mean(reby_election$PRE_TURNOUT, na.rm = T)
  )

sim_model2D2 <- 
  rbind(
    1, 
    1,
    mean(reby_election$perct, na.rm = T),
    mean(reby_election$lnGenDiff, na.rm = T),
    mean(reby_election$PRESIDENT_POSITIVE, na.rm = T),
    I(mean(reby_election$PRESIDENT_POSITIVE, na.rm = T)^2),
    0, 0, 0, 
    mean(reby_election$ELEC_COMPETE, na.rm = T),
    mean(reby_election$PRE_TURNOUT, na.rm = T)
  )

pr_model2D1 <- beta_model2D %*% sim_model2D1
pr_model2D2 <- beta_model2D %*% sim_model2D2

pr_model2D <- cbind(pr_model2D1, pr_model2D2)

tibble(
  model = "모델 4",
  `재보궐 시행 사유` = c("일신상 이유로\n자진 사직/사퇴",
                  "비위로 인한\n사직/사퇴"),
  mean = apply(pr_model2D, 2, mean),
  ll = apply(pr_model2D, 2, quantile, 0.025),
  ul = apply(pr_model2D, 2, quantile, 0.975)
) -> Model2D

### 그래프로 예측값 나타내기 ---------------------------------------------------

(zis_colors <- wesanderson::wes_palette("Zissou1", type = "discrete"))

Model2D |>
  mutate(`재보궐 시행 사유` = 
           factor(`재보궐 시행 사유`,
                  levels = c("일신상 이유로\n자진 사직/사퇴",
                             "비위로 인한\n사직/사퇴"))) |> 
  ggplot(aes(x = `재보궐 시행 사유`,
             y = mean*0.01, color = `재보궐 시행 사유`)) + 
  geom_line(show.legend = F) + 
  geom_linerange(aes(ymin = ll*0.01, ymax = ul*0.01), size = 1.2,
                 show.legend = F) +
  geom_point(fill = "white", size = 3, shape = 21, show.legend = F) + 
  scale_y_continuous(limits = c(0.3, 0.5),
                     breaks = c(seq(0.3, 0.5, 0.05)),
                     labels = scales::percent_format()) + 
  labs(x = "\n재보궐 선거 시행 사유",
       y = "재보궐 선거 투표율의 예측값\n",
       title = NULL) + 
  ggrepel::geom_text_repel(aes(x = `재보궐 시행 사유`, y = mean*0.01, 
                               label = paste0(round(mean*0.01, 4)*100, "%"),
                               color = `재보궐 시행 사유`),
                           size = 6, angle = 90,
                           nudge_x = 0,
                           nudge_y = 0,
                           xlim = c(1, Inf), ylim = c(-Inf, Inf),
                           show.legend = F) +
  scale_color_manual(values = c(zis_colors[1],
                                zis_colors[5])) + 
  #facet_wrap(~model, ncol = 2) + 
  theme_nice() +
  theme(axis.text.x =  element_text(face = "bold", size = 18))

### 그림 2 재·보궐 선거 시행 사유에 따른 재·보궐 선거 투표율의 예측값 저장하기

ggsave("Documents/Figures/Fig2_turnout_elec_reason_updated.png",
       width = 5, height = 3)


# 투표결과모델 -----------------------------------------------------------------

## 전체 ------------------------------------------------------------------------

glm(identical2 ~ ELEC_REASON_RE4 + compete_region2 +
      ELEC_COMPETE + PRESIDENT_POSITIVE + RULINGPARTY +
      ELEC_NUMBER, data = reby_election, 
    family = "binomial") -> bimodel1

texreg::screenreg(bimodel1,
                  custom.coef.names = c("(상수항)",
                                        "비위로 인한 재보궐 여부",
                                        "여당유리지역",
                                        "야당유리지역",
                                        "선거 경합도",
                                        "대통령 국정지지도",
                                        "여당 후보",
                                        "당선자 선수"),
                  custom.model.names = c("전체"),
                  reorder.coef = c(2, 8, 7, 5, 6, 3, 4, 1))

## 여당 유리지역 ---------------------------------------------------------------

glm(identical2 ~ ELEC_REASON_RE4 + ELEC_NUMBER +RULINGPARTY +
      ELEC_COMPETE + PRESIDENT_POSITIVE, 
    family = "binomial", 
    data = reby_election |> dplyr::filter(compete_region == 1L)) -> model2CA

## 야당 유리지역 ---------------------------------------------------------------

glm(identical2 ~ ELEC_REASON_RE4 + ELEC_NUMBER + RULINGPARTY +
      ELEC_COMPETE + PRESIDENT_POSITIVE, 
    family = "binomial", 
    data = reby_election |> dplyr::filter(compete_region == 2L)) -> model2CB

## 경합지역 --------------------------------------------------------------------

glm(identical2 ~ ELEC_REASON_RE4 + ELEC_NUMBER +RULINGPARTY +
      ELEC_COMPETE + PRESIDENT_POSITIVE, 
    family = "binomial", 
    data = reby_election |> dplyr::filter(compete_region == 3L)) -> model2CC

texreg::screenreg(list(model2CA, model2CB, model2CC),
                  custom.coef.names = c("(상수항)",
                                        "강제사퇴로 인한 재보궐 여부",
                                        "당선자 선수",
                                        "여당 후보자",
                                        "선거 경합도",
                                        "대통령 국정지지도"),
                  custom.model.names = c("여당 유리 지역", 
                                         "야당 유리 지역",
                                         "경합 지역"),
                  reorder.coef = c(2, 4, 5, 6, 3, 1),
                  single.row = F)


## 모수 부트스트래핑 -----------------------------------------------------------

### 재보궐 시행사유 예측확률 그리기 

beta_bimodel1<- MASS::mvrnorm(n = 4000, mu = coef(bimodel1),
                              Sigma = vcov(bimodel1))
beta_bimodel2 <- MASS::mvrnorm(n = 4000, mu = coef(model2CA),
                               Sigma = vcov(model2CA))
beta_bimodel3 <- MASS::mvrnorm(n = 4000, mu = coef(model2CB),
                               Sigma = vcov(model2CB))
beta_bimodel4 <- MASS::mvrnorm(n = 4000, mu = coef(model2CC),
                               Sigma = vcov(model2CC))

### 재보궐 시행사유의 프로필 만들기 --------------------------------------------

sim_bimodel1A <- 
  rbind(
    1, 
    0, 
    0, 0,
    mean(reby_election$ELEC_COMPETE, na.rm = T),
    mean(reby_election$PRESIDENT_POSITIVE, na.rm = T),
    median(reby_election$RULINGPARTY, na.rm = T),
    mean(reby_election$ELEC_NUMBER, na.rm = T)
  )

sim_bimodel1B <- 
  rbind(
    1, 
    1, 
    0, 0,
    mean(reby_election$ELEC_COMPETE, na.rm = T),
    mean(reby_election$PRESIDENT_POSITIVE, na.rm = T),
    median(reby_election$RULINGPARTY, na.rm = T),
    mean(reby_election$ELEC_NUMBER, na.rm = T)
  )

pr_bimodel1A <- beta_bimodel1 %*% sim_bimodel1A
pr_bimodel1B <- beta_bimodel1 %*% sim_bimodel1B


pr_bimodel1 <- cbind(pr_bimodel1A, pr_bimodel1B)

tibble(
  model = "전체",
  `재보궐 시행 사유` = c("재보궐 사유:\n일신상 사직/사퇴",
                  "재보궐 사유:\n비위 사직/사퇴"),
  mean = plogis(apply(pr_bimodel1, 2, mean)),
  ll = plogis(apply(pr_bimodel1, 2, quantile, 0.025)),
  ul = plogis(apply(pr_bimodel1, 2, quantile, 0.975))
) -> Bimodel1

sim_bimodel2A <- 
  rbind(
    1, 
    0, 
    mean(reby_election$ELEC_NUMBER, na.rm = T),
    median(reby_election$RULINGPARTY, na.rm = T),
    mean(reby_election$ELEC_COMPETE, na.rm = T),
    mean(reby_election$PRESIDENT_POSITIVE, na.rm = T)
  )

sim_bimodel2B <- 
  rbind(
    1, 
    1, 
    mean(reby_election$ELEC_NUMBER, na.rm = T),
    median(reby_election$RULINGPARTY, na.rm = T),
    mean(reby_election$ELEC_COMPETE, na.rm = T),
    mean(reby_election$PRESIDENT_POSITIVE, na.rm = T)
  )

pr_bimodel2A <- beta_bimodel2 %*% sim_bimodel2A
pr_bimodel2B <- beta_bimodel2 %*% sim_bimodel2B


pr_bimodel2 <- cbind(pr_bimodel2A, pr_bimodel2B)

tibble(
  model = "여당 유리 지역",
  `재보궐 시행 사유` = c("재보궐 사유:\n일신상 사직/사퇴",
                  "재보궐 사유:\n비위 사직/사퇴"),
  mean = plogis(apply(pr_bimodel2, 2, mean)),
  ll = plogis(apply(pr_bimodel2, 2, quantile, 0.025)),
  ul = plogis(apply(pr_bimodel2, 2, quantile, 0.975))
) -> Bimodel2

sim_bimodel3A <- 
  rbind(
    1, 
    0, 
    mean(reby_election$ELEC_NUMBER, na.rm = T),
    median(reby_election$RULINGPARTY, na.rm = T),
    mean(reby_election$ELEC_COMPETE, na.rm = T),
    mean(reby_election$PRESIDENT_POSITIVE, na.rm = T)
  )

sim_bimodel3B <- 
  rbind(
    1, 
    1, 
    mean(reby_election$ELEC_NUMBER, na.rm = T),
    median(reby_election$RULINGPARTY, na.rm = T),
    mean(reby_election$ELEC_COMPETE, na.rm = T),
    mean(reby_election$PRESIDENT_POSITIVE, na.rm = T)
  )

pr_bimodel3A <- beta_bimodel3 %*% sim_bimodel3A
pr_bimodel3B <- beta_bimodel3 %*% sim_bimodel3B


pr_bimodel3 <- cbind(pr_bimodel3A, pr_bimodel3B)

tibble(
  model = "야당 유리 지역",
  `재보궐 시행 사유` = c("재보궐 사유:\n일신상 사직/사퇴",
                  "재보궐 사유:\n비위 사직/사퇴"),
  mean = plogis(apply(pr_bimodel3, 2, mean)),
  ll = plogis(apply(pr_bimodel3, 2, quantile, 0.025)),
  ul = plogis(apply(pr_bimodel3, 2, quantile, 0.975))
) -> Bimodel3

sim_bimodel4A <- 
  rbind(
    1, 
    0, 
    mean(reby_election$ELEC_NUMBER, na.rm = T),
    median(reby_election$RULINGPARTY, na.rm = T),
    mean(reby_election$ELEC_COMPETE, na.rm = T),
    mean(reby_election$PRESIDENT_POSITIVE, na.rm = T)
  )

sim_bimodel4B <- 
  rbind(
    1, 
    1, 
    mean(reby_election$ELEC_NUMBER, na.rm = T),
    median(reby_election$RULINGPARTY, na.rm = T),
    mean(reby_election$ELEC_COMPETE, na.rm = T),
    mean(reby_election$PRESIDENT_POSITIVE, na.rm = T)
  )

pr_bimodel4A <- beta_bimodel4 %*% sim_bimodel4A
pr_bimodel4B <- beta_bimodel4 %*% sim_bimodel4B

pr_bimodel4 <- cbind(pr_bimodel4A, pr_bimodel4B)

tibble(
  model = "경합 지역",
  `재보궐 시행 사유` = c("재보궐 사유:\n일신상 사직/사퇴",
                  "재보궐 사유:\n비위 사직/사퇴"),
  mean = plogis(apply(pr_bimodel4, 2, mean)),
  ll = plogis(apply(pr_bimodel4, 2, quantile, 0.025)),
  ul = plogis(apply(pr_bimodel4, 2, quantile, 0.975))
) -> Bimodel4

bind_rows(Bimodel1, Bimodel2, Bimodel3, Bimodel4) -> Bimodel4_set

Bimodel4_set |> mutate(
  model_color = case_when(
    `재보궐 시행 사유` %in% c("재보궐 사유:\n일신상 사직/사퇴") ~ 0L,
    `재보궐 시행 사유` %in% c("재보궐 사유:\n비위 사직/사퇴") ~ 1L,
    T ~ NA_integer_
  ),
  model_color = factor(model_color, levels = c(0, 1))
) -> Bimodel4_set

## 그래프로 예측확률 나타내기 --------------------------------------------------

Bimodel4_set |> 
  mutate(model = factor(model,
                        levels = c("전체", "여당 유리 지역",
                                   "야당 유리 지역", "경합 지역")),
         sigcolor = c(1, 2, 3, 3, 3, 3, 1, 2),
         sigcolor = factor(sigcolor, 
                           levels = c(1, 2, 3))) |> 
  ggplot(aes(x = `재보궐 시행 사유`,
             y = mean, color = sigcolor)) + 
  geom_line(show.legend = F) + 
  geom_linerange(aes(ymin = ll, ymax = ul), size = 1.2,
                 show.legend = F) +
  geom_point(fill = "white", size = 3, shape = 21, show.legend = F) + 
  scale_y_continuous(limits = c(0, 0.9),
                     breaks = c(seq(0, 0.9, 0.15)),
                     labels = scales::percent_format()) + 
  labs(x = "\n재보궐 선거 시행 사유",
       y = "직전 당선자와 동일 정당 후보의 당선 예측확률\n",
       title = NULL) + 
  ggrepel::geom_text_repel(aes(x = `재보궐 시행 사유`, y = ll+0.15, 
                               label = paste0(round(mean, 4)*100, "%"),
                               color = sigcolor),
                           min.segment.length = Inf,
                           size = 8, angle = 90,
                           # nudge_x = -0.1,
                           xlim = c(1, Inf), ylim = c(-Inf, Inf),
                           show.legend = F) +
  scale_color_manual(values = c(
    zis_colors[1],zis_colors[5],"grey80")) + 
  facet_wrap(~model, ncol = 4) + theme_nice() +
  theme(axis.text.x =  element_text(face = "bold", size = 18))

## 그림 3 재·보궐 선거 시행사유에 따른 전직자와 동일 정당 후보의 당선 예측확률

ggsave("Documents/Figures/Fig3_identical_elec_reason_updated.png",
       width = 8, height = 4)


# 부록 -------------------------------------------------------------------------

## 표 A1 민주화 이후 국회의원 재·보궐 선거 실시 및 투표율 현황: 1989-4-14 ~ 2022-6-1 ------

reby_election %>% 
  dplyr::select(ELEC_DATE, CITY, DISTRICT, ELEC_REASON, ELEC_TURNOUT) %>%
  mutate_if(is.numeric, round, 1) %>%
  group_by(ELEC_DATE, CITY, DISTRICT, ELEC_REASON, ELEC_TURNOUT) %>%
  knitr::kable(
    col.names = c("재보궐 선거일", "시도", "지역구", "재보궐 사유", "투표율"),
    caption = "재보궐선거 지역구 및 재보궐 사유에 대한 정보",
    longtable = T, format = "latex") %>% 
  kableExtra::kable_minimal()

## 표 A2 주요 변수의 기술통계(전체) --------------------------------------------

reby_election |> 
  dplyr::select(ELEC_TURNOUT, ELEC_REASON_RE2, revn, lnGenDiff, PRESIDENT_POSITIVE, 
                ELEC_NUMBER, ELEC_RULING, ELEC_OPPOSITION, ELEC_COMPETE, PRE_TURNOUT, 
                identical, ELEC_ILLEGAL2) |> psych::describe()

## 그림 A1 재·보궐 시행 사유의 분포 --------------------------------------------

reby_election %>%
  dplyr::select(ELEC_REASON) |> 
  group_by(ELEC_REASON) |>  count() |> 
  mutate(n = n/146) |> 
  ggplot(aes(ELEC_REASON, y = n, fill = ELEC_REASON)) + 
  geom_bar(stat = "identity", show.legend = F) + 
  geom_text(aes(label = paste0(round(n*100, 1), "%")), 
            position = position_stack(0.8), color = "white",
            size = 9) + 
  labs(y = "밀도\n", x = NULL) + 
  viridis::scale_fill_viridis(
    begin = 0.2, end = 0.65,
    discrete = T) + 
  scale_y_continuous(labels = scales::percent_format(),
                     limits = c(0, 0.4))

## 그림 A2 양변량 분석: 재·보궐 시행 사유별 재·보궐 선거 투표율의 분포 --------

reby_election %>%
  ggplot(aes(ELEC_TURNOUT*0.01, fill = ELEC_REASON)) + 
  geom_boxplot(
    alpha = 0.8, show.legend = F
  ) + labs(y = NULL, x = "재보궐 선거 투표율") + 
  viridis::scale_fill_viridis(discrete = T) +
  viridis::scale_color_viridis(discrete = T) +
  scale_x_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
  facet_wrap(~ELEC_REASON, ncol = 7) + coord_flip() + 
  theme(axis.text.x = element_blank())

## 그림 A3 양변량 분석: 지역균열에 따른 재·보궐 선거 투표율의 분포 -------------

reby_election %>%
  ggplot(aes(ELEC_TURNOUT*0.01, fill = compete_region2)) + 
  geom_boxplot(
    alpha = 0.8, show.legend = F
  ) + labs(y = NULL, x = "재보궐 선거 투표율") + 
  scale_fill_manual(values = c(futurevisions::futurevisions("mars")[2],
                               futurevisions::futurevisions("mars")[3],
                               futurevisions::futurevisions("mars")[1])) +
  viridis::scale_color_viridis(discrete = T) +
  scale_x_continuous(labels = scales::percent_format(), limits = c(0, 1)) +
  facet_wrap(~compete_region2, ncol = 3) + coord_flip() + 
  theme(axis.text.x = element_blank())

