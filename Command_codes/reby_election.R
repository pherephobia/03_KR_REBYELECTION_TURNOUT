## Project Information ---------------------------------------------------------
## Name: 재보궐선거 투표율의 결정요인 분석 (1989-2020)
## Author: Sanghoon Park
## Updated: 2022-06-16 
## Log: 

## Package imports -------------------------------------------------------------
pacman::p_load(ezpickr, tidyverse, googlesheets4)
library(showtext)
font_add_google(name = "Nanum Gothic",
                family = "nanumgothic")

showtext_auto(TRUE)
theme_set(theme_bw())

## Data imports ----------------------------------------------------------------
reby_election <- read_sheet("https://docs.google.com/spreadsheets/d/1doUuNVPYW6t0ie7PbVBYFxcTkG6QYl0KFB0mj9BqGxc/edit?usp=sharing")
reby_election |> group_by(ELEC_DATE) |>  count() |> ungroup() -> reby_num

reby_election <- reby_election |> 
  left_join(reby_num)
table(reby_election$ELEC_REASON)
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
    identical2 = if_else(ELEC_PARTY == PRE_PARTY, 1L,
                         if_else(ELEC_ELECTED == PRE_ELECTED, NA_integer_, 0L)),
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
                             )
  )

lm(ELEC_TURNOUT ~ 
     ELEC_REASON_RE1  + revn + lnGenDiff + PRESIDENT_POSITIVE + I(PRESIDENT_POSITIVE^2) +
     ELEC_NUMBER + ELEC_RULING + ELEC_OPPOSITION + ELEC_COMPETE  + PRE_TURNOUT,
   data = reby_election) -> model2A

lm(ELEC_TURNOUT ~ 
     ELEC_REASON_RE2  + revn + lnGenDiff + PRESIDENT_POSITIVE + I(PRESIDENT_POSITIVE^2) + 
     ELEC_NUMBER + ELEC_RULING + ELEC_OPPOSITION + ELEC_COMPETE  + PRE_TURNOUT, 
   data = reby_election) -> model2B

lm(ELEC_TURNOUT ~ 
     ELEC_REASON_RE3  + revn + lnGenDiff + PRESIDENT_POSITIVE + I(PRESIDENT_POSITIVE^2) + 
     ELEC_NUMBER + ELEC_RULING + ELEC_OPPOSITION + ELEC_COMPETE  + PRE_TURNOUT, 
   data = reby_election) -> model2C

lm(ELEC_TURNOUT ~ 
     ELEC_REASON_RE4  + revn + lnGenDiff + PRESIDENT_POSITIVE + I(PRESIDENT_POSITIVE^2) + 
     ELEC_NUMBER + ELEC_RULING + ELEC_OPPOSITION + ELEC_COMPETE  + PRE_TURNOUT, 
   data = reby_election) -> model2D

lm(ELEC_TURNOUT ~ 
     ELEC_REASON_RE0  + revn + lnGenDiff + PRESIDENT_POSITIVE + I(PRESIDENT_POSITIVE^2) + 
     ELEC_NUMBER + ELEC_RULING + ELEC_OPPOSITION + ELEC_COMPETE  + PRE_TURNOUT, 
   data = reby_election) -> model2E

model2A$AIC <- AIC(model2A);model2A$BIC <- BIC(model2A)
model2B$AIC <- AIC(model2B);model2B$BIC <- BIC(model2B)
model2C$AIC <- AIC(model2C);model2C$BIC <- BIC(model2C)
model2D$AIC <- AIC(model2D);model2D$BIC <- BIC(model2D)
model2E$AIC <- AIC(model2D);model2E$BIC <- BIC(model2D)
texreg::screenreg(list(model2A, model2B, model2C, model2D, model2E),
                  custom.coef.names = c("(상수항)",
                                        "재보궐1:자진사퇴",
                                        "재보궐1:강제사퇴",
                                        "선거중요도",
                                        "Ln(인접 총선일+1)",
                                        "대통령 국정지지도",
                                        "대통령 국정지지도^2",
                                        "당선자 선수",
                                        "여당 유리지역",
                                        "야당 유리지역",
                                        "선거 경합도",
                                        "직전 총선 투표율",
                                        "재보궐2:강제사퇴",
                                        "재보궐3:강제사퇴",
                                        "재보궐4:비위사퇴",
                                        "재보궐:사직",
                                        "재보궐:사퇴",
                                        "재보궐:선거무효",
                                        "재보궐:의원직상실",
                                        "재보궐:피선거권상실",
                                        "재보궐:당선무효"),
                  custom.model.names = c("모델 1", "모델 2", "모델 3", "모델 4", "모델 5"),
                  reorder.coef = c(16, 17, 18, 19, 20, 21, 2, 3, 
                                   13, 14, 15, 11, 12, 8, 4, 5, 6, 7, 9, 10, 1),
                  custom.gof.rows = list("AIC" = c(model2A$AIC, model2B$AIC, model2C$AIC, model2D$AIC, model2E$AIC),
                                         "BIC" = c(model2A$BIC, model2B$BIC, model2C$BIC, model2D$BIC, model2E$BIC)))

ggeffects::ggpredict(model2B, terms = c("revn")) |> plot() + labs(x = "\n선거 중요도", y = "재보궐 선거 투표율(%)\n",
                                                                  title = NULL)
ggeffects::ggpredict(model2B, terms = c("lnGenDiff")) |> plot() + labs(x = "\nLog(인접 총선일)", y = "재보궐 선거 투표율(%)\n",
                                                                       title = NULL)
ggeffects::ggpredict(model2B, terms = c("ELEC_RULING")) |> plot() 
ggeffects::ggpredict(model2B, terms = c("ELEC_OPPOSITION")) |> plot()
ggeffects::ggpredict(model2B, terms = c("PRESIDENT_POSITIVE")) |> plot() + labs(x = "\n대통령 국정지지도(%)", y = "재보궐 선거 투표율(%)\n",
                                                                                title = NULL)
ggeffects::ggpredict(model2B, terms = c("ELEC_COMPETE")) |> plot() + labs(x = "\n재보궐 선거 경합도\n(100 * (2위 득표자 득표율 / 해당 재보궐 선거에서 당선자 득표율))", y = "재보궐 선거 투표율(%)\n",
                                                                         title = NULL)
ggeffects::ggpredict(model2B, terms = c("PRE_TURNOUT")) |> plot() + labs(x = "\n직전 총선 투표율(%)", y = "재보궐 선거 투표율(%)\n",
                                                                         title = NULL)
ggeffects::ggpredict(model2A, terms = c("ELEC_REASON_RE1")) |> plot()
ggeffects::ggpredict(model2B, terms = c("ELEC_REASON_RE2")) |> plot() + labs(x = "\n재보궐 선거 시행 사유", y = "재보궐 선거 투표율(%)\n",
                                                                             title = NULL)
ggeffects::ggpredict(model2C, terms = c("ELEC_REASON_RE3")) |> plot()
ggeffects::ggpredict(model2D, terms = c("ELEC_REASON_RE0")) |> plot()

lm(ELEC_COMPETE ~ 
     ELEC_ILLEGAL + revn + lnGenDiff + PRE_COMPETE + ELEC_NUMBER + ELEC_TURNOUT,
   data = reby_election) -> model3

lm(ELEC_TURNOUT ~ 
     ELEC_REASON_RE2  + revn + lnGenDiff + PRESIDENT_POSITIVE + I(PRESIDENT_POSITIVE^2) + 
     ELEC_NUMBER + ELEC_RULING + ELEC_OPPOSITION + ELEC_COMPETE  + PRE_TURNOUT + as.factor(PRESIDENT), 
   data = reby_election) -> test
texreg::screenreg(test, omit.coef = "as.factor")

car::vif(model2A);car::vif(model2B);car::vif(model2C);car::vif(model2D)

texreg::screenreg(list(model3))

ggeffects::ggpredict(model3, terms = c("ELEC_REASON_RE")) |> plot()
ggeffects::ggpredict(model3, terms = c("lnGenDiff")) |> plot()
ggeffects::ggpredict(model3, terms = c("PRE_COMPETE")) |> plot()

lm(ELEC_TURNOUT ~ 
     revn + lnGenDiff + lnPresDiff + ELEC_RULING + ELEC_OPPOSITION +ELEC_COMPETE  + PRESIDENT_POSITIVE + I(PRESIDENT_POSITIVE^2) + 
     PRE_TURNOUT + ELEC_ILLEGAL, data = reby_election) -> model3
car::vif(model1);car::vif(model2);car::vif(model3)

reby_election |> mutate(
  ELEC_ILLEGAL2 = factor(ELEC_ILLEGAL,
                         levels = c(0, 1),
                         labels = c("일신상의 이유로 인한\n사퇴/사직으로 재보궐", "불법적 사유(당선무효, 선거무효,\n피선거권 상실 등)로\n재보궐"))
) -> reby_election

glm(identical ~ ELEC_ILLEGAL2 + revn + lnGenDiff +  
      ELEC_RULING + ELEC_OPPOSITION + ELEC_COMPETE + PRESIDENT_POSITIVE + I(PRESIDENT_POSITIVE^2) +
      ELEC_NUMBER, data = reby_election, 
    family = "binomial") -> bimodel1
glm(identical2 ~ ELEC_ILLEGAL + revn + lnGenDiff +
      ELEC_RULING + ELEC_OPPOSITION + ELEC_COMPETE + PRESIDENT_POSITIVE + I(PRESIDENT_POSITIVE^2) +
      ELEC_NUMBER, data = reby_election, 
    family = "binomial") -> bimodel2


texreg::screenreg(list(bimodel1),
                  custom.coef.names = c("(상수항)",
                                        "강제사퇴로 인한 재보궐 여부",
                                        "선거중요도",
                                        "Ln(인접 총선일+1)",
                                        "여당 유리지역",
                                        "야당 유리지역",
                                        "선거 경합도",
                                        "대통령 국정지지도",
                                        "대통령 국정지지도^2",
                                        "당선자 선수"),
                  custom.model.names = c("모델 5"),
                  reorder.coef = c(2, 7, 10, 3, 4, 8, 9, 5, 6, 1),
                  single.row = T)


ggeffects::ggpredict(bimodel1, terms = c("ELEC_ILLEGAL2")) |> plot() + labs(x = "\n비위에 따른 강제사퇴로 인한 재보궐 여부", 
                                                                            y = "재보궐 선거를 야기한 정당 후보가 재보궐 선거에서 당선될 예측확률(%)\n",
                                                                            title = NULL)
ggeffects::ggpredict(bimodel2, terms = c("ELEC_ILLEGAL")) |> plot()
ggeffects::ggpredict(bimodel1, terms = c("PRESIDENT_POSITIVE [all]")) |> plot()
ggeffects::ggpredict(bimodel2, terms = c("PRESIDENT_POSITIVE [all]")) |> plot()
names(reby_election)
texreg::screenreg(list(model6))

reby_election |> 
  dplyr::select(ELEC_TURNOUT, ELEC_REASON_RE2, revn, lnGenDiff, PRESIDENT_POSITIVE, 
  ELEC_NUMBER, ELEC_RULING, ELEC_OPPOSITION, ELEC_COMPETE, PRE_TURNOUT, 
  identical, ELEC_ILLEGAL2) |> psych::describe()
prop.table(table(reby_election$ELEC_ILLEGAL2))
