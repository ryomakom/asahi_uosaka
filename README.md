<div style="background:#f5f5f5; padding:10px 16px; border-left:4px solid #4a90e2; margin:20px 0; font-size:14px;">
  <strong>Note for English readers:</strong>
  This page is written in Japanese. You can read it in English using your browser’s built-in translation feature 
  (e.g., Chrome: <em>Right-click → “Translate to English”</em>).
</div>

<div class="container-fluid main-container">



# 朝日・阪大調査データ整形用コード

<div align="right">
朝日新聞デジタル企画報道部　小宮山亮磨  <br>
@ryomakom  <br>
2025/12/3  </div>


2025年7月の参院選に向けて実施した「朝日・阪大調査」のデータを整形するコードです。調査結果は朝日新聞で[連載「ネット意識と選挙」](https://www.asahi.com/rensai/list.html?id=920)として発表しました。

三浦麻子・阪大教授が元データを[公開](https://team1mile.com/asarinlab/2025/09/09/uosaka-asahisurvey/)しています。

まずは各調査の生データを読み込んで統合します。

```{r}
library(tidyverse)
df_202502 <- read_csv("data/202502SurveyDatFull.csv")
df_202503 <- read_csv("data/202503SurveyDatFull.csv")
df_202504 <- read_csv("data/202504SurveyDatFull.csv")
df_202505fresh <- read_csv("data/202505freshSurveyDatFull.csv")
df_202505recontact <- read_csv("data/202505recontactSurveyDatFull.csv")
df_202506 <- read_csv("data/202506SurveyDatFull.csv")
df_20250701 <- read_csv("data/20250701SurveyDatFull.csv")
df_20250707 <- read_csv("data/20250707SurveyDatFull.csv")
df_20250712 <- read_csv("data/20250712SurveyDatFull.csv")
df_20250716 <- read_csv("data/20250716SurveyDatFull.csv")
df_20250718 <- read_csv("data/20250718SurveyDatFull.csv")



# 🔹 1. 入力データフレームとサフィックス
dfs <- list(
  df_202502,
  df_202503,
  df_202504,
  df_202505fresh,
  df_202505recontact,
  df_202506,
  df_20250701,
  df_20250707,
  df_20250712,
  df_20250716,
  df_20250718)

sources <- c("_02", "_03", "_04", "_05fresh", "_05recontact", "_06", "_0701","_0707",
             "_0712", "_0716", "_0718")

# 🔹 長い形式に統一してから結合（suffixはまだつけない）
long_all <- map2(dfs, sources, ~
  .x %>%
    mutate(across(everything(), as.character)) %>%  # PSID も含めて全部 character
    pivot_longer(-PSID, names_to = "var", values_to = "value") %>%
    mutate(source = .y)
) %>%
  bind_rows()

# 🔹 3. 衝突がある変数（PSID内で異なる値がある）を検出
conflicting_vars <- long_all %>%
  filter(!is.na(value)) %>%
  group_by(PSID, var) %>%
  summarise(n_unique = n()) %>%
  filter(n_unique > 1) %>%
  ungroup() %>% 
  dplyr::select(var) %>%
  distinct() %>% 
  pull()

# 🔹 4. suffixを衝突している変数(と、6月から政党コードが変わった政党の好き嫌い変数)にだけ付与
long_all <- long_all %>%
  mutate(var = if_else(var %in% conflicting_vars |
                         str_detect(var,"party_preference_") |
                         str_detect(var,"feelthermo_party"),
                       paste0(var, source), var))


# ✅ ⛔ 検証ポイント：suffix付きでも重複が残っていないか確認
long_all %>%
  filter(!is.na(value)) %>%
  group_by(PSID,var) %>%
  summarize(n=n()) %>%
  filter(n>1)

# 🔹 5. 最終整形
dat <- long_all %>%
  dplyr::select(PSID, var, value) %>%
  filter(!is.na(value)) %>% 
  pivot_wider(names_from = var,values_from = value)

dat_simple <- dat %>%
  dplyr::select(PSID,
                contains("label")) %>% 
  pivot_longer(cols=-PSID,names_to = "category",values_to = "value") %>% 
  mutate(category2=str_split_i(category,"_",1),
         time=str_split_i(category,"_",-1)) %>% 
  select(PSID,category2,time,value) %>% 
  rename(category=category2) %>% 
  mutate(value=ifelse(value=="自由民主党","自民党",value))


```

調査対象者は約5200人おり、それぞれにPSIDというIDがふられています。このPSIDひとつひとつについて、各調査への回答をひもづけたオブジェクトを作っていきます。元データでは各回答がコード化されていて、その回答が何を示すかわからなかったり、また回答者の各種特性（権威主義的傾向などなどたくさんあります）を複数の質問ではかっていて、そのままでは扱いづらいことがあるので、こうした問題を解消していきます。

```{r}

# 好きな政党と嫌いな政党のコード
p_party <- tibble(
  party_code = c(1, 2, 3, 11, 
                 4, 5, 6,
                 7, 8, 14,
                 15,16,17,
                 18,19,20,21),
  party_name = c("自民","立民","維新","公明",
                 "国民","共産","れいわ",
                 "参政","保守","社民",
                 "再生","NHK","みんつく",
                 "みらい","改革","無所連","誠真会"),
  
  party_name_long = c("自民党","立憲民主党","日本維新の会","公明党",
                      "国民民主党","共産党","れいわ新選組",
                      "参政党","日本保守党","社民党",
                      "再生の道","NHK党","みんなでつくる党",
                      "チームみらい","日本改革党","無所属連合","日本誠真会"),
  main_party=c(1,1,1,1,
               1,1,1,
               1,1,1,
               0,0,0,
               0,0,0,0)) %>% 
  mutate(party_code=as.character(party_code))


attitude_toward_parties <- dat %>%
  dplyr::select(PSID,contains("party_preference")) %>%
  dplyr::select(PSID,contains("GROUP")) %>%
  pivot_longer(cols=-c(PSID),names_to = "category",values_to = "value") %>%
  mutate(prefer_code=str_split_i(category,"_",3),
         party_code=str_split_i(category,"_",5),
         time=str_split_i(category,"_",6)) %>%
  mutate(attitude=ifelse(prefer_code==0,"like","hate")) %>%
  left_join(p_party) %>%
  filter(!is.na(value)) %>%
  dplyr::select(PSID,time,attitude,party_name) %>%
  mutate(category=str_c("party_",attitude,"d_",time)) %>%
  dplyr::select(PSID,category,party_name) %>%
  filter(!is.na(party_name)) %>% 
  pivot_wider(names_from = category,values_from = party_name) %>% 
  select(PSID,contains("party_liked"),contains("party_hated"))

# 支持する政党と支持しない政党（2月調査のみ）
party_support<- dat %>%
  dplyr::select(PSID,contains("party_support")) %>%
  dplyr::select(PSID,contains("GROUP")) %>%
  pivot_longer(cols=-c(PSID),names_to = "category",values_to = "value") %>%
  mutate(support_code=str_split_i(category,"_",3),
         party_code=str_split_i(category,"_",5))%>%
  mutate(attitude=ifelse(support_code==0,"support","not-support")) %>%
  left_join(p_party) %>%
  filter(!is.na(value))  %>%
  dplyr::select(PSID,attitude,party_name) %>%
  mutate(category=str_c("party_",attitude,"ed")) %>%
  dplyr::select(PSID,category,party_name) %>%
  filter(!is.na(party_name))  %>% 
  pivot_wider(names_from = category,values_from = party_name)



# 投票する政党
v_party2_5 <- tibble(
  party_code = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10,
                 11, 12, 13, 14),
  party_name = c("自民", "立民", "維新", "公明", "国民", "共産", "れいわ", "参政", "保守", "社民",
                 "その他", "わからない", "答えない", "投票しない")
) %>% 
  mutate(party_code=as.character(party_code))

v_party2_5 <- bind_rows(v_party2_5 %>% mutate(time="02"),
                        v_party2_5 %>% mutate(time="03"),
                        v_party2_5 %>% mutate(time="04"),
                        v_party2_5 %>% mutate(time="05fresh"),
                        v_party2_5 %>% mutate(time="05recontact"))


v_party6_7 <- tibble(
  party_code = c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10,
                 11, 12, 13, 14,
                 15, 16, 17,
                 18, 19, 20, 21
                 ),
  #party_name = c("自民", "立民", "維新", "公明", "国民", "共産", "れいわ", "参政", "保守", "社民",
  #               "再生の道", "NHK", "みんつく", "その他",
  #               "わからない","答えない","投票しない")
  party_name = c(
  "1" = "自民",
  "2" = "立民",
  "3" = "維新",
  "4" = "公明",
  "5" = "国民",
  "6" = "共産",
  "7" = "れいわ",
  "8" = "参政",
  "9" = "保守",
  "10" = "社民",
  "11" = "その他",
  "12" = "わからない",
  "13" = "答えない",
  "14" = "投票しない",
  "15" = "再生",
  "16" = "NHK",
  "17" = "みんつく",
  "18" = "みらい",
  "19" = "改革",
  "20" = "無所連",
  "21" = "誠真会"
  )
) %>% 
  mutate(party_code=as.character(party_code))

v_party6_7 <- bind_rows(v_party6_7 %>% mutate(time="06"),
                        v_party6_7 %>% mutate(time="0701"),
                        v_party6_7 %>% mutate(time="0707"),
                        v_party6_7 %>% mutate(time="0712"),
                        v_party6_7 %>% mutate(time="0716"),
                        v_party6_7 %>% mutate(time="0718"),)

v_party <- bind_rows(v_party2_5,v_party6_7)

party_to_vote <- dat %>%
  dplyr::select(PSID,contains("voteparty")) %>%
  dplyr::select(-contains("TEXT")) %>%
  pivot_longer(cols=-c(PSID),names_to = "category",values_to = "value") %>%
  mutate(category=str_c("party_to_vote_",str_split_i(category,"_",2))) %>%
  mutate(time=str_split_i(category,"_",4)) %>% 
  mutate(party_code=as.character(value)) %>%
  left_join(v_party) %>%
  select(PSID,category,party_name) %>%
  pivot_wider(names_from = category,
              values_from = party_name)


# 政党への感情温度
thermo_party <- dat %>%
  dplyr::select(matches("PSID|p_feelthermo_party")) %>%
  pivot_longer(cols=-c(PSID),names_to = "category",values_to = "value") %>%
  mutate(prefer_code=str_split_i(category,"_",3),
         party_code=str_split_i(category,"_",4),
         time=str_split_i(category,"_",5)) %>%
  mutate(prefer=ifelse(str_sub(prefer_code,-1,-1)=="1","好き_","嫌い_")) %>%
  left_join(p_party) %>%
  #mutate(category=str_c(party_name,prefer,"感情温度_",time)) %>%
  mutate(category=str_c(prefer,"感情温度_",time)) %>%
  dplyr::select(PSID,category,value) %>%
  filter(!is.na(category)) %>% 
  filter(!is.na(value)) %>% 
  mutate(value=as.double(value)) %>% 
  pivot_wider(names_from = category,values_from = value)


# 好きな/嫌いな政党を応援する/嫌いな人への感情温度
thermo_person <- dat %>%
  dplyr::select(matches("PSID|p_feelthermo_person")) %>%
  pivot_longer(cols=-c(PSID),names_to = "category",values_to = "value") %>%
  mutate(type_code=str_sub(category,20,20),
         time=str_split_i(category,"_",5)) %>%
  mutate(type=ifelse(type_code=="1","好きな政党を応援する人への感情温度",
              ifelse(type_code=="3","好きな政党を嫌いな人への感情温度",
              ifelse(type_code=="2","嫌いな政党を応援する人への感情温度",
              ifelse(type_code=="4","嫌いな政党を嫌いな人への感情温度",NA))))) %>% 
  mutate(type=str_c(type,"_",time)) %>% 
  dplyr::select(PSID,type,value) %>% 
  filter(!is.na(value)) %>% 
  pivot_wider(names_from=type,values_from = value) %>%
  dplyr::select(PSID,
                contains("好きな政党を応援する人への感情温度"),
                contains("好きな政党を嫌いな人への感情温度"),
                contains("嫌いな政党を応援する人への感情温度"),                
                contains("嫌いな政党を嫌いな人への感情温度"))

# 好きな政党に関する情報源(1~4点で評価)
infosource_code <- tibble(
  infosource_code = c(1, 2, 3, 4, 5, 6),
  infosource = c("テレビ", "新聞・ニュースサイト", "SNS", "動画共有サイト", "オンラインの会話", "対面での会話")) %>% 
  mutate(infosource_code=as.character(infosource_code))

infosource <- dat %>%
  select(PSID,contains("infosource")) %>%
  mutate(across(2:ncol(.), as.double)) %>% 
  pivot_longer(cols=-PSID,names_to = "category",values_to = "value") %>% 
  mutate(time=str_split_i(category,"_",4),
         infosource_code=str_split_i(category,"_",3)) %>%
  filter(!is.na(time)) %>%
  left_join(infosource_code) %>%
  filter(!is.na(value)) %>%
  dplyr::select(PSID,time,infosource,value) %>%
  mutate(category=str_c("infosource_",infosource,"_",time)) %>%
  dplyr::select(PSID,category,value) %>%
  pivot_wider(names_from = category,values_from = value)


# 常民性
# https://iap-jp.org/jssp/conf_archive/paper_download.php?s=2024-A-0001
jomin_q_type <- read_csv("jomin.csv") %>% 
  select(-jomin_question)

jomin <- dat %>%
  dplyr::select(PSID, contains("jomin")) %>% 
  mutate(across(everything(), as.character)) %>%
  pivot_longer(cols = -PSID, names_to = "category", values_to = "value") %>%
  mutate(jomin_code=str_split_i(category,"_",1),
         wave=str_split_i(category,"_",2)) %>% 
  dplyr::select(-category) %>%
  left_join(jomin_q_type) %>%
  mutate(value=as.double(value),
         point=ifelse(jomin_type=="b",value,6-value)) %>%
  group_by(PSID) %>%
  summarize(jomin_point=mean(point,na.rm=TRUE))

# 権威主義的態度
# https://www.jstage.jst.go.jp/article/soshioroji/39/2/39_125/_pdf/-char/ja
auth <- dat %>%
  dplyr::select(PSID, contains("auth")) %>% 
  mutate(across(2:ncol(.), as.double)) %>% 
  mutate(auth_point = rowMeans(select(., 2:ncol(.)), na.rm = TRUE)) %>%
  dplyr::select(PSID,auth_point)


# 社会階層意識
# 北村英哉
ladder <- dat %>%
  dplyr::select(PSID, contains("ladder")) %>% 
  mutate(across(2:ncol(.), as.double)) %>% 
  mutate(ladder_point = rowMeans(select(., 2:ncol(.)), na.rm = TRUE)) %>%
  dplyr::select(PSID,ladder_point)


# システム正当化
# https://www.jstage.jst.go.jp/article/jssp/39/2/39_2022-003/_pdf/-char/ja
sjs <- dat %>%
  dplyr::select(PSID, contains("SJS")) %>%
  mutate(across(2:ncol(.), as.double)) %>%
  pivot_longer(cols = -c(PSID), names_to = "category", values_to = "value") %>%
  filter(!str_detect(category, "DQS")) %>%
  mutate(value = ifelse(str_detect(category, "SJS3|SJS7"), 10-value, value)) %>%
  group_by(PSID) %>%
  summarize(sjs_point=mean(value,na.rm=TRUE))

# CRT　認知的熟慮性
# https://tsukuba.repo.nii.ac.jp/records/48147
crt_answers <- tibble(
  question = c("CRT1", "CRT2", "CRT3", "CRT4", "CRT5", "CRT6", "CRT7"),
  correct_answer = c("50", "5", "47","4", "29", "2000","6")
) %>% 
  mutate(correct_answer=as.double(correct_answer))

crt <- dat %>%
  dplyr::select(PSID, contains("CRT")) %>%
  mutate(across(2:ncol(.), as.double)) %>%
  filter(!is.na(CRT1)) %>%
  pivot_longer(cols=-c(PSID),names_to = "question",values_to = "answer") %>%
  left_join(crt_answers) %>%
  mutate(point=ifelse(answer==correct_answer,1,0)) %>%
  group_by(PSID) %>%
  summarize(crt_point=sum(point))

# 斎藤元彦氏、石丸氏、維新の会、トランプに関する評価
saito_etc <- dat %>%
  dplyr::select(matches("PSID|saito|ishimaru|ishin|trump")) %>%
  mutate(across(2:ncol(.), as.double)) %>%
  mutate(saito_point = rowMeans(select(., 2:5), na.rm = TRUE),
         ishimaru_point = rowMeans(select(., 6:9), na.rm = TRUE),
         ishin_point = rowMeans(select(., 10:13), na.rm = TRUE),
         trump_point = rowMeans(select(., 14:17), na.rm = TRUE)) %>%
  dplyr::select(PSID,saito_point,ishimaru_point,ishin_point,trump_point) %>%
  filter(rowSums(!is.na(select(., 2:5))) > 0)



# イデオロギー（もっとも左が0、もっとも右が10）
ideology <- dat %>%
  dplyr::select(PSID,ideology) %>% 
  mutate(ideology=as.double(ideology)) %>% 
  filter(ideology!=999)

# 政治的有効性感覚（0~3点、点が高いほど無力感）
political_efficacy <- dat %>%
  dplyr::select(PSID,contains("PoliticalEfficacy")) %>%
  mutate(across(2:ncol(.), as.double)) %>% 
  mutate(political_efficacy_point=(PoliticalEfficacy1+PoliticalEfficacy2+PoliticalEfficacy3)/3) %>% 
  dplyr::select(PSID,political_efficacy_point)




# 社会的支配指向性(1~7点)
# https://www.jstage.jst.go.jp/article/jssp/34/2/34_1725/_pdf/-char/ja
sdo <- dat %>%
  dplyr::select(PSID,contains("SDO")) %>%
  dplyr::select(-contains("add")) %>%
  pivot_longer(cols=-PSID,names_to = "category",values_to = "value") %>%
  mutate(value=as.double(value)) %>%
  mutate(sdo_num=as.double(str_replace(category,"SDO",""))) %>%
  mutate(value2=ifelse(sdo_num>=9,8-value,value)) %>%
  filter(!is.na(value)) %>%
  group_by(PSID) %>%
  summarize(sdo_point=mean(value2))

# 陰謀論的心性(0~10点)
# https://www.jstage.jst.go.jp/article/jssp/40/1/40_2023-012/_pdf/-char/ja
conspiracy <- dat %>%
  dplyr::select(PSID,contains("conspiracy")) %>%
  mutate(across(2:ncol(.), as.double)) %>% 
  pivot_longer(cols=-PSID,names_to = "category",values_to = "value") %>%
  group_by(PSID) %>%
  summarize(conspiracy_point=mean(value)) %>%
  filter(!is.na(conspiracy_point)) %>%
  dplyr::select(PSID,conspiracy_point)

# Big5
# https://simi.or.jp/outcome_indicators/education1-13
big5 <- dat %>% dplyr::select(PSID,EP1,AN2,CP3,NN4,OP5,EN6,AP7,CN8,NP9,ON10) %>%
  mutate(across(2:ncol(.), as.double)) %>%
  mutate(extraversion=EP1+8-EN6,
         agreeable=8-AN2+AP7,
         conscientious=CP3+8-CN8,
         neuroticism=NN4+8-NP9,
         open=OP5+8-ON10) %>% 
  dplyr::select(PSID,extraversion,agreeable,conscientious,neuroticism,open)

# 信頼
trust_code <- tibble(
  category = c("trust1", "trust2", "trust3", "trust4",
               "trust5", "trust6", "trust7", "trust8", "trust9"),
  subject = c("party", "congress", "government", "police",
              "legal_system", "experts", "neighbors", "relatives", "media")
) 

trust <- dat %>%
  dplyr::select(PSID,contains("trust")) %>%
  pivot_longer(cols=-PSID,names_to = "category",values_to = "value") %>%
  left_join(trust_code) %>%
  mutate(category=str_c("trust_",subject)) %>%
  dplyr::select(-subject) %>%
  pivot_wider(names_from = category,values_from = value) %>%
  mutate(across(2:ncol(.), as.double)) 

# メディア・シニシズム
# https://journals.sagepub.com/doi/10.1177/10776990211061764
cynicism <- dat %>%
  dplyr::select(PSID,contains("cynicism")) %>%
  pivot_longer(cols=-PSID,names_to = "category",values_to = "value") %>%
  mutate(value=as.double(value)) %>%
  mutate(value2=ifelse(as.double(str_sub(category,-1,-1)>=7),8-value,value)) %>%
  group_by(PSID) %>%
  summarize(cynicism_point=mean(value2))

# 各種政治的争点
# spiritualはスピリチュアル傾向の弱さ、JPfirstは外国人優遇だと感じる度合い、
# naturalityは自然より機能、効果を重視すべきと考える度合い、
# untivaccineはワクチン控えるべきと考える度合い、
# fakenewsはマスコミに誤情報が多いと思う度合い
various_issues <- dat %>% 
  dplyr::select(PSID,"self_issue1","self_issue2","self_issue3",
                "self_issue4","self_issue5","spiritual_1","JPfirst_1",
                "naturality_1","untivaccine_1","fakenews_1") %>% 
  rename(const_amend="self_issue1", # 憲法守るべき
         security_treaty="self_issue2", # 日米安保強化に慎重
         consumption_tax="self_issue3", # 消費税率維持すべき
         separate_surnames="self_issue4", # 夫婦同姓維持すべき
         foreign_worker="self_issue5") %>%  # 外国人労働者受け入れ抑制すべき
  mutate(across(2:ncol(.), ~ as.double(.)))

# 年代（回答がころころ変わる人は単純平均）
generation <- dat %>% dplyr::select(PSID,contains("age")) %>% 
  group_by(PSID) %>% 
  pivot_longer(cols=-PSID,names_to = "category",values_to = "value") %>% 
  mutate(value=as.double(value)) %>% 
  summarize(age=mean(value,na.rm=TRUE)) %>% 
  ungroup() %>% 
  mutate(generation=as.double(str_c(str_sub(age,1,1),"0")))

# 性別。回答が一貫しない人は性別不明とした
gender <- dat %>%
  select(PSID, contains("gender")) %>%
  pivot_longer(cols = -PSID, names_to = "category", values_to = "value") %>%
  filter(!is.na(value)) %>%
  group_by(PSID, value) %>%
  summarize(n = n(), .groups = "drop") %>%
  pivot_wider(names_from = value, values_from = n) %>%
  ungroup() %>%
  mutate(across(2:4, ~replace_na(., 0))) %>%
  mutate(gender = ifelse(Male > Female, "男性",
                  ifelse(Female > Male, "女性", "不明"))) %>%
  mutate(gender = ifelse(Male > 1 & Female > 1, "不明", gender)) %>%
  select(PSID, gender)


# 好きな政党に関する情報源(1~4点で評価)
infosource_code <- tibble(
  infosource_code = c(1, 2, 3, 4, 5, 6),
  infosource = c("テレビ", "新聞・ニュースサイト", "SNS", "動画共有サイト", "オンラインの会話", "対面での会話")) %>% 
  mutate(infosource_code=as.character(infosource_code))

# 選挙に関する情報源
infoelection <- dat %>%
  select(PSID,contains("infoelection")) %>%
  mutate(across(2:ncol(.), as.double)) %>% 
  pivot_longer(cols=-PSID,names_to = "category",values_to = "value") %>% 
  mutate(time=str_split_i(category,"_",3),
         infosource_code=str_split_i(category,"_",2)) %>%
  filter(!is.na(time)) %>%
  left_join(infosource_code) %>%
  filter(!is.na(value)) %>%
  dplyr::select(PSID,time,infosource,value) %>%
  mutate(category=str_c("infoelection_",infosource,"_",time)) %>%
  dplyr::select(PSID,category,value) %>%
  pivot_wider(names_from = category,values_from = value)



analysis <- dat %>% 
  distinct(PSID) %>% 
  left_join(attitude_toward_parties) %>%
  left_join(party_support) %>% 
  left_join(party_to_vote) %>%
  left_join(thermo_party) %>%
  left_join(thermo_person) %>% 
  left_join(jomin) %>%
  left_join(crt) %>%
  left_join(auth) %>% 
  left_join(sjs) %>% 
  left_join(saito_etc) %>% 
  left_join(ladder) %>% 
  left_join(ideology) %>% 
  left_join(political_efficacy) %>% 
  left_join(infosource) %>% 
  left_join(sdo) %>% 
  left_join(conspiracy) %>% 
  left_join(big5) %>% 
  left_join(trust) %>% 
  left_join(cynicism) %>% 
  left_join(various_issues) %>% 
  left_join(generation) %>% 
  left_join(gender) %>% 
  left_join(infoelection)

```

analysisというオブジェクトに分析結果がすべて入りました。各列の列名の末尾についている数字は「いつ行われた調査か」を示しています。

以上。
