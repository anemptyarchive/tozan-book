
# ch6-2 スクレイピングによるデータ収集 ---------------------------------------------------


# 利用する関数の実装 ---------------------------------------------------------------

# 一時停止のカウントダウンバー
sleep_bar <- function(s = 10) {
  # プログレスバーを表示
  message("\r", "[", rep("#", times = s), "] ", s, "s", appendLF = FALSE)
  
  for(i in 1:s) {
    # 処理を一時停止
    Sys.sleep(1)
    
    # 前回のメッセージを初期化
    message("\r", rep(" ", s + 10), appendLF = FALSE)
    
    # プログレスバーを表示
    message(
      "\r", "[", rep("#", times = s - i), rep(" ", times = i), "] ", s - i, "s", 
      appendLF = FALSE
    )
  }
  
  # 改行
  message("\r")
}

# お試し
sleep_bar()

# お試し
for(i in 1:2) {
  sleep_bar(3)
  print("next")
}


# 記事URLのスクレイピング ----------------------------------------------------------------------

# 利用パッケージ
library(rvest)
library(polite)
library(magrittr)


# ブログのURLを指定
blog_url <- "https://www.anarchive-beta.com/"

# スクレイピングルールの確認
session <- polite::bow(url = blog_url)

# 期間(年)を指定
year_from <- 2022
year_to   <- 2022

# 最大ページ数を指定:(念のため)
max_page <- 10

# 年ごとに記事URLを収集
url_vec <- c() # 受け皿を作成
for(year in year_from:year_to) {
  
  # 一覧ページを切り替え
  url_year_vec <- c() # 初期化
  for(page in 1:max_page) {
    
    # 一覧ページのURLを作成
    target_url <- paste0(blog_url, "archive/", year, "?page=", page)
    print(target_url)
    
    # 一覧ページのHTMLを取得
    session <- polite::nod(bow = session, path = target_url) # politeパッケージの場合
    target_html <- try(
      #rvest::read_html(target_url), # rvestパッケージの場合
      polite::scrape(bow = session), # politeパッケージの場合
      silent = TRUE
    )
    
    # 記事が無ければ次の年に進む
    if(inherits(target_html, what = "try-error")) break
    
    # 記事のURLを取得
    url_page_vec <- target_html %>% 
      rvest::html_elements(".entry-title") %>% # 記事タイトル
      rvest::html_elements("a") %>% # 記事リンク
      rvest::html_attr("href") # 記事URL
    
    # 同じ年のURLを結合
    url_year_vec <- c(rev(url_page_vec), url_year_vec)
    
    # 処理を一時停止
    #Sys.sleep(10)
    sleep_bar(10)
  }
  
  # 全ての年のURLを結合
  url_vec <- c(url_vec, url_year_vec)
}

# URLの数を確認
length(url_vec)

# URLデータを書き出し
saveRDS(object = url_vec, file = "data/ch6_2/url.rds")


# 投稿日の可視化 ----------------------------------------------------------------

# 利用パッケージ
library(tidyverse)
library(lubridate)

# チェック用
library(magrittr)
library(ggplot2)


### ・データの準備 -----

# URLデータを読み込み
url_vec <- readRDS(file = "data/ch6_2/url.rds")

# ブログのURLを指定
blog_url <- "https://www.anarchive-beta.com/"


### ・期間の指定 -----

# 期間(年月日)を指定
date_from <- "2018-12-01"
date_to   <- "2022-04-15"
date_to   <- lubridate::today() # 現在の日付


### ・日別に作図 -----

# 期間内の日付情報を作成
base_df <- seq(
  from = lubridate::as_date(date_from), 
  to = lubridate::as_date(date_to),
  by = "day"
) %>% # 日付ベクトルを作成
  tibble::tibble(date = .) # データフレームに変換

# 記事URLを投稿日に変換
date_vec <- url_vec %>% 
  stringr::str_remove(pattern = paste0(blog_url, "entry/")) %>% # 日時を示す文字列を抽出
  lubridate::as_datetime(tz = "Asia/Tokyo") %>% # POSIXt型に変換
  lubridate::as_date() %>% # Date型に変換
  sort() # 昇順に並び替え

# 日ごとに投稿数を集計
date_df <- tibble::tibble(date = date_vec) %>% 
  dplyr::count(date, name = "post") # 投稿数をカウント

# 作図用のデータフレームを作成
post_df <- date_df %>% 
  dplyr::right_join(base_df, by = "date") %>% # 日付情報に統合
  dplyr::mutate(
    post = tidyr::replace_na(post, replace = 0), # 投稿なしを0に置換
    year_month = format(date, "%Y-%m"), # 年月を抽出
    day = format(date, "%d") # 日を抽出
  )

# ヒートマップを作成
graph <- ggplot(post_df, aes(x = year_month, y = day, fill = post)) + 
  geom_tile() + # ヒートマップ
  geom_text(mapping = aes(label = post), color = "white") + # 記事数ラベル
  scale_fill_gradient(low = "white", high = "hotpink") + # タイルの色
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) + # 軸目盛ラベル
  labs(title = paste0(blog_url, "の記事投稿数"), 
       subtitle = paste("総記事数：", sum(post_df[["post"]])), 
       x = "年-月", y = "日", 
       fill = "記事数")
graph

# グラフを保存
ggplot2::ggsave(
  filename = "figure/ch6_2/day_heatmap.png", plot = graph, 
  dpi = 100, width = 12, height = 9
)


### ・月別に作図 -----

# 期間内の月情報を作成
base_df <- seq(
  from = date_from %>% 
    lubridate::as_date() %>% 
    lubridate::floor_date(unit = "mon"), 
  to = date_to %>% 
    lubridate::as_date() %>% 
    lubridate::floor_date(unit = "mon"), 
  by = "mon"
) %>% # 月ベクトルを作成
  tibble::tibble(date = .) # データフレームに変換

# 記事URLを投稿月に変換
date_vec <- url_vec %>% 
  stringr::str_remove(pattern = paste0(blog_url, "entry/")) %>% # 日時を示す文字列を抽出
  lubridate::as_datetime(tz = "Asia/Tokyo") %>% # POSIXt型に変換
  lubridate::as_date() %>% # Date型に変換
  lubridate::floor_date(unit = "mon") %>% # 月単位に切り捨て
  sort() # 昇順に並び替え

# 月ごとに投稿数を集計
date_df <- tibble::tibble(date = date_vec) %>% 
  dplyr::count(date, name = "post") # 投稿数をカウント

# 作図用のデータフレームを作成
post_df <- date_df %>% 
  dplyr::right_join(base_df, by = "date") %>% # 月情報に統合
  dplyr::mutate(
    post = tidyr::replace_na(post, replace = 0), # 投稿なしを0に置換
    year = format(date, "%Y"), # 年を抽出
    month = format(date, "%m") # 月を抽出
  )

# ヒートマップを作成
graph <- ggplot(post_df, aes(x = year, y = month, fill = post)) + 
  geom_tile() + # ヒートマップ
  geom_text(mapping = aes(label = post), color = "white") + # 記事数ラベル
  scale_fill_gradient(low = "white", high = "hotpink") + # タイルの色
  labs(title = paste0(blog_url, "の記事投稿数"), 
       subtitle = paste("総記事数：", sum(post_df[["post"]])), 
       x = "年", y = "月", 
       fill = "記事数")
graph

# グラフを保存
ggplot2::ggsave(
  filename = "figure/ch6_2/month_heatmap.png", plot = graph, 
  dpi = 100, width = 9, height = 9
)


### ・時別に作図 -----

# 期間内の時刻情報を作成
base_df <- tibble::tibble(hour = 0:23)

# 記事URLを投稿日時に変換
datetime_vec <- url_vec %>% 
  stringr::str_remove(pattern = paste0(blog_url, "entry/")) %>% # 日時を示す文字列を抽出
  lubridate::as_datetime(tz = "Asia/Tokyo") %>% # POSIXt型に変換
  sort() # 昇順に並び替え

# 時刻ごとに投稿数を集計
hour_df <- tibble::tibble(
  hour = lubridate::hour(datetime_vec) # 時刻を抽出
) %>% # データフレームに変換
  dplyr::count(hour, name = "post") # 投稿数をカウント

# 作図用のデータフレームを作成
post_df <- hour_df %>% 
  dplyr::right_join(base_df, by = "hour") %>% # 時刻情報に統合
  dplyr::mutate(post = tidyr::replace_na(post, replace = 0)) %>% # 投稿なしを0に置換
  dplyr::arrange(hour)

# ヒートマップを作成
graph <- ggplot(post_df, aes(x = 1, y = hour, fill = post)) + 
  geom_tile() + # ヒートマップ
  geom_text(mapping = aes(label = post), color = "white") + # 記事数ラベル
  scale_fill_gradient(low = "white", high = "hotpink") + # タイルの色
  scale_x_continuous(breaks = NULL) + # x軸目盛
  scale_y_continuous(breaks = 0:23, minor_breaks = FALSE) + # y軸目盛
  #theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) + # 軸目盛ラベル
  labs(title = paste0(blog_url, "の記事投稿数"), 
       subtitle = paste("総記事数：", sum(post_df[["post"]])), 
       x = "", y = "時", 
       fill = "記事数")
graph

# グラフを保存
ggplot2::ggsave(
  filename = "figure/ch6_2/hour_heatmap.png", plot = graph, 
  dpi = 100, width = 6, height = 9
)


# 棒グラフを作成
graph <- ggplot(post_df, aes(x = hour, y = post)) + 
  geom_bar(stat = "identity", fill = "hotpink", color = "white") + # ヒートマップ
  geom_text(mapping = aes(label = post), color = "hotpink", vjust = -0.5) + # 記事数ラベル
  scale_x_continuous(breaks = 0:23, minor_breaks = FALSE) + # y軸目盛
  labs(title = paste0(blog_url, "の記事投稿数"), 
       subtitle = paste("総記事数：", sum(post_df[["post"]])), 
       x = "時", y = "記事数")
graph

# グラフを保存
ggplot2::ggsave(
  filename = "figure/ch6_2/hour_bar.png", plot = graph, 
  dpi = 100, width = 12, height = 9
)


### ・時・分別に作図 -----

# 記事URLを投稿日時に変換
datetime_vec <- url_vec %>% 
  stringr::str_remove(pattern = paste0(blog_url, "entry/")) %>% # 日時を示す文字列を抽出
  lubridate::as_datetime(tz = "Asia/Tokyo") %>% # POSIXt型に変換
  lubridate::floor_date(unit = "10minutes") %>% # 10分刻みに切り捨て
  sort() # 昇順に並び替え

# 時・分ごとに投稿数を集計
datetime_df <- tibble::tibble(
  hour = lubridate::hour(datetime_vec), # 時を抽出
  minute = lubridate::minute(datetime_vec) # 分を抽出
) %>% 
  dplyr::count(hour, minute, name = "post") # 投稿数をカウント

# 時・分情報を作成
base_df <- tibble::tibble(
  hour = rep(0:23, each = 6), 
  minute = rep(0:5*10, times = 24)
)

# 作図用のデータフレームを作成
post_df <- datetime_df %>% 
  dplyr::right_join(base_df, by = c("hour", "minute")) %>% # 時・分情報に統合
  dplyr::mutate(post = tidyr::replace_na(post, replace = 0)) %>% # 投稿なしを0に置換
  dplyr::arrange(hour, minute) # 昇順に並び替え

# ヒートマップを作成
graph <- ggplot(post_df, aes(x = hour, y = minute, fill = post)) + 
  geom_tile() + # ヒートマップ
  geom_text(mapping = aes(label = post), color = "white") + # 記事数ラベル
  scale_fill_gradient(low = "white", high = "hotpink") + # タイルの色
  scale_x_continuous(breaks = 0:23, minor_breaks = FALSE) + # x軸目盛
  scale_y_continuous(breaks = 0:5*10, minor_breaks = FALSE) + # y軸目盛
  labs(title = paste0(blog_url, "の記事投稿数"), 
       subtitle = paste("総記事数：", sum(post_df[["post"]])), 
       x = "時", y = "分", 
       fill = "記事数")
graph

# グラフを保存
ggplot2::ggsave(
  filename = "figure/ch6_2/hourminute_heatmap.png", plot = graph, 
  dpi = 100, width = 12, height = 6
)


# 正確な投稿時間のスクレイピング ---------------------------------------------------------

# 利用パッケージ
library(rvest)
library(magrittr)


# URLデータを読み込み
url_vec <- readRDS(file = "data/ch6_2/url.rds")

# 記事番号を指定
i <- 1

# 記事URLを取り出し
entry_url <- url_vec[i]

# 記事HTMLを取得
entry_html <- rvest::read_html(entry_url)

# 記事テキストを抽出
entry_datetime <- entry_html %>% 
  #rvest::html_elements("header") %>% # ヘッダー
  rvest::html_elements(".entry-date") %>% # 記事の日時データ
  rvest::html_elements("time") %>% # 投稿日時
  rvest::html_attr("datetime") %>% # 日時データ
  lubridate::as_datetime(tz = "UTC") %>% # POLITXt型に変換
  lubridate::as_datetime(tz = "Asia/Tokyo") # 日本標準時に変換
entry_datetime


# 記事テキストのスクレイピング ---------------------------------------------------------------

# 利用パッケージ
library(rvest)
library(polite)
library(magrittr)


# URLデータを読み込み
url_vec <- readRDS(file = "data/ch6_2/url.rds")

# スクレイピングルールの確認
session <- polite::bow(url = url_vec[1])

# 期間(年)を指定
year_from <- 2022
year_to   <- 2022

# 年を切り替え
for(year in year_from:year_to) {
  
  # 月ごとに記事テキストをスクレイピングして書き出し
  for(month in 1:12) {
    
    # 年月の文字列を作成
    year_month <- paste0(year, "/", stringr::str_pad(month, width = 2, pad = 0))
    print(paste0("----- ", year_month, " -----"))
    
    # 期間内の記事URLのインデックスを抽出
    target_idx <- stringr::str_which(url_vec, pattern = year_month)
    print(paste0("posts : ", length(target_idx)))
    
    # 記事が無ければ次の月に進む
    if(length(target_idx) == 0) next
    
    # 記事ごとにテキストをスクレイピング
    entry_text_vec <- c()
    for(idx in target_idx) {
      
      # 記事URLを取り出し
      entry_url <- url_vec[idx]
      print(entry_url)
      
      # 記事HTMLを取得
      #entry_html <- rvest::read_html(entry_url) # rvestパッケージの場合
      session <- polite::nod(bow = session, path = entry_url)
      entry_html <- polite::scrape(bow = session) # politeパッケージの場合
      
      # 記事テキストを抽出
      entry_text <- entry_html %>% 
        rvest::html_elements(".entry-content") %>% # 記事の内容
        rvest::html_text() # テキストを取得
      
      # 同じ月のテキストを結合
      entry_text_vec <- c(entry_text_vec, entry_text)
      
      # 処理を一時停止
      #Sys.sleep(20)
      sleep_bar(20)
    }
    
    # 書き出し用のパスを作成
    file_path <- paste0("data/ch6_2/entry_text/", stringr::str_replace(year_month, pattern = "/", replacement = "_"), ".txt")
    
    # テキストを書き出し
    write(paste0(entry_text_vec, collapse = "\n"), file = file_path)
  }
}


# 頻度上位語の可視化：棒グラフ ----------------------------------------------------------------

# 利用パッケージ
library(RMeCab)
library(tidyverse)

# チェック用
library(magrittr)
library(ggplot2)


### ・形態素解析 -----

# 年月を指定
year  <- 2022
month <- 3

# テキストのファイルパスを作成
file_name <- paste0(year, "_", stringr::str_pad(month, width = 2, pad = 0), ".txt")
file_path <- paste0("data/ch6_2/entry_text/", file_name)

# MeCabによる形態素解析
mecab_df <- RMeCab::docDF(target = file_path, type = 1) %>% 
  tibble::as_tibble()
mecab_df


### ・単語の集計 -----

# 単語数を指定
term_size <- 100

# 利用する品詞を指定
pos1_vec <- c("名詞", "動詞", "形容詞")
pos2_vec <- c("一般", "固有名詞", "サ変接続", "形容動詞語幹", "ナイ形容詞語幹", "自立")

# 削除する単語を指定
stopword_symbol_vec <- c("\\(", "\\)", "\\{", "\\}", "\\[", "]", "「", "」", ",", "_", "--", "!", "#", "\\.", "\\$", "\\\\")
stopword_term_vec <- c("る", "ある", "する", "せる", "できる", "なる", "やる", "れる", "いい", "ない")

# 頻出語を抽出
freq_df <- mecab_df %>% 
  dplyr::filter(POS1 %in% pos1_vec) %>% # 指定した品詞大分類を抽出
  dplyr::filter(POS2 %in% pos2_vec) %>% # 指定した品詞小分類を抽出
  dplyr::filter(!stringr::str_detect(TERM, pattern = paste0(stopword_symbol_vec, collapse = "|"))) %>% # 指定した記号を削除
  dplyr::filter(!stringr::str_detect(TERM, pattern = paste0(stopword_term_vec, collapse = "|"))) %>% # 指定した単語を削除
  dplyr::select(term = TERM, frequency = file_name) %>% # 単語と頻度の列を取り出し
  dplyr::group_by(term) %>% # 単語でグループ化
  dplyr::summarise(frequency = sum(frequency), .groups = "drop") %>% # 同一単語の頻度を合計
  dplyr::arrange(dplyr::desc(frequency)) %>% # 降順に並び替え
  head(n = term_size) # 頻出語を抽出
freq_df[["term"]]


### ・棒グラフの作成 -----

# 棒グラフを作成
graph <- ggplot(freq_df, aes(x = reorder(term, frequency), y = frequency)) + 
  geom_bar(stat = "identity", fill = "hotpink", color = "white") + # 棒グラフ
  coord_flip(expand = FALSE) + # 軸の入れ替え
  theme(axis.text.y = element_text(size = 15)) + # x軸目盛ラベル
  labs(title = paste0(year, "年", month, "月の頻出語"), 
       x = "単語", y = "頻度")
graph

# グラフを保存
ggplot2::ggsave(
  filename = paste0("figure/ch6_2/term_", year, "_", month, ".png"), plot = graph, 
  dpi = 100, width = 9, height = 18
)


# 頻度上位語の可視化：バーチャートレース ---------------------------------------------------------

# 利用するパッケージ
library(RMeCab)
library(tidyverse)
library(gganimate)

# チェック用
library(magrittr)
library(ggplot2)


### ・形態素解析 -----

# フォルダパスを指定
dir_path <- "data/ch6_2/entry_text"

# MeCabによる形態素解析
mecab_df <- RMeCab::docDF(target = dir_path, type = 1) %>% 
  tibble::as_tibble()

# 形態素解析データを書き出し
#saveRDS(object = mecab_df, file = "data/ch6_2/mecab.rds")

# 形態素解析データを読み込み
mecab_df <- readRDS(file = "data/ch6_2/mecab.rds")


### ・単語の集計 -----

# 単語数を指定
max_rank <- 100

# 利用する品詞を指定
pos1_vec <- c("名詞", "動詞", "形容詞")
pos2_vec <- c("一般", "固有名詞", "サ変接続", "形容動詞語幹", "ナイ形容詞語幹", "自立")

# 削除する単語を指定
stopword_symbol_vec <- c("\\(", "\\)", "\\{", "\\}", "\\[", "]", "「", "」", ",", "_", "--", "!", "#", "\\.", "\\$", "\\\\")
stopword_term_vec <- c("る", "ある", "する", "せる", "できる", "なる", "やる", "れる", "いい", "ない")

# 頻出語を抽出
rank_df <- mecab_df %>% 
  dplyr::filter(POS1 %in% pos1_vec) %>% # 指定した品詞大分類を抽出
  dplyr::filter(POS2 %in% pos2_vec) %>% # 指定した品詞小分類を抽出
  dplyr::filter(!stringr::str_detect(TERM, pattern = paste0(stopword_symbol_vec, collapse = "|"))) %>% # 不要な記号を削除
  dplyr::filter(!stringr::str_detect(TERM, pattern = paste0(stopword_term_vec, collapse = "|"))) %>% # 不要な単語を削除
  dplyr::select(term = TERM, !c("POS1", "POS2")) %>% # 単語と頻度の列を取り出し
  tidyr::pivot_longer(cols = !term, names_to = "date", values_to = "frequency") %>% # 頻度列をまとめる
  dplyr::mutate(
    date = date %>% 
      stringr::str_remove(pattern = ".txt") %>% 
      stringr::str_replace(pattern = "_", replacement = "-") %>% 
      stringr::str_c("-01") %>% 
      lubridate::as_date()
  ) %>% # 日付情報に変換
  dplyr::group_by(term, date) %>% # 単語と月でグループ化
  dplyr::summarise(frequency = sum(frequency), .groups = "drop") %>% # 頻度を合計
  dplyr::arrange(date, frequency) %>% # 昇順に並び替え
  dplyr::group_by(date) %>% # 月でグループ化
  dplyr::mutate(ranking = dplyr::row_number(-frequency)) %>% # 月ごとにランク付け
  dplyr::ungroup() %>% # グループ化を解除
  dplyr::filter(ranking <= max_rank) %>% # 頻度上位単語を抽出
  dplyr::arrange(date, ranking) # 昇順に並び替え
rank_df


### ・バーチャートレースの作成 -----

# フレーム数を取得
n <- length(unique(rank_df[["date"]]))

# 遷移フレーム数を指定
t <- 8

# 停止フレーム数を指定
s <- 2

# バーチャートレースを作成:(y軸固定)
anim <- ggplot(rank_df, aes(x = ranking, y = frequency, fill = term, color = term)) + 
  geom_bar(stat = "identity", width = 0.9, alpha = 0.8) + # 棒グラフ
  geom_text(aes(y = 0, label = paste(term, " ")), hjust = 1) + # 単語ラベル
  geom_text(aes(label = paste(" ", frequency)), hjust = 0) + # 頻度ラベル
  gganimate::transition_states(states = date, transition_length = t, state_length = s, wrap = FALSE) + # フレーム
  gganimate::ease_aes("cubic-in-out") + # アニメーションの緩急
  coord_flip(clip = "off", expand = FALSE) + # 軸の入替
  scale_x_reverse() + # x軸の反転
  theme(
    axis.title.y = element_blank(), # 縦軸のラベル
    axis.text.y = element_blank(), # 縦軸の目盛ラベル
    axis.ticks.y = element_blank(), # 縦軸の目盛指示線
    #panel.grid.major.x = element_line(color = "grey", size = 0.1), # 横軸の主目盛線
    panel.grid.major.y = element_blank(), # 縦軸の主目盛線
    panel.grid.minor.y = element_blank(), # 縦軸の補助目盛線
    panel.border = element_blank(), # グラフ領域の枠線
    #panel.background = element_blank(), # グラフ領域の背景
    plot.title = element_text(color = "black", face = "bold", size = 20, hjust = 0.5), # 全体のタイトル
    plot.subtitle = element_text(color = "black", size = 15, hjust = 0.5), # 全体のサブタイトル
    plot.margin = margin(t = 10, r = 50, b = 10, l = 100, unit = "pt"), # 全体の余白
    legend.position = "none" # 凡例の表示位置
  ) + # 図の体裁
  labs(
    title = "https://www.anarchive-beta.com/の頻度上位語", 
    subtitle = "{lubridate::year(closest_state)}年{lubridate::month(closest_state)}月", 
    y = "頻度"
  ) # ラベル


# バーチャートレースを作成:(y軸可変)
anim <- ggplot(rank_df, aes(x = ranking, y = frequency, fill = term, color = term)) + 
  geom_bar(stat = "identity", width = 0.9, alpha = 0.8) + # 棒グラフ
  geom_text(aes(y = 0, label = paste(term, " ")), hjust = 1) + # 単語ラベル
  geom_text(aes(label = paste(" ", frequency, "回")), hjust = 0) + # 頻度ラベル
  gganimate::transition_states(states = date, transition_length = t, state_length = s, wrap = FALSE) + # フレーム
  gganimate::ease_aes("cubic-in-out") + # アニメーションの緩急
  coord_flip(clip = "off", expand = FALSE) + # 軸の入替
  scale_x_reverse() + # x軸の反転
  gganimate::view_follow(fixed_x = TRUE) + # フレームごとに表示範囲を調整
  theme(
    axis.title.x = element_blank(), # 横軸のラベル
    axis.title.y = element_blank(), # 縦軸のラベル
    axis.text.x = element_blank(), # 横軸の目盛ラベル
    axis.text.y = element_blank(), # 縦軸の目盛ラベル
    axis.ticks.x = element_blank(), # 横軸の目盛指示線
    axis.ticks.y = element_blank(), # 縦軸の目盛指示線
    #panel.grid.major.x = element_line(color = "grey", size = 0.1), # 横軸の主目盛線
    panel.grid.major.y = element_blank(), # 縦軸の主目盛線
    panel.grid.minor.x = element_blank(), # 横軸の補助目盛線
    panel.grid.minor.y = element_blank(), # 縦軸の補助目盛線
    panel.border = element_blank(), # グラフ領域の枠線
    #panel.background = element_blank(), # グラフ領域の背景
    plot.title = element_text(color = "black", face = "bold", size = 20, hjust = 0.5), # 全体のタイトル
    plot.subtitle = element_text(color = "black", size = 15, hjust = 0.5), # 全体のサブタイトル
    plot.margin = margin(t = 10, r = 50, b = 10, l = 100, unit = "pt"), # 全体の余白
    legend.position = "none" # 凡例の表示位置
  ) + # 図の体裁
  labs(
    title = "https://www.anarchive-beta.com/の頻度上位語", 
    subtitle = "{lubridate::year(closest_state)}年{lubridate::month(closest_state)}月"
  ) # ラベル


# gif画像を作成
g <- gganimate::animate(
  plot = anim, nframes = n*(t+s), fps = t+s, width = 600, height = 800
)
g

# gif画像を保存
gganimate::anim_save(filename = "figure/ch6_2/BarChartRace_fixed.gif", animation = g)


# 動画を作成と保存
m <- gganimate::animate(
  plot = anim, nframes = n*(t+s), fps = t+s, width = 900, height = 1200, 
  renderer = gganimate::av_renderer(file = "figure/ch6_2/BarChartRace_fixed.mp4")
)


warnings()
