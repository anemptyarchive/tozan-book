# ch5-4 Googleスプレッドシートの操作 -----------------------------------------------------

# スプレッドシートの読み込みと書き出し -------------------------------------------------------------------

# 利用するパッケージ
library(googledrive)
library(googlesheets4)
library(palmerpenguins)


### ・シートのアップロード -----

# データセットを取得
penguins_df <- palmerpenguins::penguins


# データフレームをローカルに書き出し
readr::write_csv(
  x = penguins_df, 
  file = "data/chapter5/penguins.csv", 
  na = ""
)

# スプレッドシートとしてアップロード
googledrive::drive_upload(
  media = "data/chapter5/penguins.csv", 
  path = "blog/tozan_book/chapter5/", 
  name = "penguins", 
  type = "spreadsheet", 
  overwrite = TRUE
)


# スプレッドシートとしてアップロード
metadata_sheets_id <- googlesheets4::gs4_create(
  name = "penguins", 
  sheets = list(Sheet1 = penguins_df)
)


# ファイルを移動
googledrive::drive_mv(
  file = metadata_sheets_id, 
  path = "blog/tozan_book/chapter5/", 
  overwrite = TRUE
)


### ・メタデータの取得 -----

# ファイルのメタデータを取得
metadata_dribble <- googledrive::drive_get(path = "blog/tozan_book/chapter5/penguins")

# シートIDからファイルのメタデータを取得
metadata_dribble <- googledrive::as_dribble(metadata_sheets_id)


# ファイルのメタデータからファイルIDを抽出
metadata_drive_id <- metadata_dribble[["id"]]
metadata_drive_id <- googledrive::as_id(metadata_dribble)

# ファイルのメタデータからシートIDを抽出
metadata_sheets_id <- googlesheets4::as_sheets_id(metadata_dribble)


# スプレッドシートのメタデータを取得
metadata_gs4_list <- googlesheets4::gs4_get(ss = metadata_dribble)
metadata_gs4_list <- googlesheets4::gs4_get(ss = metadata_drive_id)
metadata_gs4_list <- googlesheets4::gs4_get(ss = metadata_sheets_id)


# シートのメタデータからファイルIDを抽出
metadata_drive_id <- googledrive::as_id(metadata_gs4_list)

# シートのメタデータからシートIDを抽出
metadata_sheets_id <- metadata_gs4_list[["spreadsheet_id"]]
metadata_sheets_id <- googlesheets4::as_sheets_id(metadata_gs4_list)


# シートIDからファイルIDを取得
metadata_drive_id <- googledrive::as_id(metadata_sheets_id)

# ファイルIDからシートIDを取得
metadata_sheets_id <- googlesheets4::as_sheets_id(metadata_drive_id)


# シートのメタデータからシート名を抽出
metadata_gs4_list[["sheets"]][["name"]]


### ・シートの読み込み -----

# シートIDを取得
metadata_sheets_id <- googledrive::drive_get(path = "blog/tozan_book/chapter5/penguins") %>% 
  googlesheets4::as_sheets_id()

# データを取得
df <- googlesheets4::read_sheet(
  ss = metadata_sheets_id, 
  sheet = "Sheet1", 
  col_types = "ccddiici", 
  na = c("", "NA")
) %>% 
  # 文字列NAを欠損値NAに変換
  dplyr::mutate(
    species = factor(species), 
    island = factor(island), 
    sex = factor(sex)
  )


### ・メモ -----

# データフレームをローカルに書き出し
readr::write_csv(
  x = penguins_df, 
  file = "data/chapter5/tmp_penguins.csv"
)

# スプレッドシートとしてアップロード
googledrive::drive_put(
  media = "data/chapter5/tmp_penguins.csv", 
  path = "blog/tozan_book/chapter5/", 
  name = "tmp_penguins", 
  type = "spreadsheet", 
)

# シートIDを取得
tmp_metadata_sheets_id <- googledrive::drive_get(path = "blog/tozan_book/chapter5/tmp_penguins") %>% 
  googlesheets4::as_sheets_id()

# デフォルトの設定でデータを取得
df_default <- googlesheets4::read_sheet(ss = tmp_metadata_sheets_id)

# データフレームを調整
df2 <- df_default %>% 
  dplyr::mutate(
    species = dplyr::if_else(species == "NA", true = NA_character_, false = species) %>% 
      factor(), 
    island = dplyr::if_else(island == "NA", true = NA_character_, false = island) %>% 
      factor(), 
    bill_length_mm = unlist(bill_length_mm) %>% 
      as.numeric(), 
    bill_depth_mm = unlist(bill_depth_mm) %>% 
      as.numeric(), 
    flipper_length_mm = unlist(flipper_length_mm) %>% 
      as.integer(), 
    body_mass_g = unlist(body_mass_g) %>% 
      as.integer(), 
    year = as.integer(year), 
    sex = dplyr::if_else(sex == "NA", true = NA_character_, false = sex) %>% 
      factor()
  )


### ・シートの書き出し -----

# NAを含む行を削除
df_drop_na <- tidyr::drop_na(df)  

# シートを追加
googlesheets4::write_sheet(
  data = df_drop_na, 
  ss = metadata_sheets_id, 
  sheet = "drop_na"
)

# シートのメタデータを取得
metadata_gs4_list <- googlesheets4::gs4_get(ss = metadata_sheets_id)


