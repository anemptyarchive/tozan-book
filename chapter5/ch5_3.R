# ch5-3 Googleドライブの操作 -----------------------------------------------------

# ファイルの保存 -------------------------------------------------------------------

# 利用するパッケージ
library(googledrive)
library(palmerpenguins)
library(ggplot2)


### ファイルの作成 -----

# データセットを取得
penguins_df <- palmerpenguins::penguins

# データフレームを書き出し
readr::write_csv(penguins_df, file = "data/chapter5/sample.csv")


# 散布図を作成
graph <- ggplot(data = penguins_df, mapping = aes(x = body_mass_g, y = bill_length_mm)) + 
  geom_point(mapping = aes(color = species, shape = species), size = 3) + 
  labs(title = "palmerpenguins")

# 画像を書き出し
ggplot2::ggsave(
  filename = "data/chapter5/sample.png", 
  plot = graph, 
  dpi = 100, width = 8, height = 6
)


### ・drive_upload -----

# パスを用いてファイルをアップロード
drive_upload(
  media = "data/chapter5/sample.png", 
  path = "blog/tozan_book/chapter5/sample.png", 
  overwrite = TRUE
)

drive_upload(
  media = "data/chapter5/sample.png", 
  path = "blog/tozan_book/chapter5/", 
  name = "sample.png", 
  overwrite = TRUE
)

drive_upload(
  media = "data/chapter5/sample.png", 
  path = "blog/tozan_book/chapter5/", 
  overwrite = TRUE
)


# フォルダのメタデータを取得
metadata <- drive_get(path = "blog/tozan_book/chapter5/")
metadata <- drive_get(id = metadata[["id"]])

# ファイルIDを用いてファイルをアップロード
drive_upload(
  media = "data/chapter5/sample.png", 
  path = metadata[["id"]], 
  overwrite = TRUE
)

drive_upload(
  media = "data/chapter5/sample.png", 
  path = as_id(metadata), 
  overwrite = TRUE
)

drive_upload(
  media = "data/chapter5/sample.png", 
  path = metadata, 
  overwrite = TRUE
)


# ファイルIDを確認
drive_get(path = "blog/tozan_book/chapter5/sample.png") %>% 
  as_id()


# データをアップロード
drive_upload(
  media = "data/chapter5/sample.csv", 
  path = "blog/tozan_book/chapter5/", 
  name = "sample.csv", 
  overwrite = TRUE
)


### ・drive_update -----

# パスを用いてファイルを更新
drive_update(
  file = "blog/tozan_book/chapter5/sample.png", 
  media = "data/chapter5/sample.png"
)


# ファイルのメタデータを取得
metadata <- drive_get(path = "blog/tozan_book/chapter5/sample.png")

# メタデータを用いてファイルを更新
drive_update(
  file = metadata[["id"]], 
  media = "data/chapter5/sample.png"
)

drive_update(
  file = as_id(metadata), 
  media = "data/chapter5/sample.png"
)

drive_update(
  file = metadata, 
  media = "data/chapter5/sample.png"
)


# ファイルIDを確認
drive_get(path = "blog/tozan_book/chapter5/sample.png") %>% 
  as_id()


### ・drive_put -----

# ファイルパスを用いてファイルをアップロードor更新
drive_put(
  media = "data/chapter5/sample.png", 
  path = "blog/tozan_book/chapter5/sample.png"
)

drive_put(
  media = "data/chapter5/sample.png", 
  path = "blog/tozan_book/chapter5/", 
  name = "sample.png"
)

drive_put(
  media = "data/chapter5/sample.png", 
  path = "blog/tozan_book/chapter5/"
)


# フォルダのメタデータを取得
metadata <- drive_get(path = "blog/tozan_book/chapter5/")

# メタデータを用いてファイルをアップロードor更新
drive_put(
  media = "data/chapter5/sample.png", 
  path = metadata[["id"]], 
  name = "sample.png"
)

drive_put(
  media = "data/chapter5/sample.png", 
  path = as_id(metadata), 
  name = "sample.png"
)

drive_put(
  media = "data/chapter5/sample.png", 
  path = metadata, 
  name = "sample.png"
)


# ファイルIDを確認
drive_get(path = "blog/tozan_book/chapter5/sample.png") %>% 
  as_id()



