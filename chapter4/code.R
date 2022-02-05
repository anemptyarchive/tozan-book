
# 2次元ガウス分布のグラフの作成 ---------------------------------------------------------

# パッケージの読み込み
library(tidyverse)
library(mvnfast)


# 格子点の作成 ------------------------------------------------------------------

# x軸の値を作成
x_vals <- seq(from = 1, to = 5, by = 1)

# y軸の値を作成
y_vals <- seq(from = 11, to = 15, by = 1)


# 値をデータフレームに格納
x_vals_df <- tibble::tibble(
  x = x_vals, # x軸の値
  y = y_vals  # y軸の値
)

# 散布図を作成
ggplot() + 
  geom_point(data = x_vals_df, mapping = aes(x = x, y = y)) + # 散布図
  scale_x_continuous(breaks = x_vals) + # x軸目盛
  scale_y_continuous(breaks = y_vals) + # y軸目盛
  labs(title = "seq()")


# 格子点を作成
x_grid_df <- expand.grid(
  x = x_vals, # x軸の値
  y = y_vals  # y軸の値
)

# 散布図を作成
ggplot() + 
  geom_point(data = x_grid_df, mapping = aes(x = x, y = y)) + # 散布図
  scale_x_continuous(breaks = x_vals) + # x軸目盛
  scale_y_continuous(breaks = y_vals) + # y軸目盛
  labs(title = "expand.grid()") # ラベル


# 2次元ガウス分布の作図 -------------------------------------------------------------

# xの値を作成
x_vals <- seq(from = -5, to = 5, length.out = 51)

# xの点を作成
x_points <- expand.grid(x1 = x_vals, x2 = x_vals) %>% 
  as.matrix()


# 平均ベクトルを指定
mu_d <- c(0, 0)

# 分散共分散行列を指定
sigma_dd <- matrix(c(1, 0, 0, 1), nrow = 2, ncol = 2)


# 2次元ガウス分布を計算
dens_df <- tibble(
  x1 = x_points[, 1], # x軸の値
  x2 = x_points[, 2], # y軸の値
  density = mvnfast::dmvn(X = x_points, mu = mu_d, sigma = sigma_dd) # 確率密度
)

# 散布図を作成
ggplot() + 
  geom_point(data = dens_df, mapping = aes(x = x1, y = x2, color = density, alpha = density)) + # 散布図
  labs(title = "geom_point()") # ラベル

# 等高線図を作成
ggplot() + 
  geom_contour(data = dens_df, mapping = aes(x = x1, y = x2, z = density, color = ..level..)) + # 等高線図
  labs(title = "geom_contour()") # ラベル


# 2次元ガウス分布を作図
ggplot() + 
  geom_contour(data = dens_df, mapping = aes(x = x1, y = x2, z = density, color = ..level..)) + # 等高線図
  coord_equal() + # アスペクト比
  labs(title = "Multivariate Gaussian Distribution", 
       subtitle = paste0("mu=(", paste(mu_d, collapse = ", "), ")", 
                         ", lambda=(", paste(sigma_dd, collapse = ", "), ")"), 
       x = expression(x[1]), y = expression(x[2]), 
       color = "density") # ラベル


