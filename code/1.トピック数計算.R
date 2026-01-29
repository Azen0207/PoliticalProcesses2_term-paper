# ==============================================================================
# 睡眠中放置用：STM本番実行スクリプト (K=0)
# ==============================================================================

#0.パッケージダウンロード
# install.packages("~/Downloads/jiebaRD_0.1.tar.gz", repos = NULL, type = "source")
# install.packages("~/Downloads/jiebaR_0.11.1.tar.gz", repos = NULL, type = "source")

# 1. ライブラリ読み込み
library(stm)
library(quanteda)
library(arrow)
library(dplyr)
library(jiebaRD)
library(jiebaR)

# 2. 安全対策：保存用ディレクトリの自動作成（ミス防止）
# "data" フォルダが存在しなければ作成します
if(!dir.exists("data")) {
  dir.create("data")
  print("保存用フォルダ 'data' を作成しました。")
}

# 進捗を記録するログファイルの準備
log_file <- "data/stm_run_logver2.txt"
cat(paste("=== STM実行ログ ===", "\n開始時間:", Sys.time(), "\n\n"), file = log_file)

# 3. データの読み込みと前処理
cat("データを読み込み中...\n", file = log_file, append = TRUE)
news_full <- read_parquet("data/taiwan.parquet")
news_full$date <- as.Date(news_full$date)

# テキスト処理
corp_full <- corpus(news_full, text_field = 'text')
seg1 <- worker(bylines = TRUE, stop_word = "data/ref/cn_stopwords.txt")
token_list_full <- segment(as.character(corp_full), seg1)
tokens_obj_full <- as.tokens(token_list_full)
docvars(tokens_obj_full) <- docvars(corp_full)

tokens_full <- tokens(tokens_obj_full, remove_numbers = TRUE, remove_punct = TRUE, remove_symbols = TRUE)
dfm_full <- dfm(tokens_full)

# 1文字の単語を削除（正規表現を使用：半角・全角問わず1文字のものを除外）
dfm_full <- dfm_select(dfm_full, min_nchar = 2)

# Trim (ここでもメタデータは保持されます)
dfm_full <- dfm_trim(dfm_full, min_docfreq = 0.005, max_docfreq = 0.90, docfreq_type = "prop")

# # ★重要：メモリ対策（Macを止めないため、語彙を少し厳しく絞ります）
# # min_docfreq = 0.01：全データの1%以上（500件以上）で使われた単語のみ残す
# # これにより、メモリ消費量が激減し、完走確率が上がります
# ver1
# dfm_full <- dfm_trim(dfm_full, min_docfreq = 0.01, max_docfreq = 0.85, docfreq_type = "prop")

cat(paste("前処理完了。語彙数:", nfeat(dfm_full), "\n\n"), file = log_file, append = TRUE)

# 4. STM形式への変換と期間変数の作成
stmob_full <- convert(dfm_full, to = "stm", docvars = docvars(dfm_full))

cut_points <- as.Date(c("1946-05-15", "1949-10-01", "1976-09-09", "1978-12-22", "1989-11-09", "1997-02-19", "1997-12-18", "2002-11-14", "2012-11-14", "2024-12-31"))
stmob_full$meta$period <- cut(stmob_full$meta$date, breaks = cut_points, labels = paste0("P", 1:9), include.lowest = TRUE)

# 5. 【本番】STMの実行と途中経過の保存
cat(paste("STM計算開始 (K=0):", Sys.time(), "\n※この処理は数時間〜一晩かかります。\n"), file = log_file, append = TRUE)

# 画面出力とログファイルの両方に進捗を流すための設定
sink(file = log_file, append = TRUE, split = TRUE)

# K=0の計算（verbose = TRUE でRのコンソールに進捗度が出続けます）
model_stm_final <- stm(
  documents = stmob_full$documents, 
  vocab = stmob_full$vocab, 
  K = 0,               
  prevalence = ~ period, 
  data = stmob_full$meta, 
  init.type = "Spectral",
  seed = 123,
  verbose = TRUE # 進捗を表示
)

# ログ出力の終了
sink()

# 6. 結果の確実な保存
saveRDS(model_stm_final, "data/model_stm_final_K0ver2.rds")
saveRDS(stmob_full, "data/stmob_full_dataver2.rds") # 可視化用にデータも保存

# 完了の記録
cat(paste("\n\n計算と保存が正常に完了しました！終了時間:", Sys.time(), "\n"), file = log_file, append = TRUE)
print("すべての処理が完了し、dataフォルダに保存されました。お疲れ様でした！")