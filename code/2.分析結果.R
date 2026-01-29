
require(stm)
require(quanteda)
require(ggplot2)
require(arrow)
require(dplyr)
require(jiebaRD)
require(jiebaR)
require(stringr)
require(grDevices)


# データの読み込み
news <- readRDS("data/news_full_cleaned.rds")
model_stm <- readRDS("data/model_stm_final_K0ver2.rds")
stmob <- readRDS("data/stmob_full_dataver2.rds")

# 保存先ディレクトリの作成
dir.create("result", recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------------------------
# 【1】各トピックの特徴語10語をCSVに保存
# ------------------------------------------------------------------------------
labels <- labelTopics(model_stm, n = 10)

# Prob（出現確率）とFREX（排他性）の単語をデータフレームにまとめる
topic_words_df <- data.frame(
  Topic = 1:93,
  Prob_Words = apply(labels$prob, 1, paste, collapse = ", "),
  FREX_Words = apply(labels$frex, 1, paste, collapse = ", ")
)

# Mac環境用にBOM付きUTF-8で保存
write.csv(topic_words_df, "repulication/全ての頻出語及び特徴語.csv", 
          row.names = FALSE, fileEncoding = "UTF-8")


# ------------------------------------------------------------------------------
# 【2】指定したトピックの代表的な文書をtext形式で保存（トピック1〜93、各5件）
# ------------------------------------------------------------------------------
# メタデータに対応するテキスト全体を作成
full_texts <- paste0("[", news$date, "] ", 
                     news$title, 
                     "\n\n", 
                     news$text)[as.numeric(rownames(stmob$meta))]

# トピック1〜93についてループ
for (t in 1:93) {
  # トピックtの代表文書を5件取得
  thoughts <- findThoughts(model_stm, texts = full_texts, topics = t, n = 5)
  
  # 5件のテキストをそれぞれファイルに保存
  for (i in 1:5) {
    doc_text <- thoughts$docs[[1]][i]
    
    # ゼロ埋めで番号を作成 (例: Topic52_03)
    topic_str <- sprintf("%02d", t)
    doc_str <- sprintf("%02d", i)
    header_title <- paste0("Topic", topic_str, "_", doc_str)
    
    # ファイル名の設定
    file_name <- paste0("repulication/全ての記事/", header_title, ".text")
    
    # 保存するテキストの結合
    output_text <- paste0(header_title, "\n\n", doc_text)
    
    # ファイル書き込み（Mac用のUTF-8）
    writeLines(output_text, con = file(file_name, encoding = "UTF-8"))
  }
}


# ------------------------------------------------------------------------------
# 【3】トピック割合の時期別推移を1つのPDFに保存（トピック1〜93）
# ------------------------------------------------------------------------------
# 1. 必要なライブラリのロード（cairo_pdfのために grDevices が必要）
require(ggplot2)
require(dplyr)
require(grDevices)

prep <- estimateEffect(1:93 ~ period, stmobj = model_stm, metadata = stmob$meta)

# 2. フォントの設定 (showtextを使うのが最も確実)
if (!require("showtext")) install.packages("showtext")
require(showtext)
font_add_google("Noto Sans JP", "notosans") # 日本語・中国語をカバー
showtext_auto()

# --- 保存設定 ---
output_path <- "repulication/全ての推定割合.pdf"
dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)

# 3. PDFデバイスを開く (pdf ではなく cairo_pdf を使用)
# family には showtext で登録した名前を指定します
cairo_pdf(output_path, width = 8, height = 6, family = "notosans")

# 各トピックのループ
for (t in 1:93) {
  
  # データ抽出 (omit.plot = TRUE で計算のみ実行)
  effect_data <- plot(prep, covariate = "period", topics = t, 
                      method = "pointestimate", model = model_stm, 
                      omit.plot = TRUE)
  
  # データフレーム作成
  df_plot <- data.frame(
    period   = effect_data$uvals,
    estimate = effect_data$means[[1]],
    ci_lower = effect_data$cis[[1]][1, ],
    ci_upper = effect_data$cis[[1]][2, ]
  )
  
  # ラベルと順序の定義（「鄧」を正確に記載）
  custom_labels <- c(
    "P1: 国共内戦", "P2: 毛沢東", "P3: 文革終焉",
    "P4: 鄧小平", "P5: 江沢民＋鄧小平", "P6: 移行期",
    "P7: 江沢民", "P8: 胡錦濤＋江沢民", "P9: 習近平"
  )
  
  # データの加工
  df_plot <- df_plot %>%
    mutate(is_significant = if_else(ci_lower > 0, "Significant", "Non-Significant")) %>%
    # P1を一番上にするために levels を逆順で定義
    mutate(period = factor(period, levels = rev(paste0("P", 1:9)), labels = rev(custom_labels)))
  
  # グラフ描画
  p <- ggplot(df_plot, aes(x = estimate, y = period, color = is_significant)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "red", alpha = 0.4) +
    geom_errorbarh(aes(xmin = ci_lower, xmax = ci_upper), height = 0.2) +
    geom_point(size = 3) +
    scale_color_manual(values = c("Significant" = "black", "Non-Significant" = "gray70")) +
    
    # スケーリングの自動化：0を起点にしつつ、最大値に合わせて右側を広げる
    expand_limits(x = 0) + 
    scale_x_continuous(expand = expansion(mult = c(0.01, 0.1))) + 
    
    labs(
      title = paste("Topic", t, "の時期別推移"),
      x = "Expected Topic Proportion",
      y = "Period",
      color = "統計的有意性"
    ) +
    theme_minimal(base_family = "notosans") + # フォントを適用
    theme(
      axis.text.y = element_text(size = 11, color = "black"),
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
      legend.position = "bottom"
    )
  
  # 1ページずつPDFへ出力
  print(p)
}

# 4. デバイスを閉じる
dev.off()
showtext_auto(FALSE) # 設定を戻す
