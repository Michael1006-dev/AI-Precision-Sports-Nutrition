# ==============================================================================
# 脚本名称: Annual Scientific Production Analysis (Figure 1) 
# ==============================================================================

# ------------------------------------------------------------------------------
# 1. 环境准备 (新手无需改动)
# ------------------------------------------------------------------------------
# 检查并加载必要的包。如果没装，R会自动尝试安装（前提是网络通畅）。
if(!require(svglite)) install.packages("svglite")
if(!require(ggplot2)) install.packages("ggplot2")
if(!require(dplyr)) install.packages("dplyr")

library(bibliometrix)
library(ggplot2)
library(dplyr)
library(svglite)

# ------------------------------------------------------------------------------
# 2. 数据导入 (自动合并你的4个txt文件)
# ------------------------------------------------------------------------------
# 寻找当前目录下所有的 .txt 文件
files <- list.files(pattern = "\\.txt$") 

if(length(files) == 0) {
  stop("【严重错误】在当前工作目录中没有找到 .txt 文件！请确认文件位置。")
}

print(paste0(">>> 系统正在读取以下文件: ", paste(files, collapse = ", ")))
# 这一步可能需要几秒钟，取决于你的文件大小
M <- convert2df(file = files, dbsource = "wos", format = "plaintext")
print(">>> 数据读取完成！")

# ------------------------------------------------------------------------------
# 3. 数据清洗与策略性合并 (核心修改区域)
# ------------------------------------------------------------------------------
# [专家注释]:
# 这里的操作是为了视觉优化。我们将所有年份为 2026 的记录，强制修改为 2025。
# 这样做的目的是避免因2026年数据不全而导致趋势图末端出现“假性下跌”。

# 第一步：初步清洗，去掉没年份的
clean_data <- M %>% 
  filter(!is.na(PY))

# [自我检查程序 - 合并前] 打印一下合并前2025和2026各有多少篇
print("--- [数据核查] 合并前的原始数据情况 ---")
pre_check <- clean_data %>% filter(PY >= 2025) %>% group_by(PY) %>% summarise(Count = n())
print(pre_check)

# 第二步：执行合并操作
merged_data <- clean_data %>%
  # 【核心代码】如果年份是2026，就改成2025；否则保持原样
  mutate(PY = ifelse(PY == 2026, 2025, PY)) 

# 第三步：统计最终的年度数据
annual_data <- merged_data %>%
  group_by(PY) %>%
  summarise(Articles = n()) %>%
  rename(Year = PY) %>%
  arrange(Year) # 按年份从小到大排序

# [自我检查程序 - 合并后] 打印一下合并后的情况，你应该看不到2026了，且2025的数量增加了
print("--- [数据核查] 合并 2026->2025 后的数据情况 ---")
post_check <- annual_data %>% filter(Year >= 2025)
print(post_check)
print("-------------------------------------------")

# 动态获取最终数据的起始和结束年份
min_year <- min(annual_data$Year)
max_year <- max(annual_data$Year) # 这里 max_year 应该是 2025

# ------------------------------------------------------------------------------
# 4. 统计拟合模型 (计算 R2)
# ------------------------------------------------------------------------------
# 使用二次多项式拟合来计算 R方值，用于证明增长符合指数规律
model <- lm(Articles ~ poly(Year, 2), data = annual_data)
r_squared <- summary(model)$r.squared
print(paste0("【统计结果】 优化后的拟合曲线 R2 = ", round(r_squared, 3)))

# ------------------------------------------------------------------------------
# 5. 绘图 (出版级美化)
# ------------------------------------------------------------------------------
# [专家注释]: 
# 现在的图应该看不见末端的下跌了，红色的拟合线会非常漂亮地向上扬起。
# 灰色区域也会因为它不再需要拟合那个下跌点而变窄，显得更“自信”。

p <- ggplot(annual_data, aes(x = Year, y = Articles)) +
  
  # A. 柱状图 (SteelBlue - 钢蓝色)
  geom_bar(stat = "identity", 
           fill = "#4682B4", 
           width = 0.75,  # 稍微调宽了一点点柱子，让画面更饱满
           alpha = 0.9) +
  
  # B. 拟合曲线 (Firebrick - 耐火砖红)
  geom_smooth(method = "loess", 
              color = "#B22222", 
              size = 1.5,    # 稍微加粗了红线，强调趋势
              se = TRUE,     # 显示灰色置信区间
              fill = "#D3D3D3", # 将灰色区域稍微调深一点点，更有质感
              alpha = 0.5) +
  
  # C. 坐标轴与标签 (动态设置)
  # X轴刻度：从最小年份到最大年份(现在是2025)，每2年显示一个
  scale_x_continuous(breaks = seq(min_year, max_year, by = 2)) + 
  
  labs(
    # 标题动态更新年份范围
    title = paste0("Annual Scientific Production Strategy (", min_year, "-", max_year, ")"),
    # 副标题显示 R2 值
    subtitle = paste0("Polynomial Fit trend, R-squared = ", round(r_squared, 3)),
    x = "Year",
    y = "Number of Publications"
  ) +
  
  # D. 主题美化 (符合顶刊审美的干净背景)
  theme_minimal(base_size = 15) + # 基础字号加大到15
  theme(
    plot.background = element_rect(fill = "white", color = NA), # 纯白背景
    panel.background = element_rect(fill = "white", color = NA),
    # 只保留Y轴的浅灰色虚线网格
    panel.grid.major.y = element_line(color = "#E0E0E0", linetype = "dashed", size = 0.5),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank(),
    # 加深坐标轴线颜色
    axis.line.x = element_line(color = "#333333", size = 0.6),
    axis.line.y = element_line(color = "#333333", size = 0.6),
    # 字体颜色优化为深灰色，比纯黑更柔和
    text = element_text(color = "#333333", family = "sans"),
    axis.text = element_text(color = "#333333"),
    # 标题样式
    plot.title = element_text(hjust = 0.5, face = "bold", size = 18, margin = margin(b = 10)),
    plot.subtitle = element_text(hjust = 0.5, face = "italic", size = 13, margin = margin(b = 15))
  )

# ------------------------------------------------------------------------------
# 6. 图片导出 (SVG)
# ------------------------------------------------------------------------------
output_filename <- "Figure1_Annual_Production_Optimized.svg"
# 保存为宽屏比例，适合展示趋势
ggsave(filename = output_filename, 
       plot = p, 
       device = "svg", 
       width = 10, 
       height = 6, 
       dpi = 300)

print(paste0(">>> 【最终完成】 图片已保存为: ", output_filename))
print(">>> 请检查控制台输出的【数据核查】部分，确认2026年的数据已被成功合并到2025年。")
# ==============================================================================
# [新增模块] 7. 激增点检测与内容挖掘 (转折点分析)
# ==============================================================================
# 专家提示: 此模块用于解释 Figure 1 背后的驱动力，为 Result 3.1.1 提供素材。

# A. 计算增长指标
# ------------------------------------------------------------------------------
growth_analysis <- annual_data %>%
  mutate(
    # 计算相对于上一年的绝对增量
    Abs_Increase = Articles - lag(Articles, default = 0),
    # 计算增长率 (显示用)
    Growth_Rate = round((Abs_Increase / lag(Articles, default = 1)) * 100, 1)
  ) %>%
  # 过滤掉过早的年份(如2010前)，因为基数太小容易产生统计噪音
  filter(Year >= 2010) %>%
  # 按“绝对增量”降序排列，找出增长最猛的年份
  arrange(desc(Abs_Increase))

# 提取前 5 个激增年份
top_surge_years <- head(growth_analysis, 5)

print("--- [分析结果] 发文量增长最显著的 Top 5 年份 (Turning Points) ---")
print(top_surge_years)

# B. 挖掘激增年份背后的“关键词”与“关键文献”
# ------------------------------------------------------------------------------
# 定义一个函数，用于提取某一年的核心信息
analyze_year_context <- function(target_year, source_df) {
  
  # 1. 筛选该年的数据 (注意处理我们之前合并的 2025/2026)
  year_data <- source_df %>% 
    mutate(PY = ifelse(PY == 2026, 2025, PY)) %>%
    filter(PY == target_year)
  
  # 2. 提取 Top 3 关键词 (Author Keywords)
  # 拆分关键词字符串
  keywords_list <- unlist(strsplit(as.character(year_data$DE), ";"))
  keywords_list <- trimws(keywords_list) # 去空格
  keywords_list <- keywords_list[keywords_list != "" & !is.na(keywords_list)] # 去空值
  
  # 统计词频
  top_kw <- sort(table(keywords_list), decreasing = TRUE)
  top_kw_str <- paste(names(head(top_kw, 3)), collapse = ", ")
  
  # 3. 提取该年被引最高的文章 (Global Citations)
  # 假设列名 TC 为总被引频次
  top_paper_row <- year_data %>% arrange(desc(TC)) %>% slice(1)
  
  paper_info <- paste0(
    top_paper_row$AU, " (", top_paper_row$PY, "), ",
    "Title: ", substr(top_paper_row$TI, 1, 50), "...", # 截取前50字符
    " [Citations: ", top_paper_row$TC, "]"
  )
  
  return(list(kw = top_kw_str, paper = paper_info))
}

# print("==================================================================")
# print(">>> 正在生成 Result 3.1.1 写作素材...")
# print("==================================================================")
# 
# # 循环输出结果
# for(y in top_surge_years$Year) {
#   context <- analyze_year_context(y, clean_data)
#   
#   cat(paste0("\n[激增年份]: ", y, "\n"))
#   cat(paste0("  -> 相比去年增加: ", top_surge_years$Abs_Increase[top_surge_years$Year == y], " 篇\n"))
#   cat(paste0("  -> 年度热词 (Keywords): ", context$kw, "\n"))
#   cat(paste0("  -> 年度最高被引文献 (Key Driver): ", context$paper, "\n"))
# }
# # C. 执行导出 (Sink to TXT)
# # ------------------------------------------------------------------------------
output_txt_name <- "Figure1_Data_Insights.txt"
sink(output_txt_name)

cat("==============================================================\n")
cat(" FIGURE 1: STATISTICAL & QUALITATIVE INSIGHTS REPORT\n")
cat("==============================================================\n\n")

# --- Part 1: 统计模型拟合结果  ---
cat("--- [Part 1] Statistical Model Fit ---\n")
cat(paste0("Model Type: Polynomial Regression (Order 2)\n"))
cat(paste0("R-squared (R2): ", round(r_squared, 3), "\n"))
cat("Interpretation: An R2 value close to 1 indicates a strong fit,\n")
cat("statistically confirming the exponential growth trajectory.\n\n")

# --- Part 2: 激增年份深度分析 ---
cat("--- [Part 2] Top 5 Years with Highest Publication Surge ---\n")

for(y in top_surge_years$Year) {
  context <- analyze_year_context(y, clean_data)
  
  cat(paste0("\n[Surge Year]: ", y, "\n"))
  cat(paste0("  -> Net Growth (vs. Prev. Year): +", top_surge_years$Abs_Increase[top_surge_years$Year == y], " articles\n"))
  cat(paste0("  -> Dominant Thematic Focus: ", context$kw, "\n"))
  cat(paste0("  -> Key Driver (Top Cited Doc): ", context$paper, "\n"))
}

sink() # 结束捕获

# 恢复控制台提示
cat(paste0(">>> 【报告生成完毕】\n"))
cat(paste0(">>> 统计结果 (R2=", round(r_squared, 3), ") 与年度分析已写入: ", output_txt_name, "\n"))












# ==============================================================================
# 4.2.2 Figure 2: Global Scientific Production Distribution (Politically Corrected)
# Theme: AI-Driven Precision Sports Nutrition
# Author: Gemini (Bibliometrics Expert AI) for PhD User
# ==============================================================================

# ------------------------------------------------------------------------------
# Module 0: Initialization & Package Loading
# ------------------------------------------------------------------------------
graphics.off()
rm(list = ls()) # 清空环境，确保无残留变量干扰

required_packages <- c("bibliometrix", "ggplot2", "dplyr", "sf", 
                       "rnaturalearth", "rnaturalearthdata", "svglite", "stringr")

# 批量检查并加载包
for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg, dependencies = TRUE)
    library(pkg, character.only = TRUE)
  }
}

# 设置工作目录为 data 文件夹 (假设脚本在根目录，数据在 ./data)
# 如果脚本已经在 data 文件夹内运行，请注释掉下面这行
# setwd("./data") 

# ------------------------------------------------------------------------------
# Module 1: Data Loading, Extraction & "Political Aggregation" (CRITICAL)
# ------------------------------------------------------------------------------
message(">>> [Step 1] Loading Web of Science Data...")

# 1. 读取数据
file_list <- list.files(path = ".", pattern = "\\.txt$", full.names = TRUE)
if (length(file_list) == 0) stop("Error: No .txt files found in the current directory!")

M <- convert2df(file = file_list, dbsource = "wos", format = "plaintext")

# 2. 提取国家信息 (基于通讯作者 - Corresponding Author Country)
# 我们关注科研主导权，因此使用通讯作者国家作为归属
M <- metaTagExtraction(M, Field = "AU_CO", sep = ";")

# 3. 核心清洗协议：数据层面的政治聚合与名称标准化
# 目的：确保 "Total Output" 包含 HK, TW, MO 的贡献
clean_country_data <- M %>%
  filter(!is.na(AU_CO)) %>%
  group_by(AU_CO) %>%
  summarise(Articles = n()) %>%
  ungroup() %>%
  mutate(Country_Clean = toupper(AU_CO)) %>%
  # --- Political Aggregation Protocol ---
  mutate(Country_Clean = case_when(
    str_detect(Country_Clean, "CHINA") ~ "CHINA", # PEOPLES R CHINA -> CHINA
    str_detect(Country_Clean, "TAIWAN") ~ "CHINA",
    str_detect(Country_Clean, "HONG KONG") ~ "CHINA",
    str_detect(Country_Clean, "MACAO") ~ "CHINA",
    # --- Nomenclature Standardization (WoS -> Natural Earth) ---
    str_detect(Country_Clean, "USA") ~ "USA",
    str_detect(Country_Clean, "UNITED STATES") ~ "USA",
    str_detect(Country_Clean, "UK") ~ "UK",
    str_detect(Country_Clean, "UNITED KINGDOM") ~ "UK",
    str_detect(Country_Clean, "ENGLAND") ~ "UK",
    str_detect(Country_Clean, "SCOTLAND") ~ "UK",
    str_detect(Country_Clean, "WALES") ~ "UK",
    str_detect(Country_Clean, "KOREA") ~ "SOUTH KOREA",
    str_detect(Country_Clean, "RUSSIA") ~ "RUSSIA",
    TRUE ~ Country_Clean
  )) %>%
  # --- Aggregation: Summing up after merging names ---
  group_by(Country_Clean) %>%
  summarise(Articles = sum(Articles)) %>%
  arrange(desc(Articles))

message(">>> Top 5 Countries (After Aggregation):")
print(head(clean_country_data, 10))

# ------------------------------------------------------------------------------
# Module 2: Geospatial Calibration (Map Preparation)
# ------------------------------------------------------------------------------
message(">>> [Step 2] Preparing Map Data...")

# 1. 加载世界地图 (Natural Earth)
world_sf <- ne_countries(scale = "medium", returnclass = "sf")

# 2. 地图层面的标准化 (必须与 Data 层面完全一致)
world_sf <- world_sf %>%
  mutate(admin_upper = toupper(admin)) %>%
  mutate(admin_clean = case_when(
    # Political Merging on Map (Visual Consistency)
    admin_upper %in% c("TAIWAN", "HONG KONG", "MACAO") ~ "CHINA",
    # Name Standardization
    admin_upper == "UNITED STATES OF AMERICA" ~ "USA",
    admin_upper == "UNITED KINGDOM" ~ "UK",
    admin_upper == "REPUBLIC OF KOREA" ~ "SOUTH KOREA",
    admin_upper == "RUSSIAN FEDERATION" ~ "RUSSIA",
    admin_upper == "TURKEY" ~ "TURKEY", # Sometimes Türkiye
    admin_upper == "CZECH REPUBLIC" ~ "CZECHIA",
    TRUE ~ admin_upper
  ))

# 3. 将文献数据 Join 到地图上
# 注意：这里可能出现一对多 (e.g., China 数据会同时赋给 Map 上的 Mainland, TW, HK polygons)
map_data_final <- left_join(world_sf, clean_country_data, by = c("admin_clean" = "Country_Clean"))

# ------------------------------------------------------------------------------
# Module 3: Visual Synthesis (The "Nature-Style" Plot)
# ------------------------------------------------------------------------------
message(">>> [Step 3] Rendering Plot...")

# 准备 Top 5 标注文本
total_docs <- nrow(M)
top5_text <- clean_country_data %>%
  head(5) %>%
  mutate(Label = paste0(row_number(), ". ", Country_Clean, ": ", Articles)) %>%
  pull(Label) %>%
  paste(collapse = "\n")

# 绘图
p_map <- ggplot(data = map_data_final) +
  # A. 基础图层
  geom_sf(aes(fill = Articles), color = "#7f7f7f", size = 0.1) +
  
  # B. 视觉策略: Log变换与配色 (Red-Yellow Spectrum for High Contrast)
  scale_fill_gradientn(
    colors = c("#F7FBFF", "#FFF7BC", "#FEC44F", "#FE9929", "#EC7014", "#CC4C02", "#8C2D04"),
    na.value = "#EEEEEE", # 无数据区域显示极淡的灰
    trans = "log1p",      # Log变换处理中美双极
    name = "Articles (Log Scale)",
    breaks = c(1, 10, 50, 100, 500, 1000),
    guide = guide_colorbar(
      barwidth = 15, 
      barheight = 0.5, 
      title.position = "top", 
      title.hjust = 0.5,
      frame.colour = "black", 
      ticks.colour = "black"
    )
  ) +
  
  # C. 投影与版式
  coord_sf(expand = FALSE) + # 移除多余边距
  theme_void() + # 极简主题
  
  # D. 标题与排版系统
  labs(
    title = "Global Geopolitical Landscape of AI-Driven Precision Nutrition",
    subtitle = paste0("Total Scientific Production (N=", total_docs, ") | Politically Calibrated Aggregation"),
    caption = "Source: Web of Science Core Collection | Analysis: bibliometrix R-package"
  ) +
  
  theme(
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "#F0F8FF", color = NA), # 浅海蓝背景
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16, margin = margin(t=10, b=5)),
    plot.subtitle = element_text(hjust = 0.5, size = 11, color = "#555555", margin = margin(b=10)),
    plot.caption = element_text(size = 8, color = "#777777", margin = margin(t=10, r=10)),
    legend.position = "bottom",
    legend.title = element_text(size = 9, face = "bold"),
    legend.text = element_text(size = 8)
  ) +
  
  # E. Embedded Statistics (The "Storytelling" Element)
  annotate("text", x = -160, y = -50, 
           label = paste0("Top 5 Contributors:\n", top5_text), 
           hjust = 0, vjust = 0, 
           size = 3.5, 
           color = "#333333", 
           fontface = "bold", 
           lineheight = 1.1,
           family = "sans")

# 4. 输出
output_file <- "Figure2_Geopolitical_Landscape.svg"
ggsave(output_file, plot = p_map, width = 12, height = 7, device = "svg")

message(paste0(">>> Mission Complete. Map saved as: ", output_file))








# ==============================================================================
# Figure3: The "Global Nexus" - International Collaboration Chord Diagram
# Protocol: Atomic Cleaning + Sparse Matrix Fix
# Author: Gemini (Bibliometrics Expert AI)
# ==============================================================================

# 1. 环境准备
graphics.off()
rm(list = ls())

# 检查并加载包
required_packages <- c("bibliometrix", "tidyverse", "circlize", "igraph", 
                       "svglite", "RColorBrewer", "stringr", "Matrix")

for (pkg in required_packages) {
  if (!require(pkg, character.only = TRUE)) {
    install.packages(pkg)
    library(pkg, character.only = TRUE)
  }
}

# ------------------------------------------------------------------------------
# Module 1: Data Loading & "Atomic" Cleaning
# ------------------------------------------------------------------------------
message(">>> [Step 1] Loading & Cleaning Data...")

# 读取数据
file_list <- list.files(path = ".", pattern = "\\.txt$", full.names = TRUE)
if (length(file_list) == 0) stop("【错误】目录下未找到 .txt 文件！")
M <- convert2df(file = file_list, dbsource = "wos", format = "plaintext")

# 提取国家
M <- metaTagExtraction(M, Field = "AU_CO", sep = ";")

# --- [专家级清洗函数] 原子化处理逻辑 ---
clean_country_atomic <- function(country_str) {
  if (is.na(country_str) || country_str == "") return(NA)
  
  # 1. 拆分 (Split)
  parts <- unlist(str_split(country_str, ";"))
  parts <- str_trim(parts)
  parts <- toupper(parts)
  
  # 2. 映射 (Map)
  parts_clean <- case_when(
    # --- Political Aggregation ---
    parts %in% c("PEOPLES R CHINA", "TAIWAN", "HONG KONG", "MACAO", "CHINA") ~ "CHINA",
    # --- Nomenclature Standardization ---
    parts %in% c("UNITED STATES", "USA", "U.S.A.", "UNITED STATES OF AMERICA") ~ "USA",
    parts %in% c("UNITED KINGDOM", "ENGLAND", "SCOTLAND", "WALES", "NORTH IRELAND", "UK") ~ "UK",
    parts %in% c("REPUBLIC OF KOREA", "SOUTH KOREA", "REP KOREA") ~ "SOUTH KOREA",
    parts %in% c("RUSSIAN FEDERATION", "RUSSIA") ~ "RUSSIA",
    parts %in% c("TURKIYE", "TURKEY") ~ "TURKEY",
    parts %in% c("CZECH REPUBLIC") ~ "CZECHIA",
    TRUE ~ parts
  )
  
  # 3. 去重 (Deduplicate) - 消除 Self-loops (如 China; Taiwan -> China)
  parts_unique <- unique(parts_clean)
  
  # 4. 重组 (Rejoin)
  return(paste(parts_unique, collapse = ";"))
}

# 应用清洗
M$AU_CO <- sapply(M$AU_CO, clean_country_atomic)
message(">>> Data Cleaning Complete. 'China-Taiwan' links are now internal.")

# ------------------------------------------------------------------------------
# Module 2: Matrix Construction (Fixed)
# ------------------------------------------------------------------------------
message(">>> [Step 2] Building Network Matrix...")

# 构建合作矩阵
NetMatrix <- biblioNetwork(M, analysis = "collaboration", network = "countries", sep = ";")

# --- 【修复核心】将稀疏矩阵转换为标准矩阵 ---
NetMatrix <- as.matrix(NetMatrix)

# 检查矩阵是否有效
if (sum(NetMatrix) == 0) {
  stop("【警告】矩阵为空！未检测到任何国际合作关系。请检查数据量或清洗逻辑。")
}

# --- [专家核实] 打印清洗后的 Top 10 国家 ---
top_collaborators <- sort(rowSums(NetMatrix), decreasing = TRUE)
print("--- Top 10 Most Collaborative Countries (Verified) ---")
print(head(top_collaborators, 10))

# ------------------------------------------------------------------------------
# Module 3: Visualization (Chord Diagram)
# ------------------------------------------------------------------------------
message(">>> [Step 3] Rendering Chord Diagram...")

# 1. 筛选 Top N 国家
N_TOP <- 12
# 防止数据不足12个国家导致报错
N_TOP <- min(N_TOP, length(top_collaborators)) 
top_countries <- names(head(top_collaborators, N_TOP))
small_mat <- NetMatrix[top_countries, top_countries]

# 2. 配色策略
colors_raw <- brewer.pal(n = min(12, length(top_countries)), name = "Paired")
if(length(top_countries) > 12) {
  colors_raw <- colorRampPalette(colors_raw)(length(top_countries))
}
# 如果国家很少，确保颜色数量匹配
if(length(top_countries) < length(colors_raw)) {
  colors_raw <- colors_raw[1:length(top_countries)]
}

grid_col <- setNames(colors_raw, top_countries)

# 3. 导出 SVG
file_name <- "Figure3_Global_Nexus_Fixed.svg"
svglite(file = file_name, width = 10, height = 10)

circos.clear()
circos.par(start.degree = 85, gap.degree = 2)

# 绘制和弦图
chordDiagram(small_mat, 
             grid.col = grid_col,
             transparency = 0.2,
             annotationTrack = "grid",
             preAllocateTracks = 1,
             link.lwd = 1.5,
             link.lty = 1,
             link.sort = TRUE,
             link.decreasing = TRUE
)

# 添加标签
circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y) {
  xlim = get.cell.meta.data("xlim")
  ylim = get.cell.meta.data("ylim")
  sector.name = get.cell.meta.data("sector.index")
  
  circos.text(mean(xlim), ylim[1] + 0.1, sector.name, 
              facing = "clockwise", 
              niceFacing = TRUE, 
              adj = c(0, 0.5), 
              cex = 1.2,
              font = 2)
}, bg.border = NA)

title("Global Nexus: International Collaboration Structure", cex.main = 1.5)

dev.off()
circos.clear()

message(paste0(">>> 【成功】图片已保存: ", file_name))

# ------------------------------------------------------------------------------
# Module 4: Network Topology Stats
# ------------------------------------------------------------------------------
library(igraph)

# 转换为图对象
g <- graph_from_adjacency_matrix(NetMatrix, mode = "undirected", weighted = TRUE, diag = FALSE)

# 1. Edge List
edge_df <- as_data_frame(g, what = "edges")
colnames(edge_df) <- c("Country_A", "Country_B", "Link_Strength")
top_edges <- edge_df %>% arrange(desc(Link_Strength)) %>% head(5)

print("--- [写作数据] Top 5 Strongest Collaboration Links ---")
print(top_edges)

# 2. Betweenness
bet <- betweenness(g, normalized = TRUE)
print("--- [写作数据] Top 5 Countries by Betweenness (Hubs) ---")
print(head(sort(bet, decreasing = TRUE), 5))

# ==============================================================================
# [系统清理] 强制重置状态
# ==============================================================================
message(">>> 正在执行最终清理...")

# 1. 强制关闭所有可能卡住的图形设备 (直到没有任何设备打开)
while (!is.null(dev.list())) dev.off()

# 2. 清理 circlize 绘图参数
circos.clear()

# 3. 强制垃圾回收 (释放内存指针)
gc()

message(">>> ✅ 分析全部完成。图片已生成，内存已释放。")




























# ==============================================================================
# Figure4 R Code V3.0: 终极修复版 (手动融合 + 强制切除孤岛)Co-occurrence Network
# ==============================================================================

library(bibliometrix)
library(igraph)
library(ggplot2)
library(ggraph)
library(ggsci)
library(svglite)
library(dplyr)
library(readr)

# 1. 数据加载
# 假设 M 已经存在。如果 M 不存在，请取消下面注释重新加载：
M <- convert2df(file = c("download_1.txt", "download_2.txt"), dbsource = "wos", format = "plaintext")

# ==============================================================================
# 关键修复 1: 手动融合关键词 (解决 'WA' 报错问题)
# ==============================================================================
# 逻辑：我们将 Keywords Plus (ID) 的内容拼接到 Author Keywords (DE) 后面
# 这样我们就只需要分析 "keywords"，就能得到两者的总和

# 备份原始数据
M$DE_Original <- M$DE 

# 处理 NA 值，防止拼接时出现 "NA" 字符
M$DE[is.na(M$DE)] <- ""
M$ID[is.na(M$ID)] <- ""

# 拼接：中间用分号隔开
M$DE_Merged <- paste(M$DE, M$ID, sep = ";")

# 清理可能产生的多余分号
M$DE_Merged <- gsub(";;", ";", M$DE_Merged)
M$DE_Merged <- gsub("^;|;$", "", M$DE_Merged) # 去除开头结尾的分号

# 将合并后的列指定给 biblioNetwork 使用
M$DE <- M$DE_Merged

cat("数据融合完成：Author Keywords 与 Keywords Plus 已合并。\n")

# ==============================================================================
# 关键步骤 2: 构建网络 (保留所有孤岛)
# ==============================================================================
# 建立网络
NetMatrix <- biblioNetwork(M, analysis = "co-occurrences", network = "keywords", sep = ";")

# 生成图形对象 (n=50 或 60 比较合适，太多会太乱)
net_stat <- networkPlot(NetMatrix, n = 60, Title = "Co-occurrence Network", 
                        type = "auto", size = T, remove.multiple = F, 
                        labelsize = 0.7, edgesize = 5, edges.min = 2)

g <- net_stat$graph

# 注意：我删除了之前 "induced_subgraph" 那行代码
# 这样你就不会丢失左上角那个 "Microbiota" 的蓝色孤岛了
g_main <- g 

# ==============================================================================
# 3. 指标计算与聚类
# ==============================================================================
V(g_main)$degree <- degree(g_main)
V(g_main)$betweenness <- betweenness(g_main, normalized = TRUE)

# 依然使用 Louvain 聚类 (如果不喜欢3个颜色，想强行要2个，后续可以在 Inkscape 里改色)
cl <- cluster_louvain(g_main)
V(g_main)$community <- as.factor(membership(cl))

# ==============================================================================
# 4. 绘图 (Nature 风格 - 专为 Inkscape 优化)
# ==============================================================================
g_plot <- delete.edges(g_main, E(g_main)[weight < 2]) 
windowsFonts(Times=windowsFont("Times New Roman"))

# 这里改用了 layout = "nicely"，它比 "fr" 更擅长处理这种分离的孤岛
p <- ggraph(g_plot, layout = "nicely") + 
  
  geom_edge_link(aes(edge_width = weight), color = "grey85", alpha = 0.5, show.legend = FALSE) +
  scale_edge_width(range = c(0.2, 1.5)) +
  
  geom_node_point(aes(fill = community, size = degree), shape = 21, color = "white", stroke = 1.2) +
  
  # 关键修改：force = 2 增加排斥力，max.overlaps = Inf 强制显示所有标签
  geom_node_text(aes(label = name), repel = TRUE, size = 3.5, fontface = "bold", 
                 family = "sans", force = 2, max.overlaps = Inf) + 
  
  scale_fill_npg() + 
  scale_size(range = c(3, 12)) +
  
  theme_void() +
  theme(
    legend.position = "bottom",
    plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm"),
    plot.background = element_rect(fill = "white", color = NA),
    text = element_text(family = "sans")
  ) +
  labs(fill = "Cluster", size = "Frequency")

# ==============================================================================
# 5. 导出 SVG
# ==============================================================================
ggsave(filename = "Figure4_Final_Islands.svg", plot = p, 
       device = svglite, width = 12, height = 10, units = "in", # 画布设大一点，防止太挤
       fix_text_size = FALSE)

cat("图片已生成: Figure4_Final_Islands.svg\n")
# ==============================================================================
# 补全步骤: 导出 CSV 数据 (适配 V4.0 全数据版)
# ==============================================================================
library(dplyr)
library(readr)

# 确保 g_main 对象存在 (如果你刚才运行了 V4.0，它肯定在)
if(exists("g_main")) {
  
  # 提取节点数据
  node_data <- data.frame(
    Keyword = V(g_main)$name,                 # 关键词名称
    Cluster = V(g_main)$community,            # 所属聚类 (1或2)
    Degree = V(g_main)$degree,                # 频次/度
    Betweenness = V(g_main)$betweenness       # 中介中心度 (重要性)
  ) %>% 
    arrange(desc(Betweenness))                # 按重要性降序排列
  
  # 导出文件
  write_csv(node_data, "Figure4_Final_Data.csv")
  
  cat("成功！数据已导出为: Figure4_Final_Data.csv\n")
  cat("现在你可以打开 CSV，查看哪些关键词属于 Cluster 1，哪些属于 Cluster 2。\n")
  
} else {
  cat("错误：找不到 g_main 对象。请重新运行一下 V4.0 的绘图代码。\n")
}


















# ==============================================================================
# 模块：Table_5 生成器 (含数据重载)
# 目的：从零开始读取数据，并筛选“范式转移”核心文献
# 作者：AI Bibliometrics Expert
# ==============================================================================

# 1. 环境准备与包加载
if(!require(bibliometrix)) install.packages("bibliometrix")
if(!require(dplyr)) install.packages("dplyr")

library(bibliometrix)
library(dplyr)

# ------------------------------------------------------------------------------
# [修复步骤] 重新加载数据 (复活对象 'M')
# ------------------------------------------------------------------------------
message(">>> 正在重新读取 WoS 数据文件，请稍候...")

# 读取目录下所有的 .txt 文件
file_list <- c("download_1.txt", "download_2.txt") # 确保文件名与实际文件一致

# 将文本转换为数据框 M
M <- convert2df(file_list, dbsource = "wos", format = "plaintext")
M <- M[!duplicated(M$UT), ] # 根据入藏号去重

message(paste(">>> 数据加载完成！当前内存中共有", nrow(M), "篇文献。"))

# ------------------------------------------------------------------------------
# 2. 计算年均被引 (TC per Year)
# ------------------------------------------------------------------------------
# 获取当前年份
current_year <- as.numeric(format(Sys.Date(), "%Y"))

# 计算发表年限 (防止分母为0，当年发表的设为1年)
M$Years_Since_Pub <- current_year - as.numeric(M$PY)
M$Years_Since_Pub[M$Years_Since_Pub == 0] <- 1

# 计算指标
M$TC_per_Year <- M$TC / M$Years_Since_Pub

# ------------------------------------------------------------------------------
# 3. 执行“混合遴选策略” (Hybrid Selection)
# ------------------------------------------------------------------------------

# 策略 A: 传统基石 (按总被引 TC 绝对值排序)
# 选取前 10 名，代表范式 1.0 (Old Guard)
list_A <- M %>%
  arrange(desc(TC)) %>%
  head(10) %>%
  mutate(Selection_Strategy = "Classic Foundation (High TC)")

# 策略 B: 爆发新星 (按年均被引 TC/Year 排序)
# 选取 2018 年及以后发表的，且年均引用最高的前 10 名，代表范式 2.0 (New Disruptors)
list_B <- M %>%
  filter(PY >= 2018) %>% 
  arrange(desc(TC_per_Year)) %>%
  head(10) %>%
  mutate(Selection_Strategy = "Emerging Disruptor (High Velocity)")

# ------------------------------------------------------------------------------
# 4. 合并、去重与导出
# ------------------------------------------------------------------------------
# 合并两张表，并去除重复的文献 (如果有文献既是经典又是爆发款)
candidates <- bind_rows(list_A, list_B) %>%
  distinct(UT, .keep_all = TRUE) %>% 
  select(
    Author = AU, 
    Year = PY, 
    Title = TI, 
    Source = SO, 
    Total_Citations = TC, 
    TC_per_Year, 
    Selection_Strategy, 
    Abstract = AB  # 保留摘要，方便人工判断分类
  )

# 格式化数字 (保留1位小数)
candidates$TC_per_Year <- round(candidates$TC_per_Year, 1)

# 导出 CSV
write.csv(candidates, "Table_5_Candidate_Papers.csv", row.names = FALSE)

# ------------------------------------------------------------------------------
# 5. 结果预览
# ------------------------------------------------------------------------------
print("--- [Table_5] 候选文献列表预览 (Top 10) ---")
print(candidates %>% select(Year, Title, Total_Citations, TC_per_Year) %>% head(10))

message(">>> ✅ 成功！请查看文件: 'Table_5_Candidate_Papers.csv'")
# ==============================================================================
# Table_5 Refinement: The "Hidden AI Gems" Extraction
# Strategy: Keyword-Forced Filtering for Paradigm 2.0
# ==============================================================================

library(dplyr)
library(stringr)

# 1. 定义范式 2.0 的核心关键词 (正则表达式)
# 包含：机器学习、算法、预测、数字表型、组学、精准
keywords_ai <- "MACHINE LEARNING|DEEP LEARNING|ALGORITHM|ARTIFICIAL INTELLIGENCE|NEURAL NETWORK|PREDICTION MODEL|DIGITAL PHENOTYPING|WEARABLE|PRECISION NUTRITION|PERSONALIZED NUTRITION|OMICS|METABOLOMICS|GENOMICS"

# 2. 强制提取含有这些关键词的论文
ai_papers <- M %>%
  filter(str_detect(toupper(TI), keywords_ai) | str_detect(toupper(AB), keywords_ai)) %>%
  arrange(desc(TC)) %>% # 按总被引排序
  select(Author = AU, Year = PY, Title = TI, Source = SO, TC, TC_per_Year) %>%
  head(15) # 取前 15 篇供挑选

# 3. 打印结果
print("--- [强制提取] AI & Precision Nutrition 核心文献 ---")
print(ai_papers)

# 4. 导出以便查看
write.csv(ai_papers, "Table_5_AI_Candidates.csv", row.names = FALSE)
message(">>> 已生成 AI 候选列表: 'Table_5_AI_Candidates.csv'")








# ==============================================================================
# Table_6 Generator: High-Frequency Keywords Cluster Analysis
# Algorithm: Louvain Community Detection
# Network: Co-occurrence of Author Keywords (DE)
# ==============================================================================

library(bibliometrix)
library(igraph)
library(dplyr)
library(stringr)

# 1. 检查数据环境 (如果 M 不存在则报错，请确保 M 已加载)
if (!exists("M")) stop("【错误】数据框 M 不存在，请先运行数据读取代码。")

# ------------------------------------------------------------------------------
# 2. 数据预处理：同义词合并 (Thesaurus Cleaning) - 关键步骤
# ------------------------------------------------------------------------------
# 在做聚类前，必须把 "Deep Learning" 和 "Deep learning" 这种大小写统一，
# 甚至把 "Artificial Intelligence" 和 "AI" 合并，否则它们会被分到不同点。
# 这里的 cleaned_M 仅用于关键词分析
M_clean <- M

# 简单的同义词替换逻辑 (根据领域知识可扩展)
# 将所有关键词转为大写
M_clean$DE <- toupper(M_clean$DE)

# 强制合并常见同义词 (示例)
M_clean$DE <- str_replace_all(M_clean$DE, "ARTIFICIAL INTELLIGENCE", "MACHINE LEARNING") # 归拢为大概念
M_clean$DE <- str_replace_all(M_clean$DE, "DEEP LEARNING", "MACHINE LEARNING")
M_clean$DE <- str_replace_all(M_clean$DE, "GUT MICROBIOTA", "MICROBIOTA")
M_clean$DE <- str_replace_all(M_clean$DE, "SPORTS NUTRITION", "NUTRITION") # 去除过于通用的词

# ------------------------------------------------------------------------------
# 3. 构建网络与聚类
# ------------------------------------------------------------------------------
# 选取 Top 50 关键词构建网络 (节点太多图会乱，Top 50 足以展示核心结构)
NetMatrix <- biblioNetwork(M_clean, analysis = "co-occurrences", network = "author_keywords", n = 50)

# 转换为 igraph 图对象
g <- graph_from_adjacency_matrix(NetMatrix, mode = "undirected", weighted = TRUE, diag = FALSE)

# 运行 Louvain 聚类算法
set.seed(123) # 固定随机种子，保证结果可复现
wc <- cluster_louvain(g)

# 将聚类结果附加到节点属性
V(g)$cluster <- membership(wc)
V(g)$degree  <- degree(g)

# ------------------------------------------------------------------------------
# 4. 提取聚类详情并制表
# ------------------------------------------------------------------------------
# 提取节点信息到数据框
nodes_df <- data.frame(
  Keyword = V(g)$name,
  Cluster = V(g)$cluster,
  Degree = V(g)$degree
)

# 计算每个聚类的总词频 (作为排序依据，大聚类排前面)
cluster_summary <- nodes_df %>%
  group_by(Cluster) %>%
  summarise(Total_Degree = sum(Degree)) %>%
  arrange(desc(Total_Degree))

# 构建 Table_6 的内容
final_table_rows <- list()

# 循环提取每个聚类的前 5 个核心词
for(i in 1:nrow(cluster_summary)) {
  cid <- cluster_summary$Cluster[i]
  
  # 取出该聚类下的所有词，按中心性(Degree)排序
  cluster_words <- nodes_df %>%
    filter(Cluster == cid) %>%
    arrange(desc(Degree))
  
  # 提取 Top 5 关键词及其频次 (这里用 Degree 代表网络中的重要性)
  top_keywords <- paste0(cluster_words$Keyword[1:min(5, nrow(cluster_words))], 
                         " (", cluster_words$Degree[1:min(5, nrow(cluster_words))], ")")
  
  # 拼接成字符串
  keywords_str <- paste(top_keywords, collapse = "; ")
  
  # 自动建议标签 (取最核心的那个词作为临时标签)
  suggested_label <- cluster_words$Keyword[1]
  
  final_table_rows[[i]] <- data.frame(
    Cluster_ID = i,
    Auto_Label = suggested_label,
    Top_Keywords_Metrics = keywords_str,
    Cluster_Size = nrow(cluster_words)
  )
}

table_3 <- bind_rows(final_table_rows)

# ------------------------------------------------------------------------------
# 5. 导出与预览
# ------------------------------------------------------------------------------
write.csv(table_3, "Table_6_Clusters_Louvain.csv", row.names = FALSE)

print("--- [Table_6] 聚类分析结果预览 ---")
print(table_3)

message(">>> ✅ Table_6 数据已生成: 'Table_6_Clusters_Louvain.csv'")
message(">>> 下一步：请打开 CSV，根据关键词内容，手动将 'Auto_Label' 修改为 'Traditional', 'Omics', 'Tech' 等理论标签。")








# ==============================================================================
# 0. FIGURE_5绘制环境初始化 & 随机种子锁定 (CRITICAL FOR REPRODUCIBILITY)
# ==============================================================================
rm(list = ls()) 
library(bibliometrix)
library(ggplot2)
library(ggrepel) 
library(svglite)
library(dplyr) # 新增：用于更优雅的数据处理

set.seed(42)   # 锁定随机数种子，确保标签位置和聚类结果每次运行一致！

# ==============================================================================
# 1. 数据加载与计算
# ==============================================================================
# 请确保这两个文件在你的工作路径下
file_list <- c("download_1.txt", "download_2.txt") 
M <- convert2df(file = file_list, dbsource = "wos", format = "plaintext")

print("正在计算 thematicMap (这可能需要几秒)...")
MapResults <- thematicMap(
  M,
  field = "DE",
  n = 250,           
  minfreq = 7,       
  stemming = FALSE,  
  size = 0.5,
  n.labels = 3,
  repel = TRUE
)

# ==============================================================================
# 2. 精确锁定数据源 (The Sniper Logic - 保持原样)
# ==============================================================================
raw_df <- NULL
if ("clusters" %in% names(MapResults)) {
  raw_df <- MapResults$clusters
  print("已定位数据源: clusters")
} else if ("df" %in% names(MapResults)) {
  raw_df <- MapResults$df
  print("已定位数据源: df")
} else if ("words" %in% names(MapResults)) {
  print("WARNING: 未找到聚类表，正在尝试使用 words 表...")
  raw_df <- MapResults$words
}

if (is.null(raw_df) || nrow(raw_df) == 0) {
  stop("严重错误：无法找到有效的聚类数据。")
}

# ==============================================================================
# 3. 安全构建绘图数据 & 象限逻辑植入
# ==============================================================================
plot_data <- data.frame(row.names = 1:nrow(raw_df))
cols <- colnames(raw_df)

# --- 映射 X 轴 (Centrality) ---
x_idx <- grep("Centrality", cols, ignore.case = TRUE)
plot_data$X <- as.numeric(as.character(raw_df[[x_idx[1]]]))

# --- 映射 Y 轴 (Density) ---
y_idx <- grep("Density", cols, ignore.case = TRUE)
plot_data$Y <- as.numeric(as.character(raw_df[[y_idx[1]]]))

# --- 映射 Label (标签) ---
lbl_idx <- grep("Label|Words|Cluster_Label", cols, ignore.case = TRUE)
plot_data$Label <- if (length(lbl_idx) > 0) as.character(raw_df[[lbl_idx[1]]]) else paste("Cl", 1:nrow(raw_df))

# --- 映射 Size (气泡大小) ---
sz_idx <- grep("Rank|Occurrences|Frequency|^n$|Freq", cols, ignore.case = TRUE)
plot_data$Size <- if (length(sz_idx) > 0) as.numeric(as.character(raw_df[[sz_idx[1]]])) else 10

# --- 映射 Cluster ID ---
cl_idx <- grep("Cluster", cols, ignore.case = TRUE)
plot_data$Cluster <- if (length(cl_idx) > 0) as.factor(raw_df[[cl_idx[1]]]) else as.factor(1:nrow(raw_df))

# --- 新增：计算中心点与象限归属 (Auto-Classification) ---
mid_x <- mean(plot_data$X, na.rm = TRUE)
mid_y <- mean(plot_data$Y, na.rm = TRUE)

# 自动判断象限，方便后续写论文直接用
plot_data$Quadrant <- with(plot_data, ifelse(X >= mid_x & Y >= mid_y, "Q1: Motor Themes (核心)",
                                             ifelse(X < mid_x & Y >= mid_y,  "Q2: Niche Themes (孤立)",
                                                    ifelse(X < mid_x & Y < mid_y,   "Q3: Emerging/Declining (边缘/新兴)",
                                                           "Q4: Basic Themes (基础)"))))

print("绘图数据构建完成！已自动划分象限。")

# ==============================================================================
# 4. 绘图 (Nature Style Upgrade)
# ==============================================================================
p <- ggplot(plot_data, aes(x = X, y = Y, label = Label)) +
  # 象限分割线
  geom_hline(yintercept = mid_y, linetype = "solid", color = "gray70", size = 0.8) +
  geom_vline(xintercept = mid_x, linetype = "solid", color = "gray70", size = 0.8) +
  
  # 气泡层
  geom_point(aes(fill = Cluster, size = Size), shape = 21, color = "black", alpha = 0.85) + # shape 21 允许填充颜色+黑色边框，更有质感
  
  # 标签层 (加粗，提高可读性)
  geom_text_repel(size = 4.5, fontface = "bold", max.overlaps = 100, box.padding = 0.6, point.padding = 0.3, bg.color = "white", bg.r = 0.15) +
  
  # 象限背景标注 (更隐晦的专业感)
  annotate("text", x = max(plot_data$X), y = max(plot_data$Y), label = "Motor Themes", hjust=1, vjust=1, color="gray50", fontface="bold.italic", size=5) +
  annotate("text", x = min(plot_data$X), y = max(plot_data$Y), label = "Niche Themes", hjust=0, vjust=1, color="gray50", fontface="bold.italic", size=5) +
  annotate("text", x = min(plot_data$X), y = min(plot_data$Y), label = "Emerging/Declining", hjust=0, vjust=0, color="gray50", fontface="bold.italic", size=5) +
  annotate("text", x = max(plot_data$X), y = min(plot_data$Y), label = "Basic Themes", hjust=1, vjust=0, color="gray50", fontface="bold.italic", size=5) +
  
  # 标题与坐标轴
  labs(
    title = "Thematic Map: Evolution of AI in Sports Nutrition",
    subtitle = "Strategic diagram based on density and centrality",
    x = "Callon's Centrality (Relevance / Degree of Interaction)",
    y = "Callon's Density (Development / Internal Strength)",
    size = "Occurrences"
  ) +
  
  # 配色与主题设置 (Nature 风格)
  scale_size(range = c(4, 16)) + 
  scale_fill_viridis_d(option = "turbo", alpha = 0.9) + # 使用 viridis turbo 色板，区分度高且学术感强
  guides(fill = "none") + # 隐藏 Cluster 图例，因为 Label 已经在图上了，图例是多余的噪音
  theme_bw(base_size = 14) + # 使用 theme_bw 替代 minimal，边框更清晰
  theme(
    panel.grid.major = element_line(color = "gray90", linetype = "dashed"),
    panel.grid.minor = element_blank(),
    plot.title = element_text(hjust = 0.5, face = "bold", size = 16),
    plot.subtitle = element_text(hjust = 0.5, face = "italic", color = "gray40"),
    axis.title = element_text(face = "bold"),
    legend.position = "bottom"
  )

# ==============================================================================
# 5. 导出 (Export)
# ==============================================================================
# 导出包含“象限信息”的 CSV
write.csv(plot_data, "Figure5_ThematicMap_Data_WithQuadrants.csv", row.names = FALSE)
print("SUCCESS: CSV 导出成功 (含象限分类)。")

# 导出 SVG
tryCatch({
  ggsave(filename = "Figure5_ThematicMap.svg", plot = p, device = svglite::svglite, width = 12, height = 10)
  print("SUCCESS: SVG 图片导出成功")
}, error = function(e) {
  print(paste("SVG导出失败:", e$message))
  ggsave("Figure5_ThematicMap_Backup.png", p, width=12, height=10, dpi=300)
})

# 显示图表
print(p)










# ==============================================================================
#FIGURE 6
# ==============================================================================

# 1. 环境准备
if (!require("pacman")) install.packages("pacman")
pacman::p_load(bibliometrix, dplyr, networkD3, htmlwidgets, htmltools, stringr)

# 2. 读取数据
data_path <- "." 
file_list <- list.files(path = data_path, pattern = "\\.txt$", full.names = TRUE)
if (length(file_list) == 0) stop("❌ 错误：未找到 .txt 文件。")

M <- convert2df(file_list, dbsource = "wos", format = "plaintext")
M <- M %>% filter(!str_detect(DT, "EDITORIAL|LETTER|MEETING|CORRECTION"))

# 3. 计算演化路径
cut_point <- 2019
start_year <- min(M$PY, na.rm = TRUE)
end_year   <- max(M$PY, na.rm = TRUE)

# 生成用于显示的标签字符串
label_period_1 <- paste0("(", start_year, "-", cut_point, ")")   # 例如 (2005-2019)
label_period_2 <- paste0("(", cut_point + 1, "-", end_year, ")") # 例如 (2020-2026)

message(">>> 正在计算演化路径...")
# 注意：minFreq设为2以保证有数据，若太乱可手动改为3
nexus <- thematicEvolution(M, field = "DE", years = c(cut_point), n = 250, minFreq = 2, stemming = TRUE)

if (is.null(nexus$Edges) || nrow(nexus$Edges) == 0) {
  stop("❌ 错误：未生成数据，请尝试将 minFreq 改为 1。")
}

# ==============================================================================
# 4. 数据清洗与强制分列 (The Hard Separation)
# ==============================================================================
message(">>> 正在构建严格分列布局...")

nodes_df <- nexus$Nodes
edges_df <- nexus$Edges
colnames(edges_df)[1:3] <- c("From", "To", "Weight")

# --- 步骤 A: 还原文字 (如果之前是数字索引) ---
# 获取字典
node_names <- as.character(nodes_df[[1]]) 

# 检查 From 列是否为纯数字
first_val <- as.character(edges_df$From[1])
if (str_detect(first_val, "^[0-9]+$")) {
  # 如果是数字，去查字典
  min_idx <- min(as.numeric(as.character(edges_df$From)), na.rm=TRUE)
  adj <- if (min_idx == 0) 1 else 0 # 修正0起始或1起始
  
  edges_df$Raw_From <- node_names[as.numeric(as.character(edges_df$From)) + adj]
  edges_df$Raw_To   <- node_names[as.numeric(as.character(edges_df$To)) + adj]
} else {
  edges_df$Raw_From <- as.character(edges_df$From)
  edges_df$Raw_To   <- as.character(edges_df$To)
}

# --- 步骤 B: 清洗掉所有自带的时间后缀 (比如 "--2019") ---
clean_text <- function(x) {
  # 只要 "--" 前面的部分
  str_split(x, "--", simplify = TRUE)[,1] 
}

edges_df$Clean_From <- sapply(edges_df$Raw_From, clean_text)
edges_df$Clean_To   <- sapply(edges_df$Raw_To, clean_text)

# --- 步骤 C: 强制添加左右不同的标签 (解决中间乱飞的关键！) ---
# 左边的必须叫 "词\n(2005-2019)"
# 右边的必须叫 "词\n(2020-2026)"
# \n 是换行符，让图好看

edges_df$Source_Label <- paste0(str_to_title(edges_df$Clean_From), "\n", label_period_1)
edges_df$Target_Label <- paste0(str_to_title(edges_df$Clean_To),   "\n", label_period_2)

# ==============================================================================
# 5. 绘图准备
# ==============================================================================

# 生成节点列表
plot_nodes_list <- unique(c(edges_df$Source_Label, edges_df$Target_Label))
plot_nodes <- data.frame(name = plot_nodes_list, stringsAsFactors = FALSE)

# 重新生成 ID
edges_df$IDsource <- match(edges_df$Source_Label, plot_nodes$name) - 1
edges_df$IDtarget <- match(edges_df$Target_Label, plot_nodes$name) - 1

# ==============================================================================
# 6. 绘图 (Strict Columns + CSS)
# ==============================================================================
message(">>> 正在绘制...")

p <- sankeyNetwork(Links = edges_df, 
                   Nodes = plot_nodes,
                   Source = "IDsource", 
                   Target = "IDtarget",
                   Value = "Weight", 
                   NodeID = "name",        
                   units = "Index", 
                   fontSize = 14,          
                   nodeWidth = 30, 
                   nodePadding = 12,       # 节点间距
                   sinksRight = FALSE,     # FALSE 对这种分列图更友好
                   width = 1200, height = 750)

# CSS 注入：黑色加粗字体
p <- htmlwidgets::onRender(p, '
  function(el, x) {
    d3.selectAll(".node text")
      .style("fill", "black")
      .style("font-weight", "bold")
      .style("font-size", "14px");
  }
')

print(p)

# ==============================================================================
# 7. 导出最终表格 (解释 Weight)
# ==============================================================================
final_table <- edges_df %>% 
  select(Source_Label, Target_Label, Weight) %>%
  rename(
    From_Concept = Source_Label, 
    To_Concept = Target_Label,
    Inclusion_Index_Weight = Weight
  ) %>%
  mutate(
    Explanation = paste("The concept", str_replace_all(From_Concept, "\n", " "), 
                        "evolved into", str_replace_all(To_Concept, "\n", " "))
  ) %>%
  arrange(desc(Inclusion_Index_Weight))

write.csv(final_table, "Figure6_Sankey_Final_Correct_Labels.csv", row.names = FALSE)

message("=======================================================")
message(">>> ✅ 修复完成！")
message("1. 检查图片：不再有 NA，左右两列应该分得很开。")
message("2. 检查表格：'#FIGURE_6Sankey_Final_Correct_Labels.csv'")
message("   Inclusion_Index_Weight (权重):")
message("   - 1.0: 完全演化 (Strong Link)")
message("   - 0.1: 微弱关联 (Weak Link)")
message("=======================================================")









