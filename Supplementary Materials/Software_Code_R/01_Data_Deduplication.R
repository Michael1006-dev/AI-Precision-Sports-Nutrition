# --- 运动科学与文献计量学专用：WoS原始文本文件去重与重构脚本 ---

# 设置工作目录 (请确保包含 download_1.txt 到 download_4.txt)
# setwd("你的数据目录路径") 

# 1. 定义文件列表
file_list <- c("download_1.txt", "download_2.txt")

# 创建输出目录，保持原数据纯净
if(!dir.exists("cleaned_data")) dir.create("cleaned_data")

# 用于存储所有文章元数据的临时容器
all_records_meta <- data.frame(
  file_origin = character(),
  title = character(),
  author = character(),
  full_text_block = I(list()), # 存储整段原始文本
  stringsAsFactors = FALSE
)

cat("正在解析原始文件...\n")

# 2. 遍历读取并解析文件 (模拟 CiteSpace 的读取逻辑)
for (f in file_list) {
  # 读取每一行，不做任何编码转换以防乱码
  lines <- readLines(f, warn = FALSE)
  
  # 识别每篇文章的结束标记 "ER"
  # WoS 格式：每篇文章以 PT 开始，以 ER 结束（空行分隔）
  end_indices <- grep("^ER", lines)
  start_indices <- grep("^PT", lines)
  
  if(length(start_indices) != length(end_indices)) {
    warning(paste("文件", f, "结构可能损坏，PT与ER数量不匹配"))
    next
  }
  
  for (i in 1:length(start_indices)) {
    # 提取单篇文章的完整文本块
    block <- lines[start_indices[i]:end_indices[i]]
    
    # 提取标题 (TI) - 仅用于比对
    ti_line_idx <- grep("^TI ", block)
    # 处理多行标题的情况 (简化处理：取TI行，若无则标记Unknown)
    if(length(ti_line_idx) > 0) {
      # 简单的提取逻辑，通常TI在同一行或紧接其后，这里取第一行即可满足去重
      title_clean <- gsub("^TI ", "", block[ti_line_idx[1]])
    } else {
      title_clean <- "UNKNOWN_TITLE"
    }
    
    # 提取作者 (AU) - 仅用于打印日志
    au_line_idx <- grep("^AU ", block)
    if(length(au_line_idx) > 0) {
      author_clean <- gsub("^AU ", "", block[au_line_idx[1]])
    } else {
      author_clean <- "Unknown Author"
    }
    
    # 存入临时列表
    all_records_meta <- rbind(all_records_meta, data.frame(
      file_origin = f,
      title = toupper(trimws(title_clean)), #以此为唯一指纹
      author = author_clean,
      full_text_block = I(list(block)),
      stringsAsFactors = FALSE
    ))
  }
}

# 3. 识别重复项逻辑
# duplicated 返回的是第二次及之后出现的项（即需要删除的项）
dup_mask <- duplicated(all_records_meta$title)
indices_to_remove <- which(dup_mask)

cat("\n--- 重复项处理日志 (Duplicate Removal Log) ---\n")

if(length(indices_to_remove) > 0) {
  for (idx in indices_to_remove) {
    # 获取此重复项的信息
    dup_record <- all_records_meta[idx, ]
    
    # 找到它第一次出现的那个“原件”
    # match 返回第一个匹配项的索引
    original_idx <- match(dup_record$title, all_records_meta$title)
    original_record <- all_records_meta[original_idx, ]
    
    # 打印要求的特定格式
    cat(sprintf("'%s' 文件和 '%s' 文件中的论文重复，\n   题目: %s\n   作者: %s，\n   故将 '%s' 文件中的一项剔除。\n\n",
                original_record$file_origin, 
                dup_record$file_origin,
                dup_record$title,
                dup_record$author,
                dup_record$file_origin))
  }
} else {
  cat("恭喜！未发现任何重复文献。\n")
}

# 4. 执行剔除并写回文件
# 保留非重复项
cleaned_records <- all_records_meta[!dup_mask, ]

cat("--- 正在重构并写入清洗后的文件 ---\n")

# 按源文件名分组写回，确保文件结构不变
unique_files <- unique(cleaned_records$file_origin)

for (f in unique_files) {
  # 找出该文件原本包含且未被剔除的记录
  records_in_this_file <- cleaned_records[cleaned_records$file_origin == f, ]
  
  # 构建输出路径
  out_path <- file.path("cleaned_data", f)
  
  # 重建 WoS 文件头
  # 注意：这里我们简化处理，直接用 FN Thomson Reuters VR 1.0 加上内容
  # 更好的方式是读取原文件的头两行，但标准 WoS 格式如下通常通用：
  file_content <- c("FN Thomson Reuters Web of Science™", "VR 1.0")
  
  # 逐条加入文章块
  for (k in 1:nrow(records_in_this_file)) {
    file_content <- c(file_content, "", records_in_this_file$full_text_block[[k]])
  }
  
  # 加入文件尾
  file_content <- c(file_content, "EF")
  
  # 写入
  writeLines(file_content, out_path)
  cat(sprintf("已生成: %s (含 %d 篇文章)\n", out_path, nrow(records_in_this_file)))
}

cat("\n--- 处理完成 ---\n")
cat("原始数据目录: 当前目录\n")
cat("清洗后数据目录: /cleaned_data (请在后续分析中使用此文件夹)\n")