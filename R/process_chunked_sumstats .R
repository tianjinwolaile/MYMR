#' @title 根据chr 和pos 补充snp
#' @description 处理分块读取的VCF.gz文件，提取关键统计信息并保存为TwoSampleMR兼容的CSV.gz格式
#' @param outcome 输入outcome文件路径
#' @param chunk_size 分块读取行数（默认10000）
#' @param dbSNP dbSNP版本（默认"144"，当前仅占位）
#' @param ref_genome 参考基因组版本（默认"GRCh37"，当前仅占位）
#' @param save_path 输出CSV.gz文件路径（默认"out.csv.gz"）
#' @return 无返回值，结果保存至指定文件
#' @export
#'
#' @examples
#' # 执行分块处理
#' # outcome=fread("outcome_chr_pos.csv")
#'# colnames(outcome)
#'# outcome=outcome[,c(16,17,2,3,10,8,9)]
#'# head(outcome)
#'# process_chunked_sumstats(
#'#   outcome = outcome,chunk_size = 10^6,
#'#   dbSNP = "144",
#'#   ref_genome = "GRCh37",
#'#   save_path = "out.csv.gz"  # 输出到results/sumstats_part1.csv.gz等
#'# )

process_chunked_sumstats <- function(outcome, chunk_size = 10000, dbSNP = "144", ref_genome = "GRCh37", save_path= "out.csv.gz") {
  # 计算总行数和分块数量
  if(dir.exists("./results/")){
    dir.create("./results/")
  }
  total_rows <- nrow(outcome)
  num_chunks <- ceiling(total_rows / chunk_size)
  
  # 循环处理每个分块
  for (i in 1:num_chunks) {
    # 计算当前块的起止位置
    start_row <- (i - 1) * chunk_size + 1
    end_row <- min(i * chunk_size, total_rows)
    
    # 提取当前块的数据
    chunk_data <- outcome[start_row:end_row, ]
    
    # 动态生成保存路径 (格式：base_path_partN.csv.gz)
    save_path_i <- paste0("./result/",i,".csv.gz")
    
    # 处理当前分块
    format_sumstats(
      chunk_data,
      dbSNP = dbSNP,
      ref_genome = ref_genome,
      save_path = save_path_i
    )
    
    # 可选：打印进度信息
    message(sprintf("Processed chunk %d/%d: rows %d-%d -> %s",
                    i, num_chunks, start_row, end_row, save_path_i))
  }
  
  message("All chunks processed successfully!")
  gc()
  # 读取所有分块文件并合并
  library(data.table)
  all_files <- list.files("./result/",pattern = ".*\\.csv\\.gz",full.names = TRUE)
  full_data <- rbindlist(lapply(all_files, fread))
  fwrite(full_data,save_path)
  gc()
}