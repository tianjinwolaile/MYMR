# MYMR
## get_freq_from_1000G函数介绍，需要提供plink软件路径，以及使用的人群基因组本地数据，以及输出的frq文件路径
## get_eaf_from_1000G函数介绍，这里需要提供一个TwoSampleMR格式的数据，然后还需要提供一个.frq文件，该文件由get_freq_from_1000G函数计算得到

maffile_path="E:\\wd\\MR\\ld_clump_local\\bfile\\"
plink_path = "E:\\wd\\KJ\\dxw\\step3\\step3_1\\plink_win64_20231018\\plink.exe"
bfile_path = "E:\\wd\\MR\\ld_clump_local\\bfile\\EUR\\AMR" # 欧洲人群基因组本地数据
maffile_path="E:\\wd\\MR\\ld_clump_local\\bfile\\"#fileFrequency.frq文件路径
get_freq_from_1000G(plink_path,bfile_path,maffile_path)
exp1=get_eaf_from_1000G(exp,path = "E:\\wd\\MR\\ld_clump_local\\bfile\\EUR_freq.frq")

## process_chunked_sumstats函数介绍，这里需要提供的数据列名包含chr	pos两列，由于内存限制，这里采用分批转换，可根据内存大小自行调整chunk_size自行调整，dbSNP = "144"，目前到155版本，ref_genome可选GRCh37或者GRCh38，可自行设置输出名字

outcome=fread("outcome_chr_pos.csv")
colnames(outcome)
outcome=outcome[,c(16,17,2,3,10,8,9)]
head(outcome)
process_chunked_sumstats(
outcome = outcome,chunk_size = 10^6,
dbSNP = "144",
ref_genome = "GRCh37",
save_path = "out.csv.gz"  # 输出到results/sumstats_part1.csv.gz等
)
