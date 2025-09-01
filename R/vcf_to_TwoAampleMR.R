#' @title  snp_add_exposure_eaf ,根据snp补全全eaf值
#' @param dat the data of TwoSampleMR
#' @param build 37 or 38
#' @param pop 地区
#' @import igraph
#' @import tidyverse
#' @return
#' @export
#' @examples
snp_add_exposure_eaf <- function(dat, build = "37", pop = "EUR")
{
  stopifnot(build %in% c("37","38"))
  stopifnot("SNP" %in% names(dat))
  
  # Create and get a url
  server <- ifelse(build == "37","http://grch37.rest.ensembl.org","http://rest.ensembl.org")
  pop <- paste0("1000GENOMES:phase_3:",pop)
  
  snp_reverse_base <- function(x)
  {
    x <- str_to_upper(x)
    stopifnot(x %in% c("A","T","C","G"))
    switch(x,"A"="T","T"="A","C"="G","G"="C")
  }
  
  res_tab <- lapply(1:nrow(dat), function(i)
  {
    print(paste0("seaching for No.", i, " SNP"))
    dat_i <- dat[i,]
    
    ext <- paste0("/variation/Homo_sapiens/",dat_i$SNP, "?content-type=application/json;pops=1")
    url <- paste(server, ext, sep = "")
    res <- httr::GET(url)
    
    # Converts http errors to R errors or warnings
    httr::stop_for_status(res)
    
    # Convert R objects from JSON
    res <- httr::content(res)
    res_pop <- jsonlite::fromJSON(jsonlite::toJSON(res))$populations
    
    # Filter query results based on population set
    res_pop <- try(res_pop[res_pop$population == pop,])
    if("try-error" %in% class(res_pop))
    {
      print(paste0("There is not information for population ",pop))
      queried_effect_allele <- "NR"
      queried_other_allele <- "NR"
      queried_eaf <- -1
    }else{
      if(nrow(res_pop)==0)
      {
        print(paste0("There is not information for population ",pop))
        queried_effect_allele <- "NR"
        queried_other_allele <- "NR"
        queried_eaf <- -1
      }else{
        queried_effect_allele <- res_pop[1,"allele"][[1]]
        queried_other_allele <- res_pop[2,"allele"][[1]]
        queried_eaf <- res_pop[1,"frequency"][[1]]    
      }
    }
    
    effect_allele <- ifelse("effect_allele.exposure" %in% names(dat),
                            dat_i$effect_allele.exposure,
                            dat_i$effect_allele)
    
    other_allele <- ifelse("effect_allele.exposure" %in% names(dat),
                           dat_i$other_allele.exposure,
                           dat_i$other_allele)
    
    if("effect_allele.exposure" %in% names(dat)){
      name_output <- unique(c(names(dat), "eaf.exposure","reliability.exposure"))
    }else{
      name_output <- unique(c(names(dat), "eaf","reliability.exposure"))
    }
    
    len_effect_allele <- nchar(effect_allele)
    len_other_allele <- nchar(other_allele)
    
    if(len_effect_allele==1&len_other_allele==1)
    {
      if((queried_effect_allele==effect_allele & queried_other_allele==other_allele)|
         (queried_effect_allele==other_allele & queried_other_allele==effect_allele))
      {
        dat_i$eaf.exposure <- ifelse(effect_allele == queried_effect_allele,
                                     queried_eaf,
                                     1-queried_eaf)
        dat_i$eaf <- dat_i$eaf.exposure 
        dat_i$reliability.exposure <- "high"
      }else{
        r_queried_effect_allele <- snp_reverse_base(queried_effect_allele)
        r_queried_other_allele <- snp_reverse_base(queried_other_allele)
        if((r_queried_effect_allele==effect_allele & r_queried_other_allele==other_allele)|
           (r_queried_effect_allele==other_allele & r_queried_other_allele==effect_allele))
        {
          dat_i$eaf.exposure <- ifelse(effect_allele == r_queried_effect_allele,
                                       queried_eaf,
                                       1-queried_eaf)
          dat_i$eaf <- dat_i$eaf.exposure 
          dat_i$reliability.exposure <- "high"
        }else{
          dat_i$eaf.exposure <- ifelse(effect_allele == queried_effect_allele,
                                       queried_eaf,
                                       1-queried_eaf)
          dat_i$eaf <- dat_i$eaf.exposure 
          dat_i$reliability.exposure <- "low"
        }
      }
    }else{
      # To identify the potential DEL/ INS
      short_allele <- ifelse(len_effect_allele==1,
                             effect_allele,
                             other_allele)
      short_allele_eaf <- ifelse(short_allele == queried_effect_allele, 
                                 queried_eaf, 
                                 1-queried_eaf)
      dat_i$eaf.exposure <- ifelse(effect_allele == short_allele,
                                   short_allele_eaf,
                                   1-short_allele_eaf)
      dat_i$eaf <- dat_i$eaf.exposure 
      dat_i$reliability.exposure <- "low"
    }
    
    dat_i[name_output]
  })
  
  return(do.call(rbind, res_tab))
}


#' @title  SNP2eaf
#'
#' @param dat data of TwoSamplMR
#' @param population 地区
#' @param build 37 or 38
#'
#' @import tidyverse
#' @return
#' @export

#' @examples
SNP2eaf<- function(dat,population= c("gnomADg:nfe","gnomADg:ami"  ,          
                                     "gnomADg:amr","gnomADg:asj"     ,       
                                     "gnomADg:afr","gnomADg:mid"     ,       
                                     "gnomADg:sas","gnomADg:ALL"      ,      
                                     "gnomADg:remaining"   ,    "gnomADg:eas"   ,         
                                     "gnomADg:fin","ALFA:SAMN10492703",    
                                     "ALFA:SAMN10492705","ALFA:SAMN10492695",      
                                     "ALFA:SAMN10492696","ALFA:SAMN10492702"   ,   
                                     "ALFA:SAMN10492700","ALFA:SAMN11605645"   ,   
                                     "ALFA:SAMN10492698","ALFA:SAMN10492699" ,     
                                     "GEM-J", "TOPMed",    
                                     "1000GENOMES:phase_3:ACB","1000GENOMES:phase_3:AFR",
                                     "1000GENOMES:phase_3:ALL","1000GENOMES:phase_3:AMR",
                                     "1000GENOMES:phase_3:ASW","1000GENOMES:phase_3:BEB",
                                     "1000GENOMES:phase_3:CDX","1000GENOMES:phase_3:CEU",
                                     "1000GENOMES:phase_3:CHB","1000GENOMES:phase_3:CHS",
                                     "1000GENOMES:phase_3:CLM","1000GENOMES:phase_3:EAS",
                                     "1000GENOMES:phase_3:ESN","1000GENOMES:phase_3:EUR",
                                     "1000GENOMES:phase_3:FIN","1000GENOMES:phase_3:GBR",
                                     "1000GENOMES:phase_3:GIH","1000GENOMES:phase_3:GWD",
                                     "1000GENOMES:phase_3:IBS","1000GENOMES:phase_3:ITU",
                                     "1000GENOMES:phase_3:JPT","1000GENOMES:phase_3:KHV",
                                     "1000GENOMES:phase_3:LWK","1000GENOMES:phase_3:MSL",
                                     "1000GENOMES:phase_3:MXL","1000GENOMES:phase_3:PEL",
                                     "1000GENOMES:phase_3:PJL","1000GENOMES:phase_3:PUR",
                                     "1000GENOMES:phase_3:SAS","1000GENOMES:phase_3:STU",
                                     "1000GENOMES:phase_3:TSI","1000GENOMES:phase_3:YRI",
                                     "GGVP:GWF",   "GGVP:GWD",  
                                     "GGVP:GWW",   "GGVP:GWJ",  
                                     "GGVP:ALL")[36],build = "37"){
  dat[,c("maf.exposure","freq.effect_allele.exposure","freq.other_allele.exposure")]=NA
  for(i in 1:nrow(dat)){
    # 打印进度信息
    if(i%%10==0){print(paste0(i, " SNP is finished!"))}
    # 获取SNP的有效等位基因
    snpID=dat[i,"SNP"]
    localEffectAllele=dat[i,"effect_allele.exposure"]
    localOtherAllele=dat[i,"other_allele.exposure"]
    # 从数据库获取SNP信息
    server <- ifelse(build == build,"http://grch37.rest.ensembl.org","http://rest.ensembl.org")
    website=paste0(server,"/variation/Homo_sapiens/", snpID, "?content-type=application/json;pops=1",sep="")
    info=httr::content(httr::GET(website))
    snpInfo=jsonlite::fromJSON(jsonlite::toJSON(info))$populations
    snpInfo=snpInfo[snpInfo$population==population,]
    webEffectAllele=ifelse(is.null(snpInfo[1,"allele"][[1]]), "unknow", snpInfo[1,"allele"][[1]])
    webOtherAllele=ifelse(is.null(snpInfo[2,"allele"][[1]]), "unknow", snpInfo[2,"allele"][[1]])
    webEaf=snpInfo[1,"frequency"][[1]]
    # 根据位置信息获取eaf
    if((webEffectAllele==localEffectAllele) & (webOtherAllele==localOtherAllele)){
      dat$eaf.exposure[i]=webEaf
    }else if((webEffectAllele==localOtherAllele) & (webOtherAllele==localEffectAllele)){
      dat$eaf.exposure[i]=1-webEaf
    }else{
      revWebEffectAllele=chartr("CGAT", "GCTA", webEffectAllele)
      revWebOtherAllele=chartr("CGAT", "GCTA", webOtherAllele)
      if((revWebEffectAllele==localEffectAllele) & (revWebOtherAllele==localOtherAllele)){
        dat$eaf.exposure[i]=webEaf
      }else if((revWebEffectAllele==localOtherAllele) & (revWebOtherAllele==localEffectAllele)){
        dat$eaf.exposure[i]=1-webEaf
      }else{
        dat$eaf.exposure[i]=ifelse(localEffectAllele==webEffectAllele, webEaf, 1-webEaf)
      }
    }	
    dat$maf.exposure[i]=ifelse(!is.null( info$MAF),info$MAF,NA)
    dat$freq.effect_allele.exposure[i]=ifelse(!is.null( snpInfo$frequency[1][[1]]),snpInfo$frequency[1],NA)
    dat$freq.other_allele.exposure[i]=ifelse(!is.null( snpInfo$frequency[2][[1]]),snpInfo$frequency[2],NA)
  }
  return(dat)
}
#' @title vcf_to_TwosampleMR
#'
#' @param vcf vcf.gz文件路径
#' @param type exposure or outcome
#'
#' @import vcfR
#' @import dplyr
#' @export

#' @examples 
#' #vcf_to_TwosampleMR(vcf=system.file("data/eqtl-a-ENSG00000112715.vcf.gz",package = "MYMR"),
#' #type="exposure")
vcf_to_TwosampleMR=function(vcf='E:\\wd\\MR\\data\\ebi-a-GCST001475.vcf.gz',type="exposure"){
  library(vcfR)
  library(dplyr)
  arData <- vcfR::read.vcfR(vcf,) #读取vcf文件
  # str(arData) #查看vcf数据的结构
  # head(arData@meta,12)  #查看注释信息
  # 
  # head(arData@fix)
  # head(arData@gt)
  # arFix <- as.data.frame(arData@fix[,(1:5)]) #提取arData@fix的前5列
  arGt <- as.data.frame(arData@gt[,1:2]) #提取arData@gt的第一列
  names=unique(arGt$FORMAT)
  # rm(arData)
  # result_names=names[which(max(str_length(names))==str_length(names))]
  # result_names=stringr::str_split(result_names,":")[[1]]
  # result_names <- stringr::str_split(names[which.max(nchar(names))], ":")[[1]]
  result_names <- c("ES","SE","AF","LP","SS","NC","ID")#stringr::str_split(names[which.max(nchar(names))], ":")[[1]]
  
  data_list=list()
  for(i in unique(arGt$FORMAT)){
    gc()
    data_list[[i]]=data.frame(data.table::tstrsplit(arGt[,2][arGt[,1]==i],":"))
    colnames(data_list[[i]])=stringr::str_split(i,":")[[1]]
    data_list[[i]][,result_names[!(result_names%in%colnames(data_list[[i]]))]]=NA
    data_list[[i]]=dplyr::select(data_list[[i]],result_names)
  }
  result=dplyr::bind_rows(data_list)
  result[["LP"]] <- 10^-as.numeric(result[["LP"]])
  # result[["NCONT"]] <- result[["SS"]] - result[["NC"]]
  id=sub(".vcf\\.gz$", "", basename(vcf))
  info=ieugwasr::gwasinfo(id)
  result[["id"]]=id
  if(all(c("ncontrol","sample_size","ncase")%in%colnames(info))){
    result[["NCONT"]]=info$ncontrol
    result[["SS"]]=info$sample_size
    result[["NC"]]=info$ncase
  }else{
    if(is.null(info$ncontrol)){result[["NCONT"]]=NA}
    if(is.null(info$sample_size)){
      result[["SS"]]=info$sample_size
    }else{
      result[["SS"]]=info$sample_size
    }
    if(is.null(info$ncase)){result[["NC"]]=NA}
  }
  result=cbind(as.data.frame(arData@fix[,(1:5)])[,-3], result)#使用dplyr::inner_join会数量减少
  out=TwoSampleMR::format_data(result, type =type, snp_col = "ID",
                               effect_allele_col = "ALT", other_allele_col = "REF",
                               eaf_col = "AF", chr_col = "CHROM", pos_col = "POS",
                               beta_col = "ES", se_col = "SE", pval_col = "LP", samplesize_col = "SS",
                               ncase_col = "NC", ncontrol_col = "NCONT",id_col = "id")
  if (is.null(info$ncontrol)&is.null(info$ncase)) {
    out=out[,-c(10,13)]
  }
  rm(arData,arGt,data_list,result)
  return(out)
  rm(out)
  gc()
}
