# Library

  ## Data Reading
  library(readr)       
  library(readxl)
  
  ## Data Wrangling
  library(lubridate)
  library(scales)
  library(dplyr)
  library(tidyr)
  
  ## Data Visualization
  library(ggplot2)
  library(ggrepel)
  library(GGally)
  library(gridExtra)
  library(ggpubr)
  library(sf)      
  library(tigris)
  
  ## Data Segmentation
  library(cluster)
  library(fpc)
  library(factoextra)
  library(clusterSim)

# RFMT Terhadap Pelanggan

  ## Import Data Order
  order <- read_csv("Data/Order_Data_May21_Apr23.csv")
  order <- order %>% filter(Status_Terakhir=="Pesanan Selesai")
  
  initial_dataset <- order %>% 
    dplyr::select(Nomor_Invoice
                  ,Nama_Pembeli
                  ,Nama_Produk
                  ,Tanggal_Pembayaran 
                  ,Jumlah_Produk_Dibeli
                  ,`Harga_Jual_(IDR)`
                  ,Provinsi)
  
  ## Struktur Data
  str(initial_dataset)
  initial_dataset$Tanggal_Pembayaran <- dmy_hms(initial_dataset$Tanggal_Pembayaran) # yang harus diperbaiki yaitu Tanggal_Pembayaran aja
  initial_dataset <- initial_dataset %>% mutate(tanggal = as.Date(Tanggal_Pembayaran) #variabel baru untuk tanggal, bulan, dan tahun
                                                ,tgl  = day(Tanggal_Pembayaran)
                                                ,bln = month(Tanggal_Pembayaran,label = TRUE)
                                                ,thn = year(Tanggal_Pembayaran)
                                                ,bln_thn = format(tanggal, "%Y-%m"))
  ## check NA values
  colSums(is.na(initial_dataset))

  ## check duplicate data (keseluruhan)
  check.duplicates <- data.frame(
    row_of_data        = initial_dataset %>% nrow(),
    row_of_unique_data = initial_dataset %>% distinct() %>% nrow())

  ## check unique order,customer,and product
  unique_value <- data.frame(
    row_of_data          = initial_dataset %>% nrow(),
    row_of_unique_order  = initial_dataset$Nomor_Invoice %>% unique() %>% length(),
    row_of_unique_cust   = initial_dataset$Nama_Pembeli %>% unique() %>% length(),
    row_of_unique_produk = initial_dataset$Nama_Produk %>% unique() %>% length(),
    row_of_unique_prov   = initial_dataset$Provinsi %>% unique() %>% length())

  ## EDA
  unique_df <- initial_dataset %>% 
    group_by(bln_thn) %>% 
    summarise(unique_cust = length(unique(Nama_Pembeli))
              ,unique_order = length(unique(Nomor_Invoice))
              ,unique_produk = length(unique(Nama_Produk))
              ,total_penjualan = sum(Jumlah_Produk_Dibeli*`Harga_Jual_(IDR)`))
  
  unique_df %>% 
    ggplot(aes(x=bln_thn, y=unique_cust, group=1)) +
    geom_bar(fill = "#D57E7E", stat = "identity", color = "black") +
    geom_text(aes(label=unique_cust), vjust = -0.25) +
    theme_get() +
    theme(axis.text.x= element_text(angle = 45)) +
    labs(title = "Total Customer Per Bulan")
  
  unique_df %>% 
    ggplot(aes(x=bln_thn, y=unique_order, group=1)) +
    geom_bar(fill = "#A2CDCD", stat = "identity", color = "black") +
    geom_text(aes(label=unique_order), vjust = -0.25) +
    theme_get() +
    theme(axis.text.x= element_text(angle = 45)) +
    labs(title = "Total Order Per Bulan")
  
  
  pelanggan_baru <- initial_dataset %>% 
    group_by(Nama_Pembeli) %>% 
    summarise(urutan = min(tanggal)) %>% 
    mutate(yearmonth = substr(urutan,1,7)) %>%
    group_by(yearmonth) %>% 
    summarise(pelangganbaru = n())
  
  pelanggan_baru %>% 
    ggplot(aes(x=yearmonth, y=pelangganbaru, group = 1)) +
    geom_bar(fill = "#F7D060", stat = "identity", color = "black") +
    geom_text(aes(label=pelangganbaru), vjust = -0.25) +
    theme_get() +
    theme(axis.text.x= element_text(angle = 45)) +
    labs(title = "Pertumbuhan Customer Baru Per Bulan"
         ,x = "Bulan"
         ,y = "Pelanggan Baru")
  
  ## Transformasi RFMT Pelanggan
  analysis_date <- date(max(initial_dataset$Tanggal_Pembayaran))+days(1)
  
  df_rfmt <- initial_dataset %>%
    group_by(Nama_Pembeli) %>% 
    summarise(first_purchase = min(tanggal),
              last_purchase = max(tanggal),
              recency = as.numeric((analysis_date-last_purchase)),
              monetary = sum(Jumlah_Produk_Dibeli*`Harga_Jual_(IDR)`)) %>% 
    ungroup()
  
  df_rfmt <- initial_dataset %>% 
    dplyr::select(Nama_Pembeli, Nomor_Invoice) %>% 
    distinct() %>% 
    group_by(Nama_Pembeli) %>% 
    summarise(freq = n()) %>% 
    ungroup() %>% 
    right_join(df_rfmt, by = "Nama_Pembeli")
  
  df_rfmt <- df_rfmt %>% 
    mutate(time = round(as.numeric((last_purchase- first_purchase)/freq)),3) %>% 
    dplyr::select(Nama_Pembeli
                  ,first_purchase
                  ,last_purchase
                  ,recency
                  ,freq
                  ,monetary
                  ,time)
  
  ## Outliers Detection
  box1 <- df_rfmt %>% 
    ggplot(aes(y = recency)) + 
    geom_boxplot(fill = "#C6D57E", show.legend = FALSE) +
    theme_bw() +
    labs(title="Recency")
  
  box2 <- df_rfmt %>% 
    ggplot(aes(y = freq)) + 
    geom_boxplot(fill = "#D57E7E", show.legend = FALSE) +
    theme_bw() +
    labs(title="Frequency")
  
  box3 <- df_rfmt %>% 
    ggplot(aes(y = monetary)) + 
    geom_boxplot(fill = "#A2CDCD", show.legend = FALSE) +
    theme_bw() +
    labs(title="Monetary")
  
  box4 <- df_rfmt %>% 
    ggplot(aes(y = time)) + 
    geom_boxplot(fill = "#F7D060", show.legend = FALSE) +
    theme_bw() +
    labs(title="Time")
  
  fig1 <- ggarrange(box1,box2,box3,box4,
                    ncol=2,nrow=2) 
  
  annotate_figure(fig1, top = text_grob("Outlier RFMT Terhadap Pelanggan", face = "bold", size = 14))
  
  outliers <- function(x) {
    
    Q <- quantile(x, probs=0.95)
    
    x > Q
  }
  
  remove_outliers <- function(df, cols = names(df)) {
    for (col in cols) {
      df <- df[!outliers(df[[col]]),]
    }
    df
  }
  
  data_no_outlier <- remove_outliers(df_rfmt, cols = c("freq","monetary","time"))
  
  box1 <- data_no_outlier %>% 
    ggplot(aes(y = recency)) + 
    geom_boxplot(fill = "#C6D57E", show.legend = FALSE) +
    theme_bw() +
    labs(title="Recency")
  
  box2 <- data_no_outlier %>% 
    ggplot(aes(y = freq)) + 
    geom_boxplot(fill = "#D57E7E", show.legend = FALSE) +
    theme_bw() +
    labs(title="Frequency")
  
  box3 <- data_no_outlier %>% 
    ggplot(aes(y = monetary)) + 
    geom_boxplot(fill = "#A2CDCD", show.legend = FALSE) +
    theme_bw() +
    labs(title="Monetary")
  
  box4 <- data_no_outlier %>% 
    ggplot(aes(y = time)) + 
    geom_boxplot(fill = "#F7D060", show.legend = FALSE) +
    theme_bw() +
    labs(title="Time")
  
  fig <- ggarrange(box1,box2,box3,box4,
                   ncol=2,nrow=2)
  
  annotate_figure(fig, top = text_grob("No Outlier RFMT Terhadap Pelanggan", face = "bold", size = 14))
  
  ## Korelasi Data
  ggcorr(data_no_outlier %>% dplyr::select(recency
                                           ,freq
                                           ,monetary
                                           ,time)
         ,label = TRUE
         ,label_size = 4
         ,vjust=1
         ,hjust=0.5) +
    labs(title="Matriks Korelasi Data RFMT Terhadap Pelanggan") +
    theme_bw() 
  
  ## PCA
  pca_df  <- prcomp(df_rfmt[4:7], scale. = TRUE)
  pca_out <- prcomp(data_no_outlier[4:7], scale. = TRUE)
  
  df_norm <- pca_df$x
  out_norm <- pca_out$x
  
  ## Elbow Method
  fviz_nbclust(out_norm, pam, method = "wss",k.max = 15) + 
    labs(title = "Elbow Method", subtitle = "RFMT Pelanggan")
  
  ## Silhouette Method
  fviz_nbclust(out_norm, pam, method = "silhouette",k.max = 15) +
    labs(title = "Silhouette Index Method", subtitle = "RFMT Pelanggan")
  
  ## Gap Statistic
  fviz_nbclust(out_norm, pam, method = "gap_stat",k.max = 15,nboot = 5) + 
    labs(title = "Gap Statistic Method", subtitle = "RFMT Pelanggan")
  
  ## DBI
  dbi_df <- data.frame(cluster = numeric(), DBI = numeric())
  
  for (i in 2:10){
    kme <- pam(out_norm,i)
    dbi <- index.DB(out_norm,kme$clustering, centrotypes='medoids',d=dist(out_norm))
    vec <- c(i,dbi$DB)
    dbi_df[i,] <- vec
  }
  
  dbi_df <- na.omit(dbi_df)
  
  ## K-medoids Clustering
  kmedoids <- pam(df_norm,3)
  fviz_cluster(kmedoids, geom = "point") +
    labs(title = "Cluster Plot Data RFMT Pelanggan (Dengan Outlier)")
  
  kmedoids <- pam(out_norm,3)
  fviz_cluster(kmedoids,geom = "point") +
    labs(title = "Cluster Plot, RFMT Terhadap Pelanggan")
  
  ## Analisis RFMT Pelanggan
  data_no_outlier <- data_no_outlier %>% mutate(cluster = as.factor(kmedoids$clustering))
  
  result  <- df_rfmt %>% 
    dplyr::select(-first_purchase
                  ,-last_purchase) %>% 
    left_join(data_no_outlier %>% 
                dplyr::select(Nama_Pembeli
                              ,cluster)
              ,by="Nama_Pembeli") %>% 
    mutate(cluster = as.factor(ifelse(is.na(cluster),4,cluster))) 
  
  result %>% 
    ggplot(aes(x = freq, y = monetary, color = cluster)) + 
    geom_point(show.legend = TRUE, aes(shape=cluster), size = 3) +
    theme_bw() +
    labs(title="Cluster Plot, RFMT Pelanggan dengan Outlier",
         x="Dim1",
         y="Dim2") +
    scale_color_manual(values = c("1"="#C6D57E","2"="#D57E7E","3"="#A2CDCD","4"="black"))
  
  df_avg <- result %>%
    group_by(cluster) %>% 
    summarise(total_customer = length(Nama_Pembeli),
              r = mean(recency),
              f = mean(freq),
              m = mean(monetary),
              t = mean(time)) %>% 
    ungroup()
  
  df_avg %>% # recency
    ggplot(aes(x = cluster, y = r, fill = cluster)) + 
    geom_bar(stat = "identity",show.legend = FALSE) +
    coord_flip() +
    ylim(0,650) +
    geom_text(aes(label=paste0(round((r/sum(r))*100,1),"% (",round(r,2),")"),y = r+50), size=3) +
    theme_bw() +
    labs(
      title="Segmentasi RFMT Pelanggan Berdasarkan Recency",
      x="Cluster",
      y="R") +
    scale_fill_manual(values = my_palette)
  
  df_avg %>% # frequency
    ggplot(aes(x = cluster, y = f, fill = cluster)) + 
    geom_bar(stat = "identity",show.legend = FALSE) +
    coord_flip() +
    ylim(0,10) +
    geom_text(aes(label=paste0(round((f/sum(f))*100,1),"% (",round(f,2),")"),y = f+0.6), size=3) +
    theme_bw() +
    labs(
      title="Segmentasi RFMT Pelanggan Berdasarkan Frequency",
      x="Cluster",
      y="F") +
    scale_fill_manual(values = my_palette)
  
  df_avg %>% # monetary
    ggplot(aes(x = cluster, y = m, fill = cluster)) + 
    geom_bar(stat = "identity",show.legend = FALSE) +
    coord_flip() +
    ylim(0,4500000) +
    geom_text(aes(label=paste0(round((m/sum(m))*100,1),"% (",round(m,2),")"),y = m+400000), size=3) +
    theme_bw() +
    labs(
      title="Segmentasi RFMT Pelanggan Berdasarkan Monetary",
      x="Cluster",
      y="M") +
    scale_fill_manual(values = my_palette)
  
  df_avg %>% # time
    ggplot(aes(x = cluster, y = t, fill = cluster)) + 
    geom_bar(stat = "identity",show.legend = FALSE) +
    coord_flip() +
    ylim(0,100) +
    geom_text(aes(label=paste0(round((t/sum(t))*100,1),"% (",round(t,2),")"),y = t+7), size=3) +
    theme_bw() +
    labs(
      title="Segmentasi RFMT Pelanggan Berdasarkan Time",
      x="Cluster",
      y="T") +
    scale_fill_manual(values = my_palette)
  
# Mapping Cluster per Provinsi
  
  analysis_date <- date(max(initial_dataset$Tanggal_Pembayaran))+days(1)
  
  ## RFMT per Provinsi
  prov_df <- initial_dataset %>%
    group_by(Provinsi) %>% 
    summarise(first_purchase = min(tanggal),
              last_purchase = max(tanggal),
              recency = as.numeric((analysis_date-last_purchase)),
              monetary = sum(Jumlah_Produk_Dibeli*`Harga_Jual_(IDR)`),
              total_cust = length(Nama_Pembeli)) %>% 
    ungroup()
  
  prov_df <- initial_dataset %>% 
    dplyr::select(Provinsi, Nomor_Invoice) %>% 
    distinct() %>% 
    group_by(Provinsi) %>% 
    summarise(freq = n()) %>% 
    ungroup() %>% 
    right_join(prov_df, by = "Provinsi")
  
  prov_df <- prov_df %>% 
    mutate(time = round(as.numeric((last_purchase- first_purchase)/freq)),3) %>% 
    dplyr::select(Provinsi
                  ,total_cust
                  ,first_purchase
                  ,last_purchase
                  ,recency
                  ,freq
                  ,monetary
                  ,time)
  ## Outlier
  prov_out <- remove_outliers(prov_df, cols = c("freq","monetary","time"))
  
  ## Principal COmponent
  prov_pca  <- prcomp(prov_out[5:8], scale. = TRUE)
  prov_norm <- prov_pca$x
  
  ## DBI
  prov_dbi <- data.frame(cluster = numeric(), DBI = numeric())
  
  for (i in 2:10){
    kme <- pam(prov_norm,i)
    dbi <- index.DB(prov_norm,kme$clustering, centrotypes='medoids',d=dist(prov_norm))
    vec <- c(i,dbi$DB)
    prov_dbi[i,] <- vec
  }
  
  prov_dbi <- na.omit(prov_dbi)
  
  ## Clustering
  prov_km <- pam(prov_norm,4)
  fviz_cluster(prov_km, geom = "point") + labs(title = "Cluster Plot Per Provinsi")
  
  ## Add variabel cluster
  prov_out <- prov_out %>% mutate(cluster = as.factor(prov_km$clustering))
  
  prov_df  <- prov_df %>% 
    dplyr::select(-first_purchase
                  ,-last_purchase) %>% 
    left_join(prov_out %>% 
                dplyr::select(Provinsi
                              ,cluster)
              ,by="Provinsi") %>% 
    mutate(cluster = as.factor(ifelse(is.na(cluster),4,cluster)))
  
  my_palette <- c("#C6D57E","#D57E7E","#A2CDCD","#F7D060")
  
  ##SHP File for map Indonesia per Provinsi
  adm1_prov <- st_read("Data/shp/idn_admbnda_adm1_bps_20200401.shp")
  
  
  prov_df <- prov_df %>% 
    mutate(Provinsi = recode(Provinsi,
                             "D.I. Aceh"="Aceh",
                             "D.I. Yogyakarta"="Daerah Istimewa Yogyakarta",
                             "DKI Jakarta"="Dki Jakarta"))
  
  map <- geo_join(spatial_data = adm1_prov,
                  data_frame = prov_df,
                  by_sp = "ADM1_EN",
                  by_df = "Provinsi",
                  how = "inner")
  
  map %>% 
    ggplot(aes(fill = cluster)) +
    geom_sf() +
    scale_fill_manual(values = my_palette) +
    labs(title = "Segmentasi Provinsi Indonesia") +
    theme_get()
  
# RFMT Pelanggan Per Periode
  
  data_periode1 <- initial_dataset %>% filter(tanggal <= "2022-04-30")
  data_periode2 <- initial_dataset %>% filter(tanggal > "2022-04-30")
  
  ## rfmt untuk periode saat pandemi (2021/2022)
  analysis_date1 <- date(max(data_periode1$Tanggal_Pembayaran))+days(1)
  
  df_rfmt1 <- data_periode1 %>%
    group_by(Nama_Pembeli) %>% 
    summarise(first_purchase = min(tanggal),
              last_purchase = max(tanggal),
              recency = as.numeric((analysis_date1-last_purchase)),
              monetary = sum(Jumlah_Produk_Dibeli*`Harga_Jual_(IDR)`)) %>% 
    ungroup()
  
  df_rfmt1 <- data_periode1 %>% 
    dplyr::select(Nama_Pembeli, Nomor_Invoice) %>% 
    distinct() %>% 
    group_by(Nama_Pembeli) %>% 
    summarise(freq = n()) %>% 
    ungroup() %>% 
    right_join(df_rfmt1, by = "Nama_Pembeli")
  
  df_rfmt1 <- df_rfmt1 %>% 
    mutate(time = round(as.numeric((last_purchase- first_purchase)/freq)),3) %>% 
    dplyr::select(Nama_Pembeli
                  ,first_purchase
                  ,last_purchase
                  ,recency
                  ,freq
                  ,monetary
                  ,time)
  
  ## rfmt untuk periode setelah pandemi (2022/2023)
  analysis_date2 <- date(max(data_periode2$Tanggal_Pembayaran))+days(1)
  
  df_rfmt2 <- data_periode2 %>%
    group_by(Nama_Pembeli) %>% 
    summarise(first_purchase = min(tanggal),
              last_purchase = max(tanggal),
              recency = as.numeric((analysis_date2-last_purchase)),
              monetary = sum(Jumlah_Produk_Dibeli*`Harga_Jual_(IDR)`)) %>% 
    ungroup()
  
  df_rfmt2 <- data_periode2 %>% 
    dplyr::select(Nama_Pembeli, Nomor_Invoice) %>% 
    distinct() %>% 
    group_by(Nama_Pembeli) %>% 
    summarise(freq = n()) %>% 
    ungroup() %>% 
    right_join(df_rfmt2, by = "Nama_Pembeli")
  
  df_rfmt2 <- df_rfmt2 %>% 
    mutate(time = round(as.numeric((last_purchase- first_purchase)/freq)),3) %>% 
    dplyr::select(Nama_Pembeli
                  ,first_purchase
                  ,last_purchase
                  ,recency
                  ,freq
                  ,monetary
                  ,time)
  
  ## remove outlier per periode
  no_outlier1 <- remove_outliers(df_rfmt1, cols = c("freq","monetary","time"))
  no_outlier2 <- remove_outliers(df_rfmt2, cols = c("freq","monetary","time"))
  
  ## Matriks Korelasi Periode saat pandemi (2021/2022)
  
  ggcorr(no_outlier1 %>% dplyr::select(recency
                                       ,freq
                                       ,monetary
                                       ,time)
         ,label = TRUE
         ,label_size = 4
         ,vjust=1
         ,hjust=0.5) +
    labs(title="Correlation Matrix") +
    theme_bw()
  
  ## Matriks Korelasi Periode Setelah Pandemi (2022/2023)
  
  ggcorr(no_outlier2 %>% dplyr::select(recency
                                       ,freq
                                       ,monetary
                                       ,time)
         ,label = TRUE
         ,label_size = 4
         ,vjust=1
         ,hjust=0.5) +
    labs(title="Correlation Matrix") +
    theme_bw() 
  
  ## PCA per periode
  pca_out1 <- prcomp(no_outlier1[4:7], scale. = TRUE)
  pca_out2 <- prcomp(no_outlier2[4:7], scale. = TRUE)
  
  out_norm1 <- pca_out1$x
  out_norm2 <- pca_out2$x
  
  ## Penentuan k cluster per Periode
  set.seed(2222)
  
  ## Elbow Method
  fviz_nbclust(out_norm1, pam, method = "wss",k.max = 15) + 
    labs(title = "Elbow Method", subtitle = "RFMT Pelanggan Saat Pandemi")
  fviz_nbclust(out_norm2, pam, method = "wss",k.max = 15) + 
    labs(title = "Elbow Method", subtitle = "RFMT Pelanggan Setelah Pandemi")
  
  ## Silhouette Method
  fviz_nbclust(out_norm1, pam, method = "silhouette",k.max = 15) +
    labs(title = "Silhouette Index Method", subtitle = "RFMT Pelanggan Saat Pandemi")
  fviz_nbclust(out_norm2, pam, method = "silhouette",k.max = 15) + 
    labs(title = "Silhouette Index Method", subtitle = "RFMT Pelanggan Setelah Pandemi")
  
  ## Gap Statistic
  fviz_nbclust(out_norm1, pam, method = "gap_stat",k.max = 15,nboot = 5) + 
    labs(title = "Gap Statistic Method", subtitle = "RFMT Pelanggan Saat Pandemi")
  fviz_nbclust(out_norm2, pam, method = "gap_stat",k.max = 15,nboot = 5) + 
    labs(title = "Gap Statistic Method", subtitle = "RFMT Pelanggan Setelah Pandemi")
  
  ## DBI per periode
  dbi_df1 <- data.frame(cluster = numeric(), DBI = numeric())
  
  for (i in 2:10){
    kme <- pam(out_norm1,i)
    dbi <- index.DB(out_norm1,kme$clustering, centrotypes='medoids',d=dist(out_norm1))
    vec <- c(i,dbi$DB)
    dbi_df1[i,] <- vec
  }
  
  dbi_df1 <- na.omit(dbi_df1)
  
  dbi_df2 <- data.frame(cluster = numeric(), DBI = numeric())
  
  for (i in 2:10){
    kme <- pam(out_norm2,i)
    dbi <- index.DB(out_norm2,kme$clustering, centrotypes='medoids',d=dist(out_norm2))
    vec <- c(i,dbi$DB)
    dbi_df2[i,] <- vec
  }
  
  dbi_df2 <- na.omit(dbi_df2)
  
  ## K-medoids clustering per periode
  kmedoids_periode1 <- pam(out_norm1,2)
  fviz_cluster(kmedoids_periode1,geom = "point") +
    labs(title = "Cluster Plot Data RFMT Pelanggan Saat Pandemi")
  
  kmedoids_periode2 <- pam(out_norm2,2)
  fviz_cluster(kmedoids_periode2,geom = "point") +
    labs(title = "Cluster Plot Data RFMT Pelanggan Setelah Pandemi")
  
  ## (Periode 1) RFMT berdasarkan tiap cluster/segmen 
  no_outlier1 <- no_outlier1 %>% mutate(cluster = as.factor(kmedoids_periode1$clustering))
  
  result1  <- df_rfmt1 %>% 
    dplyr::select(-first_purchase
                  ,-last_purchase) %>% 
    left_join(no_outlier1 %>% 
                dplyr::select(Nama_Pembeli
                              ,cluster)
              ,by="Nama_Pembeli") %>% 
    mutate(cluster = as.factor(ifelse(is.na(cluster),3,cluster)))
  
  df_avg1 <- result1 %>%
    group_by(cluster) %>% 
    summarise(total_customer = length(Nama_Pembeli),
              r = mean(recency),
              f = mean(freq),
              m = mean(monetary),
              t = mean(time)) %>% 
    ungroup()
  
  df_avg1 %>% # R
    ggplot(aes(x = cluster, y = r, fill = cluster)) + 
    geom_bar(stat = "identity",show.legend = FALSE) +
    coord_flip() +
    ylim(0,250) +
    geom_text(aes(label=paste0(round((r/sum(r))*100,1),"% (",round(r,2),")"),y = r+20), size=3) +
    theme_bw() +
    labs(
      title="Segmentasi RFMT Pelanggan Periode Saat Pandemi Berdasarkan Recency",
      x="Cluster",
      y="R") +
    scale_fill_manual(values = my_palette)
  
  df_avg1 %>% # F
    ggplot(aes(x = cluster, y = f, fill = cluster)) + 
    geom_bar(stat = "identity",show.legend = FALSE) +
    coord_flip() +
    ylim(0,8) +
    geom_text(aes(label=paste0(round((f/sum(f))*100,1),"% (",round(f,2),")"),y = f+0.5), size=3) +
    theme_bw() +
    labs(
      title="Segmentasi RFMT Pelanggan Periode Saat Pandemi Berdasarkan Frequency",
      x="Cluster",
      y="F") +
    scale_fill_manual(values = my_palette)
  
  df_avg1 %>% # M
    ggplot(aes(x = cluster, y = m, fill = cluster)) + 
    geom_bar(stat = "identity",show.legend = FALSE) +
    coord_flip() +
    ylim(0,3500000) +
    geom_text(aes(label=paste0(round((m/sum(m))*100,1),"% (",round(m,2),")"),y = m+350000), size=3) +
    theme_bw() +
    labs(
      title="Segmentasi RFMT Pelanggan Periode Saat Pandemi Berdasarkan Monetary",
      x="Cluster",
      y="M") +
    scale_fill_manual(values = my_palette)
  
  df_avg1 %>% # T
    ggplot(aes(x = cluster, y = t, fill = cluster)) + 
    geom_bar(stat = "identity",show.legend = FALSE) +
    coord_flip() +
    ylim(0,60) +
    geom_text(aes(label=paste0(round((t/sum(t))*100,1),"% (",round(t,2),")"),y = t+4), size=3) +
    theme_bw() +
    labs(
      title="Segmentasi RFMT Pelanggan Periode Saat Pandemi Berdasarkan Time",
      x="Cluster",
      y="T") +
    scale_fill_manual(values = my_palette)
  
  ## (Periode 2) RFMT berdasarkan tiap cluster/segmen 
  no_outlier2 <- no_outlier2 %>% mutate(cluster = as.factor(kmedoids_periode2$clustering))
  
  result2  <- df_rfmt2 %>% 
    dplyr::select(-first_purchase
                  ,-last_purchase) %>% 
    left_join(no_outlier2 %>% 
                dplyr::select(Nama_Pembeli
                              ,cluster)
              ,by="Nama_Pembeli") %>% 
    mutate(cluster = as.factor(ifelse(is.na(cluster),3,cluster)))
  
  df_avg2 <- result2 %>%
    group_by(cluster) %>% 
    summarise(total_customer = length(Nama_Pembeli),
              r = mean(recency),
              f = mean(freq),
              m = mean(monetary),
              t = mean(time)) %>% 
    ungroup()
  
  df_avg2 %>% # R
    ggplot(aes(x = cluster, y = r, fill = cluster)) + 
    geom_bar(stat = "identity",show.legend = FALSE) +
    coord_flip() +
    ylim(0,300) +
    geom_text(aes(label=paste0(round((r/sum(r))*100,1),"% (",round(r,2),")"),y = r+30), size=3) +
    theme_bw() +
    labs(
      title="Segmentasi RFMT Pelanggan Periode Setelah Pandemi Berdasarkan Recency",
      x="Cluster",
      y="R") +
    scale_fill_manual(values = my_palette)
  
  df_avg2 %>% # F
    ggplot(aes(x = cluster, y = f, fill = cluster)) + 
    geom_bar(stat = "identity",show.legend = FALSE) +
    coord_flip() +
    ylim(0,7) +
    geom_text(aes(label=paste0(round((f/sum(f))*100,1),"% (",round(f,2),")"),y = f+0.5), size=3) +
    theme_bw() +
    labs(
      title="Segmentasi RFMT Pelanggan Periode Setelah Pandemi Berdasarkan Frequency",
      x="Cluster",
      y="F") +
    scale_fill_manual(values = my_palette)
  
  df_avg2 %>% # M
    ggplot(aes(x = cluster, y = m, fill = cluster)) + 
    geom_bar(stat = "identity",show.legend = FALSE) +
    coord_flip() +
    ylim(0,3500000) +
    geom_text(aes(label=paste0(round((m/sum(m))*100,1),"% (",round(m,2),")"),y = m+350000), size=3) +
    theme_bw() +
    labs(
      title="Segmentasi RFMT Pelanggan Periode Setelah Pandemi Berdasarkan Monetary",
      x="Cluster",
      y="M") +
    scale_fill_manual(values = my_palette)
  
  df_avg2 %>% # T
    ggplot(aes(x = cluster, y = t, fill = cluster)) + 
    geom_bar(stat = "identity",show.legend = FALSE) +
    coord_flip() +
    ylim(0,65) +
    geom_text(aes(label=paste0(round((t/sum(t))*100,1),"% (",round(t,2),")"),y = t+4), size=3) +
    theme_bw() +
    labs(
      title="Segmentasi RFMT Pelanggan Periode Setelah Pandemi Berdasarkan Time",
      x="Cluster",
      y="T") +
    scale_fill_manual(values = my_palette)
  
# RFMTS Produk
  
  ## Import Data review
  review <- read_csv("Data/Review_Data.csv")
  
  review <- review %>% 
    separate(rating,c("ket","review_score")) %>% 
    dplyr::select(product,review_score) %>% 
    mutate(review_score = as.numeric(review_score))
  
  review$product %>% unique() %>% length()
  
  df_s <- review %>% 
    group_by(product) %>% 
    summarise(rating = median(review_score)) %>% 
    ungroup() %>% 
    rename("Nama_Produk"="product"
           ,"satisfaction"="rating")
  
  ## Transformasi Data RFMTS
  analysis_date <- date(max(initial_dataset$Tanggal_Pembayaran))+days(1)
  
  df_rfmts <- initial_dataset %>%
    group_by(Nama_Produk) %>% 
    summarise(first_purchase = min(tanggal),
              last_purchase = max(tanggal),
              recency = as.numeric((analysis_date-last_purchase)),
              monetary = sum(Jumlah_Produk_Dibeli*`Harga_Jual_(IDR)`)) %>% 
    ungroup()
  
  df_rfmts <- initial_dataset %>% 
    dplyr::select(Nama_Produk, Nomor_Invoice) %>% 
    distinct() %>% 
    group_by(Nama_Produk) %>% 
    summarise(freq = n()) %>% 
    ungroup() %>% 
    right_join(df_rfmts, by = "Nama_Produk")
  
  df_rfmts <- df_rfmts %>% 
    mutate(time = round(as.numeric((last_purchase- first_purchase)/freq)),3) %>% 
    dplyr::select(Nama_Produk
                  ,first_purchase
                  ,last_purchase
                  ,recency
                  ,freq
                  ,monetary
                  ,time)
  
  df_rfmts <- df_rfmts %>% 
    left_join(df_s, by = "Nama_Produk")
  
  df_rfmts$satisfaction[is.na(df_rfmts$satisfaction)] <- sample(c(3:5)
                                                                ,size = sum(is.na(df_rfmts$satisfaction))
                                                                ,replace = TRUE
                                                                ,prob = c(0.1, 0.4, 0.5))
  
  ## Outlier
  p_no_outlier <- remove_outliers(df_rfmts, cols = c("freq","monetary","time","satisfaction"))
  
  ## Korelasi Data
  ggcorr(p_no_outlier %>% dplyr::select(recency
                                        ,freq
                                        ,monetary
                                        ,time
                                        ,satisfaction)
         ,label = TRUE
         ,label_size = 4
         ,vjust=1
         ,hjust=0.5) +
    labs(title="Correlation Matrix") +
    theme_bw()
  
  ## PCA
  p_pca <- prcomp(df_rfmts[4:8], scale. = TRUE)
  p_norm <- p_pca$x
  
  ## Penentuan k cluster
  ## Elbow Method
  fviz_nbclust(p_norm, pam, method = "wss",k.max = 15) + 
    labs(title = "Elbow Method", subtitle = "RFMTS Produk yang dibeli Pelanggan")
  
  ## Silhouette Method
  fviz_nbclust(p_norm, pam, method = "silhouette",k.max = 15) +
    ylim(0,0.5) +
    labs(title = "Silhouette Index Method", subtitle = "RFMTS Produk yang dibeli Pelanggan")
  
  ## Gap Statistic
  fviz_nbclust(p_norm, pam, method = "gap_stat",k.max = 15,nboot = 5) + 
    labs(title = "Gap Statistic Method", subtitle = "RFMTS Produk yang dibeli Pelanggan")
  
  ## K-medoids clustering
  p_kmedoids <- pam(p_norm,5)
  fviz_cluster(p_kmedoids, geom = "point") +
    labs(title = "Cluster Plot Data RFMTS Produk yang Dibeli Pelanggan")
  
  ## Analisis RFMTS
  df_rfmts <- df_rfmts %>% mutate(cluster = as.factor(p_kmedoids$clustering))
  
  p_result  <- df_rfmts %>% 
    dplyr::select(-first_purchase
                  ,-last_purchase) 
  
  p_df_avg <- p_result %>%
    group_by(cluster) %>% 
    summarise(total_produk = length(Nama_Produk),
              r = mean(recency),
              f = mean(freq),
              m = mean(monetary),
              t = mean(time),
              s = mean(satisfaction)) %>% 
    ungroup()
  
  my_palette_p <- c("#C6D57E","#D57E7E","#A2CDCD","#F7D060","#8D8DAA")
  
  p_df_avg %>% # R
    ggplot(aes(x = cluster, y = r, fill = cluster)) + 
    geom_bar(stat = "identity",show.legend = FALSE) +
    coord_flip() +
    ylim(0,550) +
    geom_text(aes(label=paste0(round((r/sum(r))*100,1),"% (",round(r,2),")"),y = r+40), size=3) +
    theme_bw() +
    labs(
      title="Segmentasi RFMTS Produk yang dibeli Pelanggan Berdasarkan Recency",
      x="Cluster",
      y="R") +
    scale_fill_manual(values = my_palette_p)
  
  p_df_avg %>% # F
    ggplot(aes(x = cluster, y = f, fill = cluster)) + 
    geom_bar(stat = "identity",show.legend = FALSE) +
    coord_flip() +
    ylim(0,500) +
    geom_text(aes(label=paste0(round((f/sum(f))*100,1),"% (",round(f,2),")"),y = f+45), size=3) +
    theme_bw() +
    labs(
      title="Segmentasi RFMTS Produk yang dibeli Pelanggan Berdasarkan Frequency",
      x="Cluster",
      y="F") +
    scale_fill_manual(values = my_palette_p)
  
  p_df_avg %>% # M
    ggplot(aes(x = cluster, y = m, fill = cluster)) + 
    geom_bar(stat = "identity",show.legend = FALSE) +
    coord_flip() +
    ylim(0,110000000) +
    geom_text(aes(label=paste0(round((m/sum(m))*100,1),"% (",round(m,2),")"),y = m+10000000), size=3) +
    theme_bw() +
    labs(
      title="Segmentasi RFMTS Produk yang dibeli Pelanggan Berdasarkan Monetary",
      x="Cluster",
      y="M") +
    scale_fill_manual(values = my_palette_p)
  
  p_df_avg %>% # T
    ggplot(aes(x = cluster, y = t, fill = cluster)) + 
    geom_bar(stat = "identity",show.legend = FALSE) +
    coord_flip() +
    ylim(0,150) +
    geom_text(aes(label=paste0(round((t/sum(t))*100,1),"% (",round(t,2),")"),y = t+15), size=3) +
    theme_bw() +
    labs(
      title="Segmentasi RFMTS Produk yang dibeli Pelanggan Berdasarkan Time",
      x="Cluster",
      y="T") +
    scale_fill_manual(values = my_palette_p)
  
  p_df_avg %>% # S
    ggplot(aes(x = cluster, y = s, fill = cluster)) + 
    geom_bar(stat = "identity",show.legend = FALSE) +
    coord_flip() +
    ylim(0,7) +
    geom_text(aes(label=paste0(round((s/sum(s))*100,1),"% (",round(s,2),")"),y = s+0.5), size=3) +
    theme_bw() +
    labs(
      title="Segmentasi RFMTS Produk yang dibeli Pelanggan Berdasarkan Satisfaction",
      x="Cluster",
      y="S") +
    scale_fill_manual(values = my_palette_p)
  