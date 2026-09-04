rm(list=ls())

#install.packages("sna")
library(sna)
library(readxl)
#install.packages("tidyverse")
library(tidyverse)

data <- read_xlsx("C:\\Users\\kikle\\OneDrive\\문서\\네이트온 받은 파일\\Nikel mineral database_20250803 (1).xlsx")
importana_num <- c(41,85,95,136,160,165,179)

element_list <- strsplit(data$`Chemistry Elements`, " ")

all_elements <- sort(unique(unlist(element_list)))


element_matrix <- sapply(all_elements, function(elem) {
  sapply(element_list, function(elist) ifelse(elem %in% elist, 1, 0))
})


new_data <- bind_cols(data["Mineral Name"], as_tibble(element_matrix))
new_data <- as.data.frame(new_data)
#write.csv(new_data,"element_matrix.csv", row.names = FALSE)
dim(new_data)
#검증
count <- rep(0,length(element_list))
for(i in 1:length(element_list)){
  count[i] <- length(element_list[[i]])
}
count
valid <- rep(rep(0,dim(new_data)[1]))
for(i in 1:dim(new_data)[1]){
  valid[i] <- sum(new_data[i,-1])
}
sum(count != valid)


rownames(new_data) <- new_data[,1]
new_data <- new_data[,-1]
new_data <- as.matrix(new_data)
n <- nrow(new_data)
m <- ncol(new_data)
vertex.col <- c(rep("blue",n),rep("red",m))
vertex.cex <- c(rep(1,n),rep(2,m))

gplot(new_data,displaylabels = F, boxed.labels = F,
      vertex.col = vertex.col, vertex.cex = vertex.cex,
      label.col = vertex.col, label.cex = 1, usearrows = F,
      edge.col = "gray50", edge.lwd = 0.7)


# 노드 수: n = 광물, m = 원소
vertex_labels <- c(rep("", n), colnames(new_data))  # 광물은 "", 원소는 이름
#png("network.png", width = 1600, height = 1200, res = 200)  # 해상도 높이기

gplot(new_data,
      gmode = "twomode",
      displaylabels = TRUE,             # 꼭 TRUE로 해두고
      label = vertex_labels,            # 노드별 표시할 이름을 여기에 지정!
      boxed.labels = FALSE,
      vertex.col = vertex.col,
      vertex.cex = vertex.cex,
      label.col = vertex.col,
      label.cex = 1,
      usearrows = FALSE,
      edge.col = "gray50",
      edge.lwd = 0.7)
#dev.off()


rows <- new_data%*%t(new_data)
table(rows)
diag(rows) <- 0
#gplot(rows, displaylabels = F, boxed.labels = F, vertex.col = "blue", edge.lwd = rows/2, edge.col = "grey50", label.pos = 1, usearrows=F)

cols <- t(new_data)%*%new_data
diag(cols) <- 0
#png("element.png", width = 1600, height = 1200, res = 200)  # 해상도 높이기

gplot(cols, displaylabels = T, boxed.labels = F, vertex.col = "blue", edge.lwd = rows/2, edge.col = "grey50", label.pos = 1, usearrows=F)
#dev.off()

library(igraph)

# 1. 연결 행렬 계산
new_data2 <- new_data[,-24]
#new_data2 <- new_data2[-97,]
rows <- new_data2 %*% t(new_data2)
diag(rows) <- 0

# 2. co-occurrence 2 이상만 남기기
#rows[rows < 2] <- 0

# 3. 그래프 생성
g <- graph_from_adjacency_matrix(rows, mode = "undirected", weighted = TRUE, diag = FALSE)

# 4. 연결 없는 노드 제거
#g <- delete_vertices(g, degree(g) == 0)
g1 <- delete_vertices(g, igraph::degree(g) == 0)

V(g1)$size <- sqrt(igraph::degree(g1))/2
E(g1)$width <- log1p(E(g1)$weight)
E(g1)$color <- adjustcolor("gray60", alpha.f = 0.3)

# 4. 레이아웃
set.seed(42)
layout <- layout_with_fr(g1, niter = 1000, area = vcount(g1)^2)


#dev.off()

# 1. 타입 정보 가져오기
type <- data$`Origin (7 Category)`
names(type) <- data$`Mineral Name`

# 2. igraph 노드 순서에 맞춰 매핑
V(g1)$type <- type[V(g1)$name]
table(data$`Origin (7 Category)`)
# 3. 색상 매핑 정의
type_colors <- c(
  "Biological" = "red",
  "Extraterrestrial" = "orange",
  "Hydrothermal" = "yellow",
  "Igneous" = "green",
  "Metamorphic" = "blue",
  "Sedimentary" = "navy",
  "Supergene" = "purple"
)

V(g1)$color <- type_colors[V(g1)$type]
#png("mineral_type1.png", width = 1600, height = 1200, res = 200)
plot(g1,
     layout = layout,
     vertex.label = NA,
     vertex.color = V(g1)$color,
     vertex.size = V(g1)$size,
     edge.width = E(g1)$width,
     edge.color = E(g1)$color,
     main = "Mineral Network by Origin"
)
legend("topright",
       legend = names(type_colors),
       col = type_colors,
       pch = 16,
       pt.cex = 1.5,
       bty = "n",
       title = "Mineral Origin")
#dev.off()

n <- nrow(new_data)
dist_mat1 <- matrix(0, n, n)

for (i in 1:n) {
  for (j in 1:n) {
    x <- new_data[i, ]
    y <- new_data[j, ]
    union_count <- sum((x + y) > 0)  # x 또는 y가 1인 항목 수
    diff_count <- sum(x != y)        # 서로 다른 항목 수
    dist_mat1[i, j] <- if (union_count == 0) 0 else diff_count / union_count
  }
}


type <- data$`(Biotic, Abiotic, Both)`
dist_mat2 <- diag(rep(0,190))
type[type == "Biotic"] = 1
type[type == "Abiotic"] = 0
type[type == "Both"] = 0.5
type <- as.numeric(type)
for (i in 1:length(type)){
  for(j in 1:length(type)){
    dist_mat2[i,j] <- abs(type[i]-type[j])
  }
}

# 문자열 벡터
raw_modes <- data$`Paragenetic Modes`
num <- sapply(raw_modes, function(x) {
  x <- gsub(" ", "", x)  # 공백 제거
  nums <- as.numeric(unlist(strsplit(x, ",")))
  min(nums, na.rm = TRUE)
})

dist_mat3 <- diag(rep(0,190))
for (i in 1:length(num)){
  for(j in 1:length(num)){
    dist_mat3[i,j] <- num[i]!=num[j]
  }
}
dist_mat3

dist_mat4 <- diag(rep(0,190))
stage <- data$`Stage (lowest)`
stage <- substr(stage,0,2)
stage <- gsub(",", "", stage)        # 쉼표 제거
as.factor(stage)
#10개 범주 
stage[stage==0]=0
stage[stage==1]=1/9
stage[stage==2]=2/9
stage[stage=="3a"]=3/9
stage[stage=="3b"]=4/9
stage[stage=="4a"]=5/9
stage[stage=="4b"]=6/9
stage[stage==5]=7/9
stage[stage==7]=8/9
stage[stage==10]=1
stage <- as.numeric(stage)

for(i in 1:length(stage)){
  for(j in 1: length(stage)){
    dist_mat4[i,j] <- abs(stage[i]-stage[j])
  }
}
dist_mat4

dist_mat5 <- diag(rep(0,nrow(data)))

nik <- data$`Nikel valence`
for(i in 1:nrow(dist_mat5)){
  for(j in 1:nrow(dist_mat5)){
    dist_mat5[i,j] <- nik[i]!=nik[j]
  }
}



dist <- (3*dist_mat1 +2*dist_mat2 + 2*dist_mat3+2*dist_mat4 + dist_mat5)/10
mds_result <- cmdscale(dist, k = 2)
plot(mds_result)
type <- data$`Origin (7 Category)`  # 예시
names(type) <- data$`Mineral Name`      # 매칭 위해 이름 부여

# 고유 그룹별 색상 팔레트 지정
group_colors <- c("Biotic" = "red", "Abiotic" = "blue", "Both" = "green")  # 원하는 대로 조정 가능

# 각 점에 대응하는 색상 추출
point_colors <- type_colors[type]

# MDS 시각화
plot(mds_result,
     col = point_colors,
     pch = 19,
     xlab = "Dimension 1",
     ylab = "Dimension 2",
     main = "MDS by Type")


rownames(dist) <- colnames(dist) <- rownames(new_data)
dist_obj <- as.dist(dist)

hc <- hclust(dist_obj, method = "ward.D2")

plot(hc, labels = rownames(new_data), main = "Hierarchical Clustering", cex = 0.5)
library(ape)
type_vec <- data$`(Biotic, Abiotic, Both)`
names(type_vec) <- data$`Mineral Name`
phy <- as.phylo(hc)
tip_types <- type_vec[phy$tip.label]
type_colors <- c("Biotic" = "red", "Abiotic" = "blue", "Both" = "green")
tip_cols <- type_colors[tip_types]

plot(as.phylo(hc), type = "fan", tip.color = type_colors, cex = 0.5, main = "Phylogenetic Tree Colored by Type")
plot(as.phylo(hc), type = "cladogram", tip.color = type_colors, cex = 0.5, main = "Phylogenetic Tree Colored by Type")
plot(as.phylo(hc), type = "unrooted", tip.color = type_colors, cex = 0.5, main = "Phylogenetic Tree Colored by Type")

#install.packages("NbClust")
library(NbClust)
mds_df <- as.data.frame(mds_result)
nbc <- NbClust(mds_df,min.nc = 2, max.nc = 15, method = "kmeans")
table(nbc$Best.nc[1,])
par(mfrow=c(1,1))

barplot(table(nbc$Best.nc[1,]))

wssplot <- function(data,nc=15,seed=1234){
  wss <- (nrow(data)-1)*sum(apply(data,2,var))
  for(i in 2:nc){
    set.seed(seed)
    wss[i] <- sum(kmeans(data,centers=i)$withinss)
  }
  plot(1:nc,wss,type="b")
}

wssplot(dist)

#install.packages("factoextra")
library(factoextra)
set.seed(1234)
km <- kmeans(mds_result, centers = 3,nstart = 25)
km$size

plot(mds_result, col = km$cluster, pch = 19, main = "K-means Clustering")
points(km$centers,col=1:3,pch=8,cex=1.5)

mds_df <- as.data.frame(mds_result)

fviz_cluster(km, data = mds_df)  # 클러스터 시각화

rownames(mds_df) <- rownames(new_data)
#install.packages("flexclust")
library(flexclust)
library(cluster)
result <- pam(dist,3)

clusplot(result)

fviz_cluster(
  list(data = mds_df, cluster = result$clustering),
  geom = "text",
  labelsize = 5,
  repel = TRUE,
  main = "PAM Clustering of Names (Levenshtein Distance)"
)

kmeans_label <- km$cluster
pam_label <- result$clustering


sil_pam <- silhouette(pam_label, dist_obj)
sil_kmeans <- silhouette(kmeans_label, dist_obj)
mean(sil_pam[,3])
mean(sil_kmeans[,3])

plot(sil_pam, main="Silhouette - PAM")
plot(sil_kmeans, main="Silhouette - KMeans")

randIndex(table(kmeans_label,pam_label))
table(kmeans_label,pam_label)

