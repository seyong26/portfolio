#연결선수 기준
centr_degree(g,mode = "all", normalized = T)
which.max(centr_degree(g,mode = "all", normalized = T)$res)
closeness(g, mode = "all",weights = NA)
#근접 중심성 기준
centr_clo(g, mode="all", normalized = T)
which.max(centr_clo(g, mode="all", normalized = T)$res)
#중개 중심성 기준
betweenness(g, directed = T,weights = NA)
which.max(betweenness(g, directed = T,weights = NA))
edge_betweenness(g, directed = T,weights = NA)
centr_betw(g,directed = T, normalized = T)
which.max(centr_betw(g,directed = T, normalized = T)$res)
set.seed(42)
layout1 <- layout_with_fr(g, niter = 1000, area = vcount(g)^2)
#ceb <- cluster_edge_betweenness(g)

#plot_dendrogram(ceb,mode="hclust", cex=0.5)

#plot(ceb, g,  vertex.size = 5, vertex.label = NA)
#length(ceb)
#plot(ceb, g_node_only,
#     layout = layout1,
#     vertex.size = 5,
#     vertex.label = NA,
#     edge.color = NA,            # 혹시 몰라 명시
#     main = "cluster_edge_betw")

randIndex(com,kmeans_label)
com <- as.numeric(membership(ceb))
length(com)
length(kmeans_label)
table(com,kmeans_label)

cl_louvain <- cluster_louvain(g)
plot(cl_louvain, g, layout = layout1, vertex.size = 5, vertex.label = NA)
g_node_only <- delete_edges(g, E(g))  # 모든 edge 삭제
# 기존 layout 재사용
plot(cl_louvain, g_node_only,
     layout = layout1,
     vertex.size = 5,
     vertex.label = NA,
     edge.color = NA,            # 혹시 몰라 명시
     main = "Louvain Communities (Nodes Only)")
length(cl_louvain)
table(membership(cl_louvain),kmeans_label)
randIndex(cl,kmeans_label)
cl <- as.numeric(membership(cl_louvain))


clp <- cluster_label_prop(g)
length(clp)
plot(clp,g, vertex.size = 5, vertex.label = NA)
a <- table(membership(clp),kmeans_label)
randIndex(a)
plot(clp, g_node_only,
     layout = layout1,
     vertex.size = 5,
     vertex.label = NA,
     edge.color = NA,            # 혹시 몰라 명시
     main = "clp(전파 레이블)")

cw <- cluster_walktrap(g)	
length(cw)
plot(cw,g, vertex.size = 5, vertex.label = NA)
plot(cw, g_node_only,
     layout = layout1,
     vertex.size = 5,
     vertex.label = NA,
     edge.color = NA,            # 혹시 몰라 명시
     main = "cluster walktrap")

a <- table(membership(cw),kmeans_label)
randIndex(a)

ci <- cluster_infomap(g)	
length(ci)
plot(ci,g, vertex.size = 5, vertex.label = NA)
plot(ci, g_node_only,
     layout = layout1,
     vertex.size = 5,
     vertex.label = NA,
     edge.color = NA,            # 혹시 몰라 명시
     main = "cluster infomap")
a <- table(membership(ci),kmeans_label)
randIndex(a)
#co <- cluster_optimal(g)
#length(co)


cfg <- cluster_fast_greedy(g)
plot(cfg,g, vertex.label=NA)
plot(cfg, g_node_only,
     layout = layout1,
     vertex.size = 5,
     vertex.label = NA,
     edge.color = NA,            # 혹시 몰라 명시
     main = "cluster fast greedy")
length(cfg)
randIndex(as.factor(membership(cfg)),kmeans_label)

# 예: 2차원 MDS 결과 기반 k-means (k=3)
set.seed(1234)
k_result <- kmeans(mds_result, centers = 3,nstart = 25)
table(k_result$cluster)
# 클러스터 번호를 노드 속성으로 지정
V(g)$cluster <- k_result$cluster
# 클러스터별 색상 지정
cluster_colors <- rainbow(length(unique(k_result$cluster)))
V(g)$color <- cluster_colors[V(g)$cluster]
# 기존 layout (예: layout_with_fr)
g <- delete_vertices(g, igraph::degree(g) == 0)
plot(g,
     layout = layout,
     vertex.size = 5,
     vertex.label = NA,
     vertex.color = V(g)$color,
     edge.color = adjustcolor("gray60", alpha.f = 0.2),
     main = "Colored by k-means Clustering")

plot(g_node_only,
     layout = layout1,
     vertex.size = 5,
     vertex.label = NA,
     vertex.color = V(g)$color,
     edge.color = adjustcolor("gray20", alpha.f = 0.2),
     main = "Network Colored by k-means Clustering")
# 각 그룹별 노드 인덱스 목록 생성
group_list <- split(V(g), V(g)$cluster)


V(g_node_only)$cluster <- k_result$cluster

# 예시: kmeans 결과가 V(g_node_only)$cluster 에 저장되어 있다면
V(g_node_only)$color <- cluster_colors[V(g_node_only)$cluster]
layout <- layout_with_fr(g, niter = 1000, area = vcount(g)^2)

# 다시 시각화
plot(g_node_only,
     layout = layout1,
     vertex.size = 5,
     vertex.label = NA,
     vertex.color = V(g_node_only)$color,  # 추가됨!
     edge.color = NA,
     mark.groups = group_list,
     mark.col = adjustcolor(cluster_colors, alpha.f = 0.2),
     main = "k-means Clustering with Group Boundaries")
