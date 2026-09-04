library(MASS)

# 거리행렬 변환
d_mat <- sum_dist

# 자기 자신 제외하고 0인 거리만 아주 작은 값으로 변경
d_mat[d_mat == 0 & row(d_mat) != col(d_mat)] <- 1e-8

d <- as.dist(d_mat)

library(MASS)

set.seed(123)
mds <- cmdscale(d, k = 2)


# Shepard plot용 객체 생성
sh <- Shepard(d, mds)

plot(sh,pch = ".")

lines(sh$x,sh$yf,type="S",col = "red")

cor_pearson <- cor(sh$x, sh$y, method = "pearson")
cor_spearman <- cor(sh$x, sh$y, method = "spearman")

cor_pearson
cor_spearman

mds_dist <- dist(mds)

# Stress 계산
stress <- sqrt(sum((as.vector(d) - as.vector(mds_dist))^2) / sum(as.vector(d)^2))

stress

mds_cmd <- cmdscale(d, k = 2, eig = TRUE)

gof <- mds_cmd$GOF

eig <- mds_cmd$eig
positive_eig <- eig[eig > 0]
axis_explain <- positive_eig[1:2] / sum(positive_eig)
cum_explain <- sum(axis_explain)
