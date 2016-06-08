
library(igraph)
library(MASS)

n = 200 # 样本数
node_num = 10 # 节点数
arr_num = 1 # 属性信息个数
arr_sample_num = 1000 #属性信息变量样本个数
power = 1 # 生成scale-free network 幂率指数
beta = 4 # 
rho = 0.8 # 
p_theta = 0.5 #Gibbs sampling probilities
G = vector('list',n) # 图list
M = vector('list',n) # 矩阵list
B = vector('list',n) # beta矩阵list

if( rho >0 && rho <1 ){
    for (i in 1:n){
        g <- barabasi.game(node_num , power = power, directed = FALSE) #scale-free network 生成图
        G[[i]] = g
        M[[i]] = as_adjacency_matrix(g)
        B[[i]] = M[[i]]
        len = length(M[[i]]@x)
        rand = runif(len, min = 0, max = 1)
        for (j in 1:len){
            if( rand[j]>rho ){
                B[[i]]@x[j] = 0
            }else if( rand[j]<= rho/2 ){
                B[[i]]@x[j] = beta
            }else{
                B[[i]]@x[j] = (-beta)
            }
        }
        sigma = diag(arr_num)
        mu = rep(0,arr_num)
        x = mvrnorm(n=arr_sample_num,mu,sigma)

    }
}else{
    cat("rho is error!\n");
}

GibbsSampling = function(num, p_theta){

} 