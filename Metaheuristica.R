source("funcoes.R")

main = function(tabela,BTmax,nvizinho,Lt) {

	rota = 1:length(tabela[,1])
	crota = calc_custo(tabela,rota)
	print(paste("Rota Burra:"))
	print(rota)
	print(paste("Custo:",calc_custo(tabela,rota)))


	rota = heuristica_vizinho(tabela)
	print("Rota construída:")
	print(rota)
	print(paste("Custo:",calc_custo(tabela, rota)))

	BTmax = BTmax
	nvizinho = nvizinho
	Lt = Lt

	lista = busca_tabu(tabela, rota,BTmax, nvizinho, Lt)
	rota = lista[[1]]
	tempo = lista[[2]]
	print("Rota Tabu:")
	print(rota)
	print(paste("Custo:",calc_custo(tabela, rota)))
	print(paste("Tempo de Processamento: ", tempo,"segundos"))
	
	return(lista)
}