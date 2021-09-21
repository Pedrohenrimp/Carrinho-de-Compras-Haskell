type ProdutoEstoque = ([Char], Float, Int)
type EstoqueProdutos = [ProdutoEstoque]

type ProdutoCarrinho = ([Char], Float, Int)
type CarrinhoCompra = [ProdutoCarrinho]


--Adicionando ao Estoque

monitorEstoque :: ProdutoEstoque
monitorEstoque = ("monitor", 500, 100)

telefoneEstoque :: ProdutoEstoque
telefoneEstoque = ("telefone", 150, 300)

tecladoEstoque :: ProdutoEstoque
tecladoEstoque = ("teclado", 70, 50)

mouseEstoque :: ProdutoEstoque
mouseEstoque = ("mouse", 50, 50)

estoque :: EstoqueProdutos
estoque = monitorEstoque : telefoneEstoque : tecladoEstoque : mouseEstoque : []


--Adicionando ao Carrinho

monitorCarrinho :: ProdutoCarrinho
monitorCarrinho = ("monitor", 500, 2)

telefoneCarrinho :: ProdutoCarrinho
telefoneCarrinho = ("telefone", 150, 5)

tecladoCarrinho :: ProdutoCarrinho
tecladoCarrinho = ("teclado", 70, 2)

carrinho :: CarrinhoCompra
carrinho = monitorCarrinho : telefoneCarrinho : tecladoCarrinho : []


--Finalizando a compra
getNome :: ([Char], Float, Int) -> [Char]
getNome(nome, _, _) = nome

getValor :: ([Char], Float, Int) -> Float
getValor(_, valor, _) = valor

getQuantidade :: ([Char], Float, Int) -> Int
getQuantidade(_, _, quantidade) = quantidade

newMonitor :: ProdutoEstoque
newMonitor = ("monitor", 500 , getQuantidade monitorEstoque - getQuantidade monitorCarrinho)

newTelefone :: ProdutoEstoque
newTelefone = ("telefone", 150 , getQuantidade telefoneEstoque - getQuantidade telefoneCarrinho)

newTeclado :: ProdutoEstoque
newTeclado = ("teclado", 70, getQuantidade tecladoEstoque - getQuantidade tecladoCarrinho)


estoqueFinal :: EstoqueProdutos
estoqueFinal = newMonitor : newTelefone : newTeclado : mouseEstoque : []


--Calculando o total

getQuantidadeFloat :: ([Char], Float, Int) -> Float
getQuantidadeFloat (_, _, quantidade) = fromIntegral quantidade

totalProduto :: ProdutoCarrinho -> Float
totalProduto produto = (getQuantidadeFloat produto) * (getValor produto)

listaTotais :: [Float]
listaTotais = map totalProduto carrinho

totalCompra :: Float
totalCompra = sum listaTotais


--Main
main :: IO ()
main = do

putStrLn "Estoque: "
print(estoqueFinal)
putStrLn "Carrinho: "
print(carrinho)
putStrLn "Total: "
print(totalCompra)