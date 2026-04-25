func max(a : int, b : int) : int {
    if (a > b) {
        return a;
    }
    return b;
}

func main() : int {
    // 1. Entrada de dados expandida (6 itens)
    let n : int = 6;
    let capacidade : int = 100;
    
    // Itens: (Valores) [10, 30, 45, 80, 100, 150] 
    //        (Pesos)   [5, 10, 20, 30, 40, 50]
    let valores : int[] = [10, 30, 45, 80, 100, 150];
    let pesos : int[] = [5, 10, 20, 30, 40, 50];
    
    // 2. Tabela DP para capacidade 100 (101 posicoes de 0 a 100)
    // Inicializacao literal obrigatoria para o funcionamento no Interpretador
    let dp : int[] = [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0];

    let i : int = 0;
    let j : int = 0;

    // 3. Processamento (Programacao Dinamica 0-1)
    while (i < n) {
        let pesoAtual : int = pesos[i];
        let valorAtual : int = valores[i];
        
        j = capacidade;
        while (j >= pesoAtual) {
            let valorSeEscolher : int = dp[j - pesoAtual] + valorAtual;
            dp[j] = max(dp[j], valorSeEscolher);
            j = j - 1;
        }
        i = i + 1;
    }

    // 4. Exibicao do Resultado
    print("Capacidade testada: 100");
    print("Valor maximo calculado:");
    print(dp[100]);

    return 0;
}