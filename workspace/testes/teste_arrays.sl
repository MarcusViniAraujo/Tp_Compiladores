// teste_arrays.sl (Versão Final Compatível)

func main() : int {
    print("Testando Arrays...");
    
    // Usando literal direto para evitar erro de sintaxe no Parser com 'new'
    let arr : int[] = [0, 0, 0, 0, 0];
    
    // Teste de Leitura de Tamanho (TypeChecker vai validar EArraySize)
    let tam = arr.size; 
    print("Tamanho:");
    print(tam);

    // Teste de Escrita (TypeChecker vai validar EIndex no lado esquerdo)
    arr[0] = 10;
    arr[1] = 20;
    
    // Teste de Leitura (TypeChecker vai validar EIndex no lado direito)
    print("Valores:");
    print(arr[0]);
    print(arr[1]);
    
    // Loop para somar
    let i = 0;
    let soma = 0;
    
    while (i < 5) {
         // Se o array tiver menos de 5, o interpretador daria erro, 
         // mas aqui garantimos 5 zeros na inicialização.
         soma = soma + arr[i];
         i = i + 1;
    }
    
    print("Soma (deve ser 30):");
    print(soma);

    return 0;
}