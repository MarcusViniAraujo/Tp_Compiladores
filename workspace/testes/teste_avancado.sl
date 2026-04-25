// teste_avancado.sl

struct Box {
    val : int;
}

func main() : int {
    // 1. Teste de Inferência (SLetInfer)
    // Não dissemos que x é int, o compilador tem que adivinhar
    let x = 10; 
    let y = 20;

    // Se a inferência falhar, isso daria erro de tipo (Int + ???)
    let soma = x + y;

    // 2. Teste de Generics (Simplificado)
    // Como nossa unificação aceita TVar, isso deve passar se a AST estiver correta
    // (O parser precisa suportar o 'forall' se ele estiver na gramática)
    
    return soma;
}