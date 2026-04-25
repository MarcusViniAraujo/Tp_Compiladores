func soma(a: int, b: int) : int {
    return a + b;
}

func main() : int {

    let x = 10; 
    
    print("Valor de x (inferido):");
    print(x);

    let resultado = x + 30;
    print("x + 30 = ");
    print(resultado);

    let ativo = true;

    if (ativo) {
        print("Inferencia de Booleano!");
    }

    let z = soma(x, resultado);
    print("Resultado da soma inferida:");
    print(z);

    return 0;
}