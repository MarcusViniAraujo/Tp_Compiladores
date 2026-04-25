struct Ponto {
    x : int;
    y : int;
}

func soma(a : int, b : int) : int {
    print("Calculando soma...");
    return a + b;
}

func main() : int {
    print("Inicio do Programa");
    
    let p : Ponto = Ponto{10, 20};
    print(p.x);
    
    let resultado = soma(p.x, p.y);
    print("Resultado:");
    print(resultado);
    
    if (resultado > 25) {
        print("Maior que 25!");
    }
    
    return 0;
}