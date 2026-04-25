func main() : int {
    let arr : int[] = [10, 20, 30, 40, 50];
    
    let i = 0;
    let soma = 0;
    
    while (i < 5) {
        soma = soma + arr[i]; // Teste do i32.load
        i = i + 1;
    }
    
    arr[0] = 999;

    print(soma);
    
    return soma; // Deve retornar 150 (10+20+30+40+50)
}