func ordenacao(arr : int[]) : int[] {
    let n = arr.size;
    let i = 0;

    while (i < n - 1) {
        let menor = i;
        let j = i + 1;

        while (j < n) {
            if (arr[j] < arr[menor]) {
                menor = j;
            }
            j = j + 1;
        }

        let temp = arr[i];
        arr[i] = arr[menor];
        arr[menor] = temp;

        i = i + 1;
    }

    return arr; 
}

func main() : int {
    let meuVetor : int[] = [45, 12, 89, 3, 27];
    
    print(45);
    print(12);
    print(89);
    print(3);
    print(27);

    meuVetor = ordenacao(meuVetor);

    print( "fim da ordenacao"); 

    let k = 0;
    while (k < meuVetor.size) {
        print(meuVetor[k]);
        k = k + 1;
    }

    return 0;
}