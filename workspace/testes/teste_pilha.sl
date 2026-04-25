func push(stack : int[], topo : int, valor : int) : int[] {
    if (topo < stack.size) {
        stack[topo] = valor;
    }
    return stack;
}

func pop(stack : int[], topo : int) : int {
    return stack[topo - 1];
}

func main() : int {
    let pilha : int[] = [0, 0, 0, 0, 0];
    let topo : int = 0;

    print("=== INICIO DA PILHA ===");

    // PUSH 10
    print("Push 10");
    pilha = push(pilha, topo, 10);
    topo = topo + 1;
    print("Topo atual:");
    print(topo);

    // PUSH 20
    print("Push 20");
    pilha = push(pilha, topo, 20);
    topo = topo + 1;
    print("Topo atual:");
    print(topo);

    // PUSH 30
    print("Push 30");
    pilha = push(pilha, topo, 30);
    topo = topo + 1;
    print("Topo atual:");
    print(topo);

    print("=== INICIO DOS POPS ===");

    // POP 1
    if (topo > 0) {
        print("Pop:");
        print(pop(pilha, topo));
        topo = topo - 1;
        print("Topo atual:");
        print(topo);
    }

    // POP 2
    if (topo > 0) {
        print("Pop:");
        print(pop(pilha, topo));
        topo = topo - 1;
        print("Topo atual:");
        print(topo);
    }

    // POP 3
    if (topo > 0) {
        print("Pop:");
        print(pop(pilha, topo));
        topo = topo - 1;
        print("Topo atual:");
        print(topo);
    }

    print("=== FIM DA PILHA ===");

    return 0;
}
