func inc(x : int) : int {
    return x + 1;
}

func constante(x : int, y : int) : int {
    return x;
} 

func main() : int {

    let y = 0;

    print(inc(y));
    print(y);
    print(constante(10, 20)); 

    return 0;
}