void fn1() {
    return;
}
int main() {
    int i, j, k;
    write("test 1");
    j = read();

    for (i=0; i<9; i=i+1) {
        write("test 2");
    }

    while (i < 20) {
        write("test 3");
        i = i + 1;
    }

    if (i < 30) {
        write("test 4");
    }

    if (i < 40) {
        write("test 5");
    } else if (i < 50) {
        write("test 6");
    } else {
        write("test 7");
    }
    return 1;
}
