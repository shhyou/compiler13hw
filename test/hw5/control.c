int main() {
    int a, b;
    float c, d;
    a = 1;
    b = 0;
    if (1) {
        write("#0 correct\n");
    }
    if (a == b) {
        write("#1 wrong\n");
    } else {
        write("#1 correct\n");
    }
    if (a != b) {
        write("#2 correct\n");
    } else {
        write("#2 wrong\n");
    }

    c = 1.0;
    d = 0.0;
    if (c > d) {
        write("#3 correct\n");
    } else {
        write("#3 wrong\n");
    }

    if (a && b) {
        write("#4 wrong\n");
    } else {
        write("#4 correct\n");
    }

    if (a || b) {
        write("#5 correct\n");
    }

    a = 0;
    while (a<10) {
        write("#6 correct: ");
        write(a);
        write("\n");
        a = a+1;
    }

    return 0;
}
