int num;

void fna() {
    write("a passed\n");
}

int fnb() {
    write("b passed\n");
    return 0;
}

float fnc() {
    write("c passed\n");
    return 0.0;
}

void fnd() {
    if (num < 10) {
        write(num);
        write("\n");
        num = num + 1;
        fnd();
    } else {
        write("d passed\n");
    }
}

int main() {
    fna();
    fnb();
    fnc();
    num = 0;
    fnd();
    return 0;
}
