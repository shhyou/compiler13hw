int f(float a) {
    return a;
}
float n(int a) {
    return a;
}

int main() {
    int a;
    float b;
    a = 1 + 1.3;
    write(a);
    write("\n");

    b = 1 + 1.3;
    write(b);
    write("\n");

    a = f(1.5);
    write(a);
    write("\n");

    b = n(1);
    write(b);
    write("\n");

    b = n(4.4);
    write(b);
    write("\n");
}
