int main() {
    int i;
    float f;
    i = read();
    f = fread();

    write(i);
    write("\n");

    write(f);
    write("\n");

    i = 10;
    while (i > 0) {
        f = fread();
        write(f);
        write("\n");
        i = i - 1;
    }
    return 0;
}
