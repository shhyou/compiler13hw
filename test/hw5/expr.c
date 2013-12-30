int main() {
    int i, j, k;
    float l, m, n;

    int o;
    float p;

    i = 1;
    j = 2;
    k = 3;

    l = 1.0;
    m = 2.0;
    n = 3.0;

    o = (i+j)*k;
    write(o);
    write("\n");

    p = (l+m)*n;
    write(p);
    write("\n");

    o = i / k;
    write(o);
    write("\n");

    p = l / n;
    write(p);
    write("\n");

    p = m*n - l*4.0 - 5.0;
    write(p);
    write("\n");

    return 0;
}
