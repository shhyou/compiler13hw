int g;

int main() {
    typedef int INT;
    typedef float FLOAT;

    int l;
    INT tint;
    FLOAT tfloat;

    int arr[2];

    g = 1;
    l = 2;
    tint = 3;
    tfloat = 4.0;
    arr[0] = 5;
    arr[1] = 6;

    write(g);
    write("\n");

    write(l);
    write("\n");

    write(tint);
    write("\n");

    write(tfloat);
    write("\n");

    l = arr[0];
    write(l);
    write("\n");

    l = arr[1];
    write(l);
    write("\n");

    return 0;
}
