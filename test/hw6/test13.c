int a[1][2][3];

void fn(int a[][3]) {
    write(a[0][0]);
    write(a[0][1]);
    write(a[0][2]);
    write(a[1][0]);
    write(a[1][1]);
    write(a[1][2]);
    write("\n");
}

void fn2(int a[], int b[]) {
    a[0] = a[0] + b[0];
    a[1] = a[1] + b[2];
    a[2] = a[2] + b[2];

    write(a[0]);
    write(a[1]);
    write(a[2]);
    write(b[0]);
    write(b[1]);
    write(b[2]);
    write("\n");
}

int main() {
    float f[2][2];
    f[0][0] = 1.0;
    f[0][1] = 2.0;
    f[1][0] = 3.0;
    f[1][1] = 4.0;
    
    write(f[1][0]);
    write("\n");

    a[0][0][0] = 1;
    a[0][0][1] = 2;
    a[0][0][2] = 3;

    a[0][1][0] = 4;
    a[0][1][1] = 5;
    a[0][1][2] = 6;

    write(a[0][0][0]);
    write(a[0][0][1]);
    write(a[0][0][2]);
    write(a[0][1][0]);
    write(a[0][1][1]);
    write(a[0][1][2]);
    write("\n");


    fn(a[0]);

    fn2(a[0][0], a[0][1]);
}
