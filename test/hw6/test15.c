void gunc(float f) {
    write(f);
    write("\n");
}


int func(int a, int b, float c, float d) {
    write(a);
    write("\n");
    write(b);
    write("\n");
    write(c);
    write("\n");
    write(d);
    write("\n");
    return 0;
}

int ret(int n) {
    return n;
}

float retf(float n) {
    return n;
}

int main() {
    int a = 0;
    float d = 3.0;

    write("test1:\n");
    gunc(1.0);

    write("test2:\n");
    func(a, 1, 2.0, d);

    write("test3:\n");
    func(1-1, a+1, 3.0-1.0, d-0.0);

    write("test4:\n");
    func(ret(1-1), ret(a+1), retf(3.0-1.0), retf(d-0.0));
}
