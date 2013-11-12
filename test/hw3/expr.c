int fn1() {
    return 1;
}
float fn3() {
    return 1.0;
}
int main() {
    int i, j, k, l;
    float fi = 1.0, fj = 2.0, fk = 3.0, fl = 4.0;

    fi = 1.2*fi + -fj * (fl - fk * fn3());
    fi = -fn3() -(-(-(4)));
    fi = !fn3() -(!(!(4)));
    i = !fn1();
    i = 1 < 2;
    i = 1 > 2;
    i = 1 >= 2;
    i = 1 <= 2;
    i = 1 != 2;
    i = 1 == 2;
}
