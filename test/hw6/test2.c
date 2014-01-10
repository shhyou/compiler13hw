int main() {
    int a,b,f;
    float fnum = 3.2;

    a = read();
    b = read();
    f = a*b;

    if ( !(fnum < a) || b/a  < 0 && !f) {
        write("True\n");
    } else {
        write("False\n");
    }   

    return 0;
}
