typedef int a[2];
typedef a b[3];
typedef b c[4];

void f(c C, int c) {
  f();
}

typedef float c;
typedef d c[5];

void h(){
  int arr[7][4][3][2];
  typedef float arr; /* redeclared */
  f(arr[0]);
}

int w(int w) { typedef int w; }
int u(int a) { typedef float u; typedef u a; a i; return i; }

void main() {}