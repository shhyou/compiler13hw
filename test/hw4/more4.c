typedef int a[2];
typedef a b[3];
typedef b c[4];

void f(c C) {
  f();
}

typedef float c;
typedef d c[5];

void h(){
  int arr[7][4][3][2];
  f(arr[0]);
}
