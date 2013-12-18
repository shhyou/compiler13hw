int f(int a, float f[1]) {
 return a + f;
}

int g(int a, float b, int c[1], float d[1]) {
  return a + b + c[-1] + d[-1];
}

int main() {
  int a[100];
  float h, k[100];
  a[0] = g(h, a[0], k, a);
  return f(a, h);
}
