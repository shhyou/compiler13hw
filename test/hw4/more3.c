int f(int a, float f[1], float g[1.2][3.4]) {
 return a + f;
}

int g(int k[][1.1][x+1.2][1.3+5]) {
  return k[0][1][2][3];
}

int h(int k[1.3 + 5], int n[5.1]) {
  return k[1]+n[2.1];
}

int main() {
  int a[100];
  float h;
  return f(a, h);
}
