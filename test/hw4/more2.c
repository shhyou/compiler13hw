int f(int f) { return f; }

/* incompatible type? check? */
int h(int i) { return h; }

/* undeclared type name */
int w() { a var; return var;  }

/*               redeclared */
void g(int g) { int g; }

/* Cannot call `n` */
int n(int n) { int m = n(); }

/* ok */
float k(int m) { int k; }
int main() {
  int a = main;
  int b = b;
  return;
}
