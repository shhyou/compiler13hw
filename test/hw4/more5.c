typedef int b;

int f() {
  {
    typedef int a;
  }
  {
    typedef float a; 
    typedef int a;
  }
  {
    a XD = 0;
    return XD;
  }
}
