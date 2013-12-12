#include<cstdio>
#include<vector>

using namespace std;

struct uniq_name {
  template<typename T = int, typename U, typename V>
    T operator()(U x, V y) { return x + y; }
};

template<typename T> vector<T> vec {[](auto x){ return x; }};
/* let xs = [id] in (head xs 1, head xs True) */

int main() {
  uniq_name strct = uniq_name();
  printf("%f\n", strct.operator()<float,float,float>(1,2));
  printf("%d\n", strct.operator()(1,2));
  return 0;
}
