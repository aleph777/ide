#include <iostream>
#include <random>
#include <vector>

using std::vector;
using std::cout;

int main() {
  std::random_device rd;
  std::mt19937 gen(rd());

  vector<int> vi({1, 2, 3, 4, 5, 6, 7, 8, 9});

  for (auto i = 1; i <= 9; ++i) {
    std::uniform_int_distribution<> distr(0, static_cast<int>(vi.size())-1);

    int num = distr(gen);

    cout << vi[num] << "\n";

    vi.erase(vi.begin()+num);
  }
}
