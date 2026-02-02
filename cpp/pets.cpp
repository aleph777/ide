// -*- C++ -*-

#include <format>
#include <iostream>
#include <list>


#include "pet-storage.h"

using namespace LocalTest;

using std::format;
using std::list;
using std::string;
using std::cout;

template <typename Data, PersistentStorage<Data> StorageType> class Pet {
public:
  // The dependency is injected here
  Pet(StorageType &s) : storage_(s) {}

  void updateCurrentUser(const Data& d) {
    if (storage_.exists("current_user")) {
      storage_.set("current_user", d);
    }
  }

  bool set(string key, const Data& d) { return storage_.set(key, d); };

  const Data get(string key) { return storage_.get(key); }

  bool exists(string key) { return storage_.exists(key); }

  const list<string> keys() { return storage_.keys(); }

private:
  StorageType& storage_;
};

struct CatData {
  bool scratching_post;
  bool litter_box;
  string yarn_color;
};

struct DogData {
  bool bone;
  bool squeakie;
  bool leash;
};

int main() {
  PetStorage<CatData> cs;
  PetStorage<DogData> ds;

  Pet<CatData, PetStorage<CatData>> kitty(cs);
  Pet<DogData, PetStorage<DogData>> puppy(ds);

  auto cat_set = kitty.set("mittens", CatData(true, true, "red"));
  auto dog_set = puppy.set("bonkers", DogData(true, false, true));

  cout << cat_set << "\n";
  cout << dog_set << "\n";

  auto cat_data = kitty.get("mittens");
  auto dog_data = puppy.get("bonkers");

  cout << cat_data.yarn_color << "\n";
  cout << dog_data.squeakie << "\n";

  cout << kitty.exists("mittens") << "\n";
  cout << puppy.exists("numnums") << "\n";

  kitty.set("fluffy", CatData(true, true, "green"));

  for (auto &key : kitty.keys()) {
    // std::string s = std::format("Hello, {} version {}!", name, version);
    string s(format("Name:{: >8}\n", key));
    cout << s;
  }
}
