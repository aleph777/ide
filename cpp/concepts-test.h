/* =============================== -*- C++ -*- ================================
  Copyright © 2026 - 2026 - <<<COMPANY>>>  All Rights Reserved.

  Redistribution of this file, in original or modified form, without
  prior written consent of 10Beauty is prohibited.

-------------------------------------------------------------------------------

============================================================================ */

#ifndef CONCEPTS_TEST_H_
#define CONCEPTS_TEST_H_

#include <concepts>
#include <string>

template <typename T, typename Data>
concept PersistentStorage = requires(T s, const Data &d, std::string key) {
  // Must be able to save data
  { s.save(key, d) } -> std::same_as<bool>;

  // Must be able to load data
  { s.load(key) } -> std::same_as<Data>;

  // Must check if data exists
  { s.exists(key) } -> std::convertible_to<bool>;

  // Must have a way to clear/reset persistence
  { s.clear() } -> std::same_as<void>;
};

struct UserData {
    int id;
    std::string name;
};

// Implementation using a simple local file/map
struct LocalFileStorage {
    bool save(std::string key, const UserData& d) {
        /* Write to disk... */
        return true;
    }
    UserData load(std::string key) {
        return {1, "John Doe"};
    }
    bool exists(std::string key) { return true; }
    void clear() { /* Delete files... */ }
};

template <typename Data, PersistentStorage<Data> StorageType>
class UserManager {
private:
    StorageType& storage; // Reference to the injected storage

public:
    // The dependency is injected here
    UserManager(StorageType& s) : storage(s) {}

    void update_user(const UserData& user) {
        if (storage.exists("current_user")) {
            storage.save("current_user", user);
        }
    }
};

template <typename T>
concept ThreadSafe = requires(T t) {
    { t.lock() } -> std::same_as<void>;
    { t.unlock() } -> std::same_as<void>;
};

// A dependency that MUST be both Persistent and ThreadSafe
template <typename T, typename D>
concept SecureStorage = PersistentStorage<T, D> && ThreadSafe<T>;

template <typename D, SecureStorage<D> S>
class SecureProcessor {
    S& storage;
    // ...
};

#endif
