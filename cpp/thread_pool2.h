// -*- C++ -*-
#include <condition_variable>
#include <functional>
#include <future>
#include <iostream>
#include <mutex>
#include <queue>
#include <thread>
#include <vector>

class ThreadPool {
public:
  explicit ThreadPool(size_t threads) {
    for (size_t i = 0; i < threads; ++i) {
      // jthread handles automatic joining on destruction
      workers.emplace_back([this](std::stop_token stop_tok) {
        while (!stop_tok.stop_requested()) {
          std::move_only_function<void()> task;
          {
            std::unique_lock lock(queue_mutex);
            condition.wait(lock, stop_tok, [this] {
              return !tasks.empty();
            });
            if (tasks.empty()) continue;

            task = std::move(tasks.front());
            tasks.pop();
          }
          task();
        }
      });
    }
  }

  // Submit a function and return a future to its result
  template <typename F, typename... Args>
  auto enqueue(F&& f, Args&&... args) -> std::future<std::invoke_result_t<F, Args...>> {
    using return_type = std::invoke_result_t<F, Args...>;

    auto task = std::make_shared<std::promise<return_type>>();
    auto future = task->get_future();

    {
      std::lock_guard lock(queue_mutex);
      // C++23 move_only_function allows us to capture the promise/task directly
      tasks.emplace([f = std::forward<F>(f), ...args = std::forward<Args>(args), task]() mutable {
        try {
          if constexpr (std::is_void_v<return_type>) {
            std::invoke(std::move(f), std::move(args)...);
            task->set_value();
          } else {
            task->set_value(std::invoke(std::move(f), std::move(args)...));
          }
        } catch (...) {
          task->set_exception(std::current_exception());
        }
      });
    }

    condition.notify_one();
    return future;
  }

private:
  std::vector<std::jthread> workers;
  // C++23 move_only_function: The "Secret Sauce"
  std::queue<std::move_only_function<void()>> tasks;

  std::mutex queue_mutex;
  std::condition_variable_any condition;
};
