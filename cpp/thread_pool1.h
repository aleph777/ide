// -*- C++ -*-

#include <condition_variable>
#include <cstddef>
#include <functional>
#include <future>
#include <iostream>
#include <mutex>
#include <queue>
#include <thread>
#include <vector>

class ThreadPool {
public:
  explicit ThreadPool(std::size_t num_threads)
  : stop_(false)
  {
    for (std::size_t i = 0; i < num_threads; ++i) {
      workers_.emplace_back([this] {
        worker_loop();
      });
    }
  }

  ~ThreadPool() {
    {
      std::scoped_lock lock(mutex_);
      stop_ = true;
    }

    cv_.notify_all();

    for (auto& t : workers_) {
      if (t.joinable()) {
        t.join();
      }
    }
  }

  template<typename F, typename... Args>
  auto enqueue(F&& f, Args&&... args)
        -> std::future<std::invoke_result_t<F, Args...>>
  {
    using ReturnType   = std::invoke_result_t<F, Args...>;
    using PackagedTask = std::packaged_task<ReturnType()>;

    auto task = std::make_shared<PackagedTask>(
                                               std::bind(std::forward<F>(f),
                                                         std::forward<Args>(args)...)
                                               );

    std::future<ReturnType> future = task->get_future();

    {
      std::scoped_lock lock(mutex_);

      if (stop_) {
        throw std::runtime_error("ThreadPool stopped");
      }

      tasks_.emplace([task]() {
        (*task)();
      });
    }

    cv_.notify_one();

    return future;
  }

private:
  void worker_loop() {
    while (true) {
      std::function<void()> task;

      {
        std::unique_lock lock(mutex_);

        cv_.wait(lock, [this] {
          return stop_ || !tasks_.empty();
        });

        if (stop_ && tasks_.empty()) {
          return;
        }

        task = std::move(tasks_.front());
        tasks_.pop();
      }

      task();
    }
  }

  std::vector<std::thread> workers_;
  std::queue<std::function<void()>> tasks_;

  std::mutex mutex_;
  std::condition_variable cv_;

  bool stop_;
};
