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

using std::condition_variable;
using std::function;
using std::invoke_result_t;
using std::make_shared;
using std::mutex;
using std::packaged_task;
using std::queue;
using std::scoped_lock;
using std::thread;
using std::unique_lock;
using std::vector;

using QueueTasks    = queue<function<void()>>;
using VectorWorkers = vector<thread>;

class ThreadPool {
public:
  explicit ThreadPool(std::size_t num_threads)
  : stop_(false)
  {
    for (std::size_t i = 0; i < num_threads; ++i) {
      workers_.emplace_back([this] {worker_loop();});
    }
  }

  ~ThreadPool() {
    {
      scoped_lock lock(mutex_);
      stop_ = true;
    }
    cv_.notify_all();

    for (auto& t : workers_) {
      if (t.joinable()) t.join();
    }
  }

  template<typename F, typename... Args>
  auto enqueue(F&& f, Args&&... args)
        -> std::future<invoke_result_t<F, Args...>>
  {
    using ReturnType   = invoke_result_t<F, Args...>;
    using PackagedTask = packaged_task<ReturnType()>;

    auto task = make_shared<PackagedTask>(
      [f = std::forward<F>(f), ...args = std::forward<Args>(args)]() mutable -> decltype(auto) {
        return std::invoke(std::move(f), std::move(args)...);
    });

    std::future<ReturnType> future = task->get_future();

    {
      scoped_lock lock(mutex_);

      if (stop_) throw std::runtime_error("ThreadPool stopped");

      tasks_.emplace([task]() {(*task)();});
    }
    cv_.notify_one();

    return future;
  }

private:
  void worker_loop() {
    while (true) {
      function<void()> task;

      {
        unique_lock lock(mutex_);

        cv_.wait(lock, [this] {
          return stop_ || !tasks_.empty();
        });

        if (stop_ && tasks_.empty()) return;

        task = std::move(tasks_.front());
        tasks_.pop();
      }

      task();
    }
  }

  VectorWorkers workers_;
  QueueTasks tasks_;

  mutex mutex_;
  condition_variable cv_;

  bool stop_;
};
