#include<vector>
#include<thread>
#include<mutex>
#include<chrono>
#include<iostream> 
 
#define ITERATIONS 1000
#define THINKING_TIME 10
#define EATING_TIME 1

std::mutex forks[5];
int hungry[5];

void think() {
    std::this_thread::sleep_for(std::chrono::milliseconds(THINKING_TIME));
}

void eat() {
    std::this_thread::sleep_for(std::chrono::milliseconds(EATING_TIME));
}

void log(int phil, const char* action) {
//    std::cout << "Philosopher " << phil
//        << " is " << action << std::endl;
}

void philosopher(int phil, std::mutex& left, std::mutex& right) {
    for (int times = 0; times < ITERATIONS; times++) {
        log(phil, "thinking");
        think();
        {
            // don't actually take the locks yet
            std::unique_lock<std::mutex> lockL(left, std::defer_lock);
            std::unique_lock<std::mutex> lockR(right, std::defer_lock);

            // lock both unique_locks without deadlock
            std::lock(lockL, lockR);
            log(phil, "eating");
            eat();
        }
    }
}

int main() {
    std::vector<std::thread> t(5);
    for (int i = 0; i < 4; i++) {
        t[i] = std::thread([i]{philosopher(i+1, forks[i], forks[i+1]);});
    }
    t[4] = std::thread([]{philosopher(5, forks[4], forks[0]);});
    for(auto& thr: t) thr.join();
}