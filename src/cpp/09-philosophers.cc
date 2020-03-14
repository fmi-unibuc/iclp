#include<vector>
#include<thread>
#include<chrono>
#include<iostream> 
 
#define ITERATIONS 1000
#define THINKING_TIME 10
#define EATING_TIME 1

int forks[5];
int hungry[5];
int last_hungry[5];
int current_hungry[5];
int max_hungry[5];

void think() {
    std::this_thread::sleep_for(std::chrono::milliseconds(THINKING_TIME));
}

void eat() {
    std::this_thread::sleep_for(std::chrono::milliseconds(EATING_TIME));
}

void starving(int phil, int iteration) {
    hungry[phil] ++;
    if (last_hungry[phil] == iteration - 1) {
        current_hungry[phil]++;
    } else {
        current_hungry[phil] = 1;
    }
    if (current_hungry[phil] > max_hungry[phil]) {
        max_hungry[phil] = current_hungry[phil];
    }
    last_hungry[phil] = iteration;
}

bool takeForks(int phil, int& left, int& right) {
    synchronized {
        if (left == 0 && right == 0) {
            left = right = phil;
            return true;
        }
    }
    return false;
}

void releaseForks(int& left, int& right) {
    left = right = 0;
}

void log(int phil, const char* action) {
//    std::cout << "Philosopher " << phil
//        << " is " << action << std::endl;
}

void philosopher(int phil, int& left, int& right) {
    for (int times = 0; times < ITERATIONS; times++) {
        log(phil, "thinking");
        think();
        if (takeForks(phil, left, right)) {
            log(phil, "eating");
            eat();
            releaseForks(left, right);
        } else {
            log(phil, "starving");
            starving(phil - 1, times);
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
    for (int i = 0; i < 5; i++) {
        std::cout << "Philosopher " << i + 1 << " went hungry "
            << hungry[i] << " times." << std::endl
            << "\tMaximum starving in a row: "
            << max_hungry[i] << std::endl;
    }
}