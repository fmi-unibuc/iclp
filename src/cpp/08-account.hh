#include <mutex>
#include <thread>
#include <iostream>
#define SYSTEM_FAILURE_RATE 999

int throws;

struct Account {
    explicit Account(const char* _name, int initial)
        : name{_name}, balance{initial} {}
 
    int balance;
    std::string name;
};

bool withdraw(Account & from, int num) {
    atomic_noexcept {
        if (from.balance < num) return false;
        //std::this_thread::yield(); 
        from.balance -= num;
        return true;
    }
}

void deposit(Account & to, int num) {
    atomic_noexcept {
        to.balance += num;
    }
}

bool transfer(Account &from, Account &to, int num)
{
    static int count = 0;
    atomic_noexcept {
        count++;
        bool ok = withdraw(from, num);
        if (count % SYSTEM_FAILURE_RATE == 0) {
            throws++;
            throw std::runtime_error("system failure!");
        }
        // ^^^ expose non-atomicity in 04-transfer.cc
        if (ok) {
            deposit(to, num);
        }
        return ok; 
    }
}

void balance(Account &acct) {
    std::cout << "Account " << acct.name 
        << " balance: " << acct.balance << std::endl;

}