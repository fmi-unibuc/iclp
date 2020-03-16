#include <mutex>
#include <thread>
#include <iostream>

int SYSTEM_FAILURE_RATE;
int throws;

struct Account {
    explicit Account(const char* _name, int initial)
        : name{_name}, balance{initial} {}
 
    int balance;
    std::string name;
    std::mutex m_balance;
};

bool withdraw(Account & from, int num) {
    const std::lock_guard<std::mutex> from_lock(from.m_balance);
    // ^^^ fixes bug in 04-withdrawx2.cc
    if (from.balance < num) return false;
    std::this_thread::yield(); // expose bug in 04-withdrawx2.cc
    from.balance -= num;
    return true;
}

void deposit(Account & to, int num) {
    const std::lock_guard<std::mutex> from_lock(to.m_balance);
    // ^^^ fixes bug in 05-depositxn.cc
    to.balance += num;
}

bool transfer(Account &from, Account &to, int num)
{
    static int count = 0;
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

void balance(Account &acct) {
    std::cout << "Account " << acct.name 
        << " balance: " << acct.balance << std::endl;

}