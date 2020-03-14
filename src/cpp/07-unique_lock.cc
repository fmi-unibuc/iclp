#include <mutex>
#include <thread>
#include <iostream>
struct Account {
    explicit Account(const char* _name, int initial)
        : name{_name}, balance{initial} {}
 
    int balance;
    std::string name;
    std::mutex g_mutex;
};
 
bool transfer(Account &from, Account &to, int num)
{
    // don't actually take the locks yet
    std::unique_lock<std::mutex> lock1(from.g_mutex, std::defer_lock);
    std::unique_lock<std::mutex> lock2(to.g_mutex, std::defer_lock);
 
    // lock both unique_locks without deadlock
    std::lock(lock1, lock2);

    if (from.balance < num) return false;
    from.balance -= num;
    to.balance += num;
    return true;
    // mutexes unlocked in 'unique_lock' dtors
}

void balance(Account &acct) {
    std::cout << "Account " << acct.name 
        << " balance: " << acct.balance << std::endl;

}
 
int main()
{
    Account acc1("Ionel", 100);
    Account acc2("Gigel", 50);
    balance(acc1);
    balance(acc2);
    std::cout << "Initial sum: " << acc1.balance + acc2.balance << std::endl;
 
    std::thread t1([&]{
        for (int i=0; i<100; i++) transfer(acc1, acc2, 2);
    });
    std::thread t2([&]{
        for (int i=0; i<100; i++) transfer(acc2, acc1, 1);
    });
 
    t1.join();
    t2.join();
    balance(acc1);
    balance(acc2);
    std::cout << "Total sum: " << acc1.balance + acc2.balance << std::endl;
}