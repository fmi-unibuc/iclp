#include <mutex>
#include <thread>
#include <iostream>
struct Account {
    explicit Account(const char* _name, int initial)
        : name{_name}, balance{initial} {}
 
    int balance;
    std::string name;
};
 
bool transfer(Account &from, Account &to, int num)
{
    synchronized {
        if (from.balance < num) return false;
        from.balance -= num;
        to.balance += num;
        return true;
    }
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