#include "04-account.hh"
 
int main()
{
    Account acc1("Ionel", 50);
 
    std::thread t1([&]{withdraw(acc1, 50);});
    std::thread t2([&]{withdraw(acc1, 50);});
    t1.join();
    t2.join();
    balance(acc1);
}