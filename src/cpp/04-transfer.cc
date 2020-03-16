#include "04-account.hh"
#include<vector>
 
int main()
{
    std::srand(std::time(nullptr)); // use current time as seed for random generator
    SYSTEM_FAILURE_RATE = std::rand()%1900+100;

    Account acc1("Ionel", 1000);
    Account acc2("Gigel", 1000);

    std::vector<std::thread> v(1000);
    for(auto& t: v) t = std::thread([&]{ try {transfer(acc1, acc2, 1);} catch (...) {} });
    for(auto& t: v) t.join();
    balance(acc1);
    balance(acc2);
    std::cout << "Total sum: " << acc1.balance + acc2.balance << std::endl;
    std::cout << "Total throws: " << throws << std::endl;
}