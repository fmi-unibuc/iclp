#include "04-account.hh"
#include<vector>
 
int main()
{
    Account acc1("Ionel", 0);
    std::vector<std::thread> v(1000);
    for(auto& t: v) t = std::thread([&]{ deposit(acc1, 1); });
    for(auto& t: v) t.join();
    balance(acc1);
}