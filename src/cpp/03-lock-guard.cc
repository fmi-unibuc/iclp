#include <iostream>
#include <vector>
#include <mutex>
#include <thread>
int f(int tID)
{   static int i = 0;
    static std::mutex g_i_mutex;

    const std::lock_guard<std::mutex> lock(g_i_mutex);
    if (tID % 2 == 0) ++i; else --i;
    return i;
}
int main()
{   std::vector<std::thread> v(10);
    for(int i=0; i < v.size(); i++)
        v[i] = std::thread([i]{ for(int n = 0; n < 10; ++n) f(i); });
    for(auto& t: v) t.join();
    std::cout << "Final value: " << f(0)-1 << std::endl;
}
