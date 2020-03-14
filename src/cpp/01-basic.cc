#include <iostream>
#include <thread>
using namespace std;

int main() {
    auto t1 = thread([]{
        cout << "Hello" << endl;
    });
    auto t2 = thread([]{
        cout << "World" << endl;
    });
    t1.join();
    t2.join();
}