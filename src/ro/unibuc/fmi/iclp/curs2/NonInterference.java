package ro.unibuc.fmi.iclp.curs2;

import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

public class NonInterference {
  static int c;
  public static void main(String[] args)
      throws InterruptedException {
    final Lock lock = new ReentrantLock();
    Thread myThread = new Thread(() -> {
      for (int x = 0; x < 5000; ++x) {
        lock.lock(); try {
          if (x % 1000 == 0)
            System.out.println("thread: " + c);
          c++;
        } finally { lock.unlock(); }
      }});
    myThread.start();
    for (int x = 0; x < 5000; ++x) {
      lock.lock(); try {
        c--;
        if (x % 1000 == 0)
          System.out.println("main: " + c);
      } finally { lock.unlock(); }
    }
    myThread.join();
    System.out.println("c = " + c);
  }
}
