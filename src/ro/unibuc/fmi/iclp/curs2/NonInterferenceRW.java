package ro.unibuc.fmi.iclp.curs2;

import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReadWriteLock;
import java.util.concurrent.locks.ReentrantLock;
import java.util.concurrent.locks.ReentrantReadWriteLock;

public class NonInterferenceRW {
  static int c;
  public static void main(String[] args)
      throws InterruptedException {
    final ReadWriteLock lock = new ReentrantReadWriteLock();
    Thread incThread = new Thread(() -> {
      for (int x = 0; x < 5000; ++x) {
        lock.writeLock().lock(); try {
            for (int i = 0; i < 5; i++)
              { c++; Thread.yield(); }
        } finally { lock.writeLock().unlock(); }
      }});
    Thread decThread = new Thread(() -> {
      for (int x = 0; x < 5000; ++x) {
        lock.writeLock().lock(); try {
            for (int i = 0; i < 5; i++)
              { c--; Thread.yield(); }
        } finally { lock.writeLock().unlock(); }
      }});
    Runnable reader = () -> {
      lock.readLock().lock();
      try {
        System.out.println(Thread.currentThread().getName() + " counter: " + c);
      } finally { lock.readLock().unlock(); }
    };
    incThread.start();
    decThread.start();
    ExecutorService executorService = Executors.newFixedThreadPool(2);
    for (int i = 0; i < 100; i++) executorService.execute(reader);
    executorService.shutdown();
    while (!executorService.isTerminated()) {
      executorService.awaitTermination(1, TimeUnit.SECONDS);
    }
    incThread.join();
    decThread.join();
    System.out.println("c = " + c);
  }
}
