package ro.unibuc.fmi.iclp.curs3;

import java.util.concurrent.atomic.AtomicInteger;

public class NonInterferenceAtomic {
  static AtomicInteger c = new AtomicInteger(0);
  public static void main(String[] args)
      throws InterruptedException {
    Thread myThread = new Thread(() -> {
      for (int x = 0; x < 5000; ++x) c.incrementAndGet();
    });
    myThread.start();
    for (int x = 0; x < 5000; ++x) c.decrementAndGet();
    myThread.join();
    System.out.println("c = " + c.get());
  }
}
