package ro.unibuc.fmi.iclp.curs1;

public class NonInterference {
  static int[] c = { 0 };
  static Object lock = new Object();
  public static void main(String[] args)
      throws InterruptedException {
    Thread myThread = new Thread(() -> {
      for (int x = 0; x < 5000; ++x)
        synchronized (c) {
           if (x % 1000 == 0)
             System.out.println("thread: " + c[0]);
           c[0]++;
      }
    });
    myThread.start();
    for (int x = 0; x < 5000; ++x)
      synchronized (c) {
        c[0]--;
        if (x % 1000 == 0)
          System.out.println("main: " + c[0]);

    }
    myThread.join();
    System.out.println("c = " + c[0]);
  }
}
