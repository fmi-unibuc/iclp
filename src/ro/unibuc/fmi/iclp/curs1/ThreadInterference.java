package ro.unibuc.fmi.iclp.curs1;

public class ThreadInterference {
  private static Integer counter = 0;
  public static void main (String[] args)
      throws InterruptedException {
    Thread thread1 = new Thread(new Task());
    Thread thread2 = new Thread(new Task());
    thread1.start(); thread2.start();
    thread1.join(); thread2.join();
    System.out.println(counter);
  }

  private static class Task implements Runnable {
    public void run () {
      for (int i = 0; i < 400; i++) {
        int temp = counter++;
        if (i % 100 == 0)
          System.out.println(Thread.currentThread().getName() +
              " - before: " + temp + " after: " + counter);
      }
    }
  }
}
