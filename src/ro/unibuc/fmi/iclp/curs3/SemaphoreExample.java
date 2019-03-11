package ro.unibuc.fmi.iclp.curs3;

import java.util.concurrent.Semaphore;

public class SemaphoreExample {

  static class Resource implements Runnable {
    private Semaphore semaphore = new Semaphore(3);

    @Override public void run() {
      try {
        System.out.println(
            Thread.currentThread().getName()
            + " requests resource.");
        semaphore.acquire();
        System.out.println(
            Thread.currentThread().getName()
            + " acquired resource.");
        try {
          for (int i = 0; i < 3; i++) {
            System.out.println(
                Thread.currentThread().getName() +
                ": performs operation " + i);
            Thread.sleep(1000);
          }
        } finally {
          System.out.println(
            Thread.currentThread().getName()
            + " releases resource.");
          semaphore.release();
        }
      } catch (InterruptedException e) {
        System.out.println("Got interrupted!");
      }
    }
  }

  public static void main(String[] args)
          throws InterruptedException {
    Runnable resource = new Resource();
    Thread[] threads = new Thread[4];
    for (int i = 0; i < 4; i++) {
      threads[i] = new Thread(resource, "Thread " + i);
      threads[i].start();
    }
    for (int i = 0; i < 4; i++) {
      threads[i].join();
    }

  }
}
