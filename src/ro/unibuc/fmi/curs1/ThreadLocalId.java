package ro.unibuc.fmi.curs1;

public class ThreadLocalId implements Runnable {
    private ThreadLocal<Long> threadLocal = new ThreadLocal<>();
    private long shared = 0;

    public void run() {
      threadLocal.set(Thread.currentThread().getId());
      System.out.format("Name: %s Id: %d%n",
          Thread.currentThread().getName(), threadLocal.get());
      threadLocal.set(threadLocal.get()+1);
    }

  public static void main(String[] args) throws InterruptedException {
    ThreadLocalId sharedRunnableInstance = new ThreadLocalId();

    Thread thread1 = new Thread(sharedRunnableInstance);
    Thread thread2 = new Thread(sharedRunnableInstance);

    thread1.start(); thread2.start();
    thread1.join(); thread2.join();
  }
}
