package ro.unibuc.fmi.iclp.curs1;

public class MessageLoop implements Runnable {
  public void run() {
    String importantInfo[] =
        {"This", "is", "very", "important"};
    try {
      for (int i = 0; i < importantInfo.length; i++) {
        Thread.sleep(2000);//Pause for 4 seconds
        threadMessage(importantInfo[i]);
      }
    } catch (InterruptedException e) {
      threadMessage("I wasn't done!");
    }
  }

  public static void threadMessage(String message) {
    String threadName = Thread.currentThread().getName();
    System.out.format("%s: %s%n", threadName, message);
  }

  public static void main(String args[])
      throws InterruptedException {
    threadMessage("Starting MessageLoop thread");
    Thread t = new Thread( new MessageLoop(), "hread-ul meu");
    t.start();
    threadMessage("Waiting for MessageLoop thread to finish");
    t.join();
    threadMessage("Finally!");
  }
}
