package ro.unibuc.fmi.curs1;

public class MessageLoopInterrupted {
  public static void main(String args[]) throws InterruptedException {
    long patience = 1000 * 5;
    long startTime = System.currentTimeMillis();
    MessageLoop.threadMessage("Starting MessageLoop thread");
    Thread t = new Thread(new MessageLoop());
    t.start();
    MessageLoop.threadMessage("Waiting for MessageLoop thread to finish");
    while (t.isAlive()) {
      MessageLoop.threadMessage("Still waiting...");
      t.join(2000);   // de facut modificarea t.join() si vazut ca asteapta sa scrie
      if (((System.currentTimeMillis() - startTime)
          > patience) && t.isAlive()) {
        MessageLoop.threadMessage("Tired of waiting!");
        t.interrupt();
        t.join();
      }
    }
    MessageLoop.threadMessage("Finally!");
  }

}
