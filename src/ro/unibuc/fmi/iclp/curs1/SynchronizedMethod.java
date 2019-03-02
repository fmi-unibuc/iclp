package ro.unibuc.fmi.iclp.curs1;

public class SynchronizedMethod extends Thread {

  public static void main(String[] args)
      throws InterruptedException {
    new SynchronizedMethod().start();
  }

  public void run() {
    Container cc1 = new Container();
    Container cc2 = new Container();
    Thread t1 = new Thread( () -> {
      for (int i = 0; i < 5000; i++)
        cc1.increment();
    }
    );
    Thread t2 = new Thread( () -> {
      for (int i = 0; i < 5000; i++)
        cc2.decrement();
    }
    );
    t1.start(); t2.start();
    try {
      t1.join();
      t2.join();
    } catch (InterruptedException e) {
      e.printStackTrace();
    }
    System.out.println("c = " + Container.c);
  }

}
