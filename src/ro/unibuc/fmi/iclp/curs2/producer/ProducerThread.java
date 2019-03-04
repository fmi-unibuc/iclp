package ro.unibuc.fmi.iclp.curs2.producer;

import java.util.Collection;
import java.util.Random;

public class ProducerThread<T> implements Runnable {
  private final DropBox<T> box;
  private Collection<T> items;

  public ProducerThread(DropBox box, Collection<T> items) {
    this.box = box;
    this.items = items;
  }

  @Override public void run() {
    Random rnd = new Random();
    try {
      for (T item : items) {
        Thread.sleep(rnd.nextInt(2000));
//        System.out.println(Thread.currentThread().getName() + " produced " + item);
        box.put(item);
      }
    } catch (InterruptedException e) {
      System.err.println("Got interrupted!");
    }
  }
}
