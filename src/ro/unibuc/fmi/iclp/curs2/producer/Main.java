package ro.unibuc.fmi.iclp.curs2.producer;

import java.util.Arrays;

public class Main {

  public static void main(String [] args) throws InterruptedException {
    DropBox<String> box = new Cell<>();
    ProducerThread<String> p = new ProducerThread<>(box,
        Arrays.asList("This", "is", "important"));
    ConsumerThread<String> c = new ConsumerThread<>(box, message ->
      System.out.format("%s received %s%n",
          Thread.currentThread().getName(), message)
    );
    Thread[] pt = new Thread[100];
    Thread[] ct = new Thread[100];
    int threads = 100;
    for (int i = 0; i < threads; i++) {
      pt[i] = new Thread(p, "Producator " + i);
      ct[i] = new Thread(c, "Consumator " + i);
      pt[i].start(); ct[i].start();
    }
    for (int i = 0; i < threads; i++)
      pt[i].join();
    box.close();
    for (int i = 0; i < threads; i++)
      ct[i].join();
    System.out.println("DONE");
  }
}
