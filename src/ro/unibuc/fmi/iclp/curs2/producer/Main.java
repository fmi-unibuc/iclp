package ro.unibuc.fmi.iclp.curs2.producer;

import java.util.Arrays;

public class Main {

  public static void main(String [] args) throws InterruptedException {
    DropBox<String> box = new Cell<>();
    ProducerThread<String> p1 = new ProducerThread<>(box,
        Arrays.asList("This", "is", "important"));
    ProducerThread<String> p2 = new ProducerThread<>(box,
        Arrays.asList("so", "incredibly", "much", "highly"));
    ConsumerThread<String> c = new ConsumerThread<>(box, message ->
      System.out.format("%s received %s%n",
          Thread.currentThread().getName(), message)
    );
    Thread pt1 = new Thread(p1); Thread pt2 = new Thread(p2);
    Thread ct1 = new Thread(c); Thread ct2 = new Thread(c);
    pt1.start(); pt2.start(); ct1.start(); ct2.start();
    pt1.join(); pt2.join();
    box.close();
    ct1.join();ct2.join();
    System.out.println("DONE");
  }
}
