package ro.unibuc.fmi.iclp.two.pc;

import java.util.Collection;
import java.util.Random;

public class ProducerThread<T> implements Runnable {
    private final DropBox<T> box;
    private Collection<T> items;

    public ProducerThread(DropBox box, Collection<T> items)
    { this.box = box; this.items = items; }

    @Override public void run() {
        Random rnd = new Random();
        try {
            for (T item : items)
            { box.put(item); Thread.sleep(rnd.nextInt(2000)); }
        } catch (InterruptedException e) {
            System.err.println("Got interrupted!");
        }
    }
}
