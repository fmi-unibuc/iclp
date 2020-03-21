package ro.unibuc.fmi.iclp.three;

import ro.unibuc.fmi.iclp.two.pc.ConsumerThread;
import ro.unibuc.fmi.iclp.two.pc.DropBox;
import ro.unibuc.fmi.iclp.two.pc.ProducerThread;

import java.util.Arrays;
import java.util.Optional;
import java.util.concurrent.locks.Condition;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

import static ro.unibuc.fmi.iclp.Helper.*;

public class LockCell<T> implements DropBox<T> {
    private T cell = null;
    private boolean open = true;
    Lock lock = new ReentrantLock();
    Condition putWaitList = lock.newCondition();
    Condition takeWaitList = lock.newCondition();

    @Override public void close() {
        syncRun(lock, () -> {
            open = false;
            putWaitList.signalAll();
            takeWaitList.signalAll();
        });
    }

    @Override public boolean put(T message)
            throws InterruptedException {
        return syncTCall(lock, () -> {
            while (open && cell != null) putWaitList.await();
            if (!open) return false;
            cell = message;
            takeWaitList.signal();
            return true;
        });
    }

    @Override public Optional<T> take()
            throws InterruptedException {
        return syncTCall(lock, () -> {
            while (open && cell == null) takeWaitList.await();
            if (open) putWaitList.signal();
            T message = cell;
            cell = null;
            return Optional.ofNullable(message);
        });
    }



    public static void main(String [] args)
            throws InterruptedException {
        DropBox<String> box = new LockCell<>();
        ProducerThread<String> p1 = new ProducerThread<>(box,
                Arrays.asList("This", "is", "important"));
        ProducerThread<String> p2 = new ProducerThread<>(box,
                Arrays.asList("so", "incredibly", "much", "highly"));
        ConsumerThread<String> c = new ConsumerThread<>(box,
                message -> System.out.format("%s received %s%n",
                        Thread.currentThread().getName(), message));
        Thread pt1 = new Thread(p1); Thread pt2 = new Thread(p2);
        Thread ct1 = new Thread(c); Thread ct2 = new Thread(c);
        pt1.start(); pt2.start(); ct1.start(); ct2.start();
        pt1.join(); pt2.join(); box.close();
        ct1.join();ct2.join(); System.out.println("DONE");
    }
}
