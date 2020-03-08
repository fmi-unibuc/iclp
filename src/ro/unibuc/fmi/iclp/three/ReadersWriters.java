package ro.unibuc.fmi.iclp.three;

import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.locks.ReadWriteLock;
import java.util.concurrent.locks.ReentrantReadWriteLock;

import static ro.unibuc.fmi.iclp.Helper.*;

public class ReadersWriters {
    static int c;
    public static void main(String[] args)
            throws InterruptedException {
        final ReadWriteLock lock = new ReentrantReadWriteLock();
        Thread incThread = new Thread(() -> {
            for (int x = 0; x < 5000; ++x) {
//                sync(lock.writeLock(), () -> {
                            for (int i = 0; i < 5; i++)
                            { c++; }
//                });
                sleep(1);
            }});
        Thread decThread = new Thread(() -> {
            for (int x = 0; x < 5000; ++x) {
//                sync(lock.writeLock(), () -> {
                    for (int i = 0; i < 5; i++)
                    { c--; }
//                });
                sleep(1);
            }});
        Runnable reader = () -> {
            syncRun(lock.readLock(), () ->
                    System.out.println(Thread.currentThread().getName() + " counter: " + c)
            );
            sleep(10);
        };
        incThread.start();
        decThread.start();
        ExecutorService executorService = Executors.newFixedThreadPool(2);
        for (int i = 0; i < 1000; i++) executorService.execute(reader);
        incThread.join();
        decThread.join();
        executorService.shutdownNow();
        System.out.println("c = " + c);
    }
}