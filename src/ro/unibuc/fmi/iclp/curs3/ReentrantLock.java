package ro.unibuc.fmi.iclp.curs3;

import java.util.concurrent.Semaphore;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.locks.Condition;
import java.util.concurrent.locks.Lock;

public class ReentrantLock implements Lock {
    private Thread owner = null;
    private int holdCount = 0;
    Semaphore mutex = new Semaphore(1);
    Semaphore critical = new Semaphore(1);

    @Override
    public void lock() {
        mutex.acquireUninterruptibly();
        if (isHeldByCurrentThread()) {
            holdCount++;
            mutex.release();
            return;
        }
        mutex.release();
        critical.acquireUninterruptibly();
        owner = Thread.currentThread();
        holdCount = 0;
    }

    public boolean isHeldByCurrentThread() {
        return owner == Thread.currentThread();
    }

    @Override
    public void lockInterruptibly() throws InterruptedException {
        throw new UnsupportedOperationException();
    }

    @Override
    public boolean tryLock() {
        throw new UnsupportedOperationException();
    }

    @Override
    public boolean tryLock(long l, TimeUnit timeUnit) throws InterruptedException {
        throw new UnsupportedOperationException();
    }

    @Override
    public void unlock() {
        mutex.acquireUninterruptibly();
        if (isHeldByCurrentThread()) {
            holdCount--;
            if (holdCount == 0) critical.release();
            mutex.release();
            return;
        }
        mutex.release();
        throw new IllegalMonitorStateException("Attempting to release lock not held by thread");
    }

    @Override
    public Condition newCondition() {
        throw new UnsupportedOperationException();
    }
}
