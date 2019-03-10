package ro.unibuc.fmi.iclp.curs3;

import java.util.Date;
import java.util.concurrent.Semaphore;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.locks.Condition;
import java.util.concurrent.locks.Lock;

public class MonitorLock implements Lock {
    Thread owner = null;
    Semaphore next = new Semaphore(0);
    int next_count = 0;
    Semaphore mutex = new Semaphore(1);

    @Override
    public void lock() {
        mutex.acquireUninterruptibly();
        owner = Thread.currentThread();
    }

    @Override
    public void lockInterruptibly() throws InterruptedException {
        mutex.acquire();
        owner = Thread.currentThread();
    }

    @Override
    public boolean tryLock() {
        return false;
    }

    @Override
    public boolean tryLock(long l, TimeUnit timeUnit) throws InterruptedException {
        return false;
    }

    @Override
    public void unlock() {
        if (owner != Thread.currentThread())
            throw new IllegalMonitorStateException();
        owner = null;
        if (next_count > 0) next.release();
        else mutex.release();
    }

    @Override
    public Condition newCondition() {
        return null;
    }

    public class MonitorConditon implements Condition {
        Semaphore cond_mutex = new Semaphore(0);
        int cond_count = 0;

        @Override
        public void await() throws InterruptedException {
            cond_count++;
            unlock();
            cond_mutex.acquire();
            owner = Thread.currentThread();
            cond_count--;
        }

        @Override
        public void awaitUninterruptibly() {
            throw new UnsupportedOperationException();
        }

        @Override
        public long awaitNanos(long l) throws InterruptedException {
            return 0;
        }

        @Override
        public boolean await(long l, TimeUnit timeUnit) throws InterruptedException {
            return false;
        }

        @Override
        public boolean awaitUntil(Date date) throws InterruptedException {
            return false;
        }

        @Override
        public void signal() {
            if (cond_count > 0) {
                next_count++;
                cond_mutex.release();
                next.acquireUninterruptibly();
                next_count--;
            }
        }

        @Override
        public void signalAll() {

        }
    }
}


