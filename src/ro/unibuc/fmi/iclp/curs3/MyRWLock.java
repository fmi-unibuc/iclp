package ro.unibuc.fmi.iclp.curs3;

import java.sql.Time;
import java.util.Date;
import java.util.concurrent.Semaphore;
import java.util.concurrent.TimeUnit;
import java.util.concurrent.locks.Condition;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReadWriteLock;
import java.util.concurrent.locks.ReentrantLock;

public class MyRWLock implements ReadWriteLock {
    private Lock readMutex = new ReentrantLock();
    private int readersCount = 0;
    private Semaphore writeMutex  = new Semaphore(1);

    @Override
    public Lock readLock() {
        return new Lock() {
            @Override
            public void lock() {
                readMutex.lock();
                if (readersCount == 0) // first reader blocks writers
                    writeMutex.acquireUninterruptibly();
                readersCount++;
                readMutex.unlock();
            }

            @Override
            public void lockInterruptibly()
                    throws InterruptedException {
                readMutex.lockInterruptibly();
                readersCount++;
                if (readersCount == 1)
                    writeMutex.acquire();
                readMutex.unlock();
            }

            @Override
            public boolean tryLock() {
                if (readMutex.tryLock()) {
                    if (readersCount == 0
                            && !writeMutex.tryAcquire()) return false;
                   readersCount++;
                   readMutex.unlock();
                   return true;
                }
                return false;
            }

            @Override
            public boolean tryLock(long l, TimeUnit timeUnit)
                    throws InterruptedException {
                if (readMutex.tryLock(l, timeUnit)) {
                    if (readersCount == 0
                            && !writeMutex.tryAcquire(l, timeUnit))
                        return false;
                    readersCount++;
                    readMutex.unlock();
                    return true;
                }
                return false;
            }

            @Override
            public void unlock() {
                readMutex.lock();
                readersCount--;
                if (readersCount == 0)
                    writeMutex.release();
                readMutex.unlock();
            }

            @Override
            public Condition newCondition() {
                throw new UnsupportedOperationException();
            }
        };
    }

    @Override
    public Lock writeLock() {
        return new Lock() {
            Semaphore next = new Semaphore(0);
            int next_count = 0;
            @Override
            public void lock() {
                writeMutex.acquireUninterruptibly();
            }

            @Override
            public void lockInterruptibly() throws InterruptedException {
                writeMutex.acquire();
            }

            @Override
            public boolean tryLock() {
                return writeMutex.tryAcquire();
            }

            @Override
            public boolean tryLock(long l, TimeUnit timeUnit) throws InterruptedException {
                return writeMutex.tryAcquire(l, timeUnit);
            }

            @Override
            public void unlock() {
                if (next_count > 0)
                    next.release();
                else writeMutex.release();
            }

            @Override
            public Condition newCondition() {
                return new Condition() {
                    Semaphore cond_mutex = new Semaphore(0);
                    int cond_count = 0;

                    @Override
                    public void await() throws InterruptedException {
                        cond_count++;
                        unlock();
                        cond_mutex.acquire();
                        cond_count--;
                    }

                    @Override
                    public void awaitUninterruptibly() {
                        throw new UnsupportedOperationException();
                    }

                    @Override
                    public long awaitNanos(long l) throws InterruptedException {
                        cond_count++;
                        unlock();
                        if (cond_mutex.tryAcquire(l, TimeUnit.NANOSECONDS)) {
                            cond_count--;
                            return 1;
                        } else { // reacquire lock
                            lock();
                            return 0;
                        }
                    }

                    @Override
                    public boolean await(long l, TimeUnit timeUnit) throws InterruptedException {
                        return (awaitNanos(timeUnit.toNanos(l)) > 0);
                    }

                    @Override
                    public boolean awaitUntil(Date date) throws InterruptedException {
                        return false;
                    }

                    @Override
                    public void signal() {
                        if (cond_count > 0) {
                            next_count ++;
                            cond_mutex.release();
                            next.acquireUninterruptibly();
                            next_count--;
                        }
                    }

                    @Override
                    public void signalAll() {
                        if (cond_count > 0) {
                            next_count += cond_count;
                            cond_mutex.release(cond_count);
                            next.acquireUninterruptibly(cond_count);
                            next_count -= cond_count;
                        }
                    }
                };
            }
        };
    }
}
