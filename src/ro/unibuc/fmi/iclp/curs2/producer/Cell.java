package ro.unibuc.fmi.iclp.curs2.producer;

import java.util.Optional;
import java.util.concurrent.locks.Condition;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

public class Cell<T> implements DropBox<T> {
  private T cell = null;
  private boolean open = true;
  private final Lock lock;
  private final Condition fullCond;
  private final Condition emptyCond;

  public Cell() {
    lock = new ReentrantLock();
    fullCond = lock.newCondition();
    emptyCond = lock.newCondition();
  }

  @Override
  public boolean put(T message)
      throws InterruptedException {
    lock.lock();
    try {
      while (open && cell != null) fullCond.await();
      if (!open) return false;
      cell = message;
      emptyCond.signal();
//    System.out.println(Thread.currentThread().getName() + " put " + message);
      return true;
    } finally {
      lock.unlock();
    }
  }

  @Override
  public Optional<T> take()
      throws InterruptedException {
    lock.lock();
    try {
      while (open && cell == null) emptyCond.await();
      if (open) fullCond.signal();
      T message = cell;
      cell = null;
      return Optional.ofNullable(message);
    } finally {
      lock.unlock();
    }
  }

  @Override
  public void close() {
    lock.lock();
    try {
      open = false;
      emptyCond.signalAll();
      fullCond.signalAll();
    } finally {
      lock.unlock();
    }
  }

}
