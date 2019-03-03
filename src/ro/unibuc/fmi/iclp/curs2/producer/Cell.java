package ro.unibuc.fmi.iclp.curs2.producer;

import java.util.Optional;

public class Cell<T> implements DropBox<T> {
  private T cell = null;
  private boolean open = true;

  @Override
  public synchronized boolean put(T message)
      throws InterruptedException {
    while (open && cell != null) wait();
    if (!open) return false;
    cell = message;
    notifyAll();
    return true;
  }

  @Override
  public synchronized Optional<T> take()
      throws InterruptedException {
    while (open && cell == null) wait();
    if (open) notifyAll();
    T message = cell;
    cell = null;
    return Optional.ofNullable(message);
  }

  @Override
  public synchronized void close() {
    open = false; notifyAll();
  }

}
