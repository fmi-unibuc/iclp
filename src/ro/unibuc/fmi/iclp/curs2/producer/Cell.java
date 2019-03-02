package ro.unibuc.fmi.iclp.curs2.producer;

import java.util.Optional;

public class Cell<T> implements DropBox<T> {
  private T message;
  private boolean empty = true;
  private boolean open = true;

  @Override
  public synchronized boolean put(T message)
      throws InterruptedException {
    while (!empty && open) wait(1000);
    if (!empty) return false;
    this.message = message;
    empty = false;
    notifyAll();
    return true;
  }

  @Override
  public synchronized Optional<T> take()
      throws InterruptedException {
    while (empty && open) wait(1000);
    if (empty) return  Optional.empty();
    empty = true;
    notifyAll();
    return Optional.of(message);
  }

  @Override
  public synchronized void close() {
    open = false;
  }

}
