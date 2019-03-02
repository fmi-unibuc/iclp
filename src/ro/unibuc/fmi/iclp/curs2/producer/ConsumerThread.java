package ro.unibuc.fmi.iclp.curs2.producer;

import java.util.Optional;
import java.util.function.Consumer;

public class ConsumerThread<T> implements Runnable {
  private final DropBox<T> box;
  private final Consumer<T> handle;

  public ConsumerThread(DropBox<T> box, Consumer<T> handle) {
    this.box = box;
    this.handle = handle;
  }

  @Override public void run() {
    try {
      Optional<T> message = box.take();
      while (message.isPresent()) {
          handle.accept(message.get());
          message = box.take();
      }
    } catch (InterruptedException e) {
      System.err.println("Got interrupted!");
    }
  }
}
