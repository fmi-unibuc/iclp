package ro.unibuc.fmi.iclp.curs2.producer;

import java.util.Optional;

public interface DropBox<T> {
  /**
   * Puts message in dropbox, if dropbox open
   * @param message to be added to dropbox
   * @return true if operation succeeded (dropbox open)
   * @throws InterruptedException
   */
  boolean put(T message) throws InterruptedException;

  /**
   * Takes a message from the dropbox
   * @return an message, if dropbox open
   * @throws InterruptedException
   */
  Optional<T> take() throws InterruptedException;

  /**
   * Closes the dropbox
   */
  void close() throws InterruptedException;
}
