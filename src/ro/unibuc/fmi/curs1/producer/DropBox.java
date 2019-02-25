package ro.unibuc.fmi.curs1.producer;

import java.util.Optional;

public interface DropBox<T> {
  /**
   * Puts a message in the dropbox, if dropbox open
   * @param message message to be added to dropbox
   * @return whether the operation succeeded (dropbox open)
   * @throws InterruptedException
   */
  boolean put(T message) throws InterruptedException;

  /**
   * Takes a message from the dropbox, if dropbox open
   * @return an optional message, if dropbox open
   * @throws InterruptedException
   */
  Optional<T> take() throws InterruptedException;

  /**
   * Closes the dropbox, making subsequent calls invalid
   */
  void close();
}
