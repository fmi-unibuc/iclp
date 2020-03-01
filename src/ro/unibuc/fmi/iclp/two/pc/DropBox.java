package ro.unibuc.fmi.iclp.two.pc;

import java.util.Optional;

public interface DropBox<T> {
    /**
     * Puts a message in the dropbox
     * @return whether the operation succeeded (dropbox open)
     */
    boolean put(T message) throws InterruptedException;
    /**
     * Takes a message from the dropbox
     * @return an optional message, if dropbox open
     */
    Optional<T> take() throws InterruptedException;
    /**
     * Closes the dropbox, making subsequent calls invalid
     */
    void close();
}


