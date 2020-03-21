package ro.unibuc.fmi.iclp;

@FunctionalInterface
public interface ThrowingRunnable<E extends Exception> {
    void run() throws E;
}
