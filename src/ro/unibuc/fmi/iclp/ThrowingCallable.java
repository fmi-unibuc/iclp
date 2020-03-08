package ro.unibuc.fmi.iclp;

@FunctionalInterface
public interface ThrowingCallable<T,E extends Exception> {
    T call() throws E;
}
