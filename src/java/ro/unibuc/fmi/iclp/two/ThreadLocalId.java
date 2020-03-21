package ro.unibuc.fmi.iclp.two;

public class ThreadLocalId implements Runnable {
    private ThreadLocal<Long> threadLocal = new ThreadLocal<>();
    private long notLocal;

    public void run() {
        threadLocal.set(Thread.currentThread().getId());
        notLocal = Thread.currentThread().getId();
        try {
            Thread.sleep(1000);
        } catch (InterruptedException e) {
            e.printStackTrace();
        }
        System.out.format("Name: %s Id: local: %d notLocal: %d%n",
                Thread.currentThread().getName(), threadLocal.get(), notLocal);
    }

    public static void main(String[] args) throws InterruptedException {
        ThreadLocalId sharedRunnable = new ThreadLocalId();
        Thread thread1 = new Thread(sharedRunnable);
        Thread thread2 = new Thread(sharedRunnable);
        thread1.start(); thread2.start();
        thread1.join(); thread2.join();
    }
}
