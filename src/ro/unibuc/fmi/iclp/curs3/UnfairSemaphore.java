package ro.unibuc.fmi.iclp.curs3;

public class UnfairSemaphore {
    private int permits;
    public UnfairSemaphore(int permits) {
        this.permits = permits;
    }

    public synchronized void acquire() throws InterruptedException {
        while (permits <= 0) wait();
        permits--;
    }

    public synchronized void release() {
        permits++;
        notify();
    }
}
