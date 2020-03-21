package ro.unibuc.fmi.iclp.two;

public class SynchronizedMethod implements Runnable {
    private int c = 0;
    public static void main(String[] args)
            throws InterruptedException {
        SynchronizedMethod sm = new SynchronizedMethod();
        Thread t1 = new Thread(sm); Thread t2 = new Thread(sm);
        t1.start(); t2.start(); t1.join(); t2.join();
        System.out.println("c = " + sm.c);
    }

    @Override public void run() {
        for (int x = 0; x < 5000; ++x) incrementC();
    }

    synchronized void incrementC() { c++; }
}

