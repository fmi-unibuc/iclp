package ro.unibuc.fmi.iclp.two;

public class NonInterference implements Runnable{
    public int c = 0;

    private synchronized void increment() {
        c ++;
    }

    @Override
    public void run() {
        for (int x = 0; x < 5000; ++x) {
            increment();
        }
    }
    public static void main(String[] args)
            throws InterruptedException {
        NonInterference ni = new NonInterference();
        Thread myThread1 = new Thread(ni);
        Thread myThread2 = new Thread(ni);
        myThread1.start();
        myThread2.start();
        myThread1.join();
        myThread2.join();
        System.out.println("c = " + ni.c);
    }
}
