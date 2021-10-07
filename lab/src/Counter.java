public class Counter {
    static int counter = 0;
    static final int ITERATIONS = 1_000_000;
    static Object lock = new Object();

    public static void main(String[] args) throws InterruptedException {
        Thread t1 = new Thread(() -> {
            for (int i = 0; i < ITERATIONS; ++i)
                synchronized(lock) {
                    ++counter;
                }
        });

        Thread t2 = new Thread(() -> {
            for (int i = 0; i < ITERATIONS; ++i)
                synchronized(lock) {
                    ++counter;
                }
        });

        t1.start();
        t2.start();

        t1.join();
        t2.join();

        System.out.println("Counted " + counter);
    }
}
