package ro.unibuc.fmi.iclp;

public class HelloRunnableJava8 {
    static volatile int i;

    public static void main(String[] args) throws InterruptedException {
        final Thread m = Thread.currentThread();
        Thread child = new Thread(
                () -> {
                    try {
                        System.out.println("Hello");
                        Thread.sleep(1000);
                        Thread.yield();
                        System.out.println("!");
                    } catch (InterruptedException e) {
                        System.err.println("Child interrupted!");
                    }
                });
        child.start();
        System.out.println(" ");
        Thread.sleep(500);
        System.exit(0);
        System.out.println("World");
    }
}
