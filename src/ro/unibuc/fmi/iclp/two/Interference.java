package ro.unibuc.fmi.iclp.two;

public class Interference {
    static int c = 0;
    public static void main(String[] args)
            throws InterruptedException {
        Thread myThread = new Thread(() -> {
            for (int x = 0; x < 5000; ++x) c++;
        });
        myThread.start();
        for (int x = 0; x < 5000; ++x) c--;
        myThread.join();
        System.out.println("c = " + c);
    }
}
