package ro.unibuc.fmi.iclp.one;

public class HelloRunnable implements Runnable {

    @Override
    public void run() {
        System.out.print("Hello");
        System.out.print("!");
    }

    public static void main(String[] args) {
        Thread thread = new Thread(new HelloRunnable());
        thread.start();
        System.out.print(" ");
        for (int i= 0; i< 10000; i++);
        System.out.print("Runnable");
    }
}
