package ro.unibuc.fmi.iclp.one;

public class HelloThread extends Thread {

    @Override
    public void run() {
        System.out.print("Hello");
        System.out.print("!");
    }

    public static void main(String[] args) {
        Thread thread = new HelloThread();
        thread.start();
        System.out.print(" ");
        for (int i= 0; i< 10000; i++);
        System.out.print("World");
    }
}
