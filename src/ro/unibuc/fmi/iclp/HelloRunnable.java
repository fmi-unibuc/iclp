package ro.unibuc.fmi.iclp;

public class HelloRunnable {
    public static void main(String[] args) {
        new Thread(() -> System.out.print("hello ")).start();
        new Thread(() -> System.out.print("world")).start();
        System.out.println("!");
    }
}
