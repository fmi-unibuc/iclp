package ro.unibuc.fmi.curs1;

public class HelloRunnable implements Runnable {
  public void run() { System.out.println("Hello thread!"); }
  public static void main(String args[]) {
    Thread t = new Thread (new HelloRunnable());
    t.start();
  }
}
