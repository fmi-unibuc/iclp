package ro.unibuc.fmi.curs1;

public class HelloThread {
  public static void main(String args[]) {
    new Thread ( () -> System.out.println("Hello thread!") )
        .start();
  }
}
