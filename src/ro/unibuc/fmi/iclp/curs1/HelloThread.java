package ro.unibuc.fmi.iclp.curs1;

public class HelloThread {
  public static void main(String args[]) {
    new Thread ( () -> System.out.println("Hello thread!") )
        .start();
  }
}
