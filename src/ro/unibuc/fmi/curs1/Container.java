package ro.unibuc.fmi.curs1;

public class Container {
  static int c = 0;
  synchronized void increment() { c++; }
  synchronized void decrement() { c--; }
}