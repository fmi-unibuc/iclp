package ro.unibuc.fmi.curs1;

public class SleepyMessages {
  public static void main(String args[])
      throws InterruptedException {
    String importantInfo[] =
        { "This", "is", "very", "important"};

    for (int i = 0;  i < importantInfo.length; i++) {
      Thread.sleep(2000);//Pause for 4 seconds
      System.out.println(importantInfo[i]);
    }
  }
}
