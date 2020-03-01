package ro.unibuc.fmi.iclp.two;

public class MessageLoopInterrupted implements Runnable {
    public void run() {
        String[] importantInfo =
                {"This", "is", "very", "important"};
        try {
            for (String s : importantInfo) {
                Thread.sleep(4000);//Pause for 4 seconds
                threadMessage(s);
            }
        } catch (InterruptedException e) {
            threadMessage("I wasn't done!");
        }
    }

    public static void threadMessage(String message) {
        String threadName = Thread.currentThread().getName();
        System.out.format("%s: %s%n", threadName, message);
    }

    public static void main(String[] args)
            throws InterruptedException {
        long patience = 1000 * 10;
        long startTime = System.currentTimeMillis();
        threadMessage("Starting MessageLoop thread");
        Thread t = new Thread(new MessageLoopInterrupted()); t.start();
        threadMessage("Waiting for MessageLoop thread to finish");
        while (t.isAlive()) {
            threadMessage("Still waiting..."); t.join(2000);
            if ((System.currentTimeMillis() - startTime
                    > patience)  && t.isAlive()) {
                threadMessage("Tired of waiting!");
                t.interrupt(); t.join();
            } }
        threadMessage("Finally!");
    }
}
