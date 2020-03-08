package ro.unibuc.fmi.iclp.three;

import java.util.concurrent.*;

public class FutureExample {
    public static void main(String[] args) throws Exception {
        ExecutorService executor = Executors.newSingleThreadExecutor();
        Future<Integer> future = executor.submit(() -> {
            int time = ThreadLocalRandom.current().nextInt(1000, 5000);
            Thread.sleep(time);
            return time;
        });
        while (!future.isDone()) {
            System.out.println("Task not done ...");
            Thread.sleep(500);
        }
        System.out.println("Task duration: " + future.get());
        executor.shutdown();
    }
}
