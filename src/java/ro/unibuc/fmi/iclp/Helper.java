package ro.unibuc.fmi.iclp;

import java.util.concurrent.locks.Lock;

public class Helper {
    public static void sleep(long miliSeconds) {
        try {
            Thread.sleep(miliSeconds);
        } catch (InterruptedException e) {
            e.printStackTrace();
        }
    }

    public static void syncRun(Lock l, Runnable runnable) {
        l.lock();
        try {
            runnable.run();
        } finally {
            l.unlock();
        }
    }

    public static void syncTRun(Lock l, ThrowingRunnable<InterruptedException> runnable) throws InterruptedException {
        l.lock();
        try {
            runnable.run();
        } finally {
            l.unlock();
        }
    }


    public static <T> T syncTCall(Lock l, ThrowingCallable<T, InterruptedException> callable)
            throws InterruptedException {
        l.lock();
        try {
            return callable.call();
        } finally {
            l.unlock();
        }
    }

}
