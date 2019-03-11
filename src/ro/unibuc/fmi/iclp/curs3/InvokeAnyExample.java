package ro.unibuc.fmi.iclp.curs3;

import java.util.Arrays;
import java.util.Collection;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

public class InvokeAnyExample {
    final Integer[] data;
    ExecutorService executor = Executors.newWorkStealingPool();

    public InvokeAnyExample(Collection<Integer> data) {
        this.data =  data.stream().sorted().toArray(Integer[]::new);
    }

    int search1(Integer key) {
        for (int i = 0; i < data.length; i++)
            if (data[i].equals(key)) return i;
        return -1;
    }

    int search2(Integer key) {
        for (int i = 0; i < data.length; i++)
            if (data[i].equals(key)) return i;
        return -1;
    }

    int search3(Integer key) {
        return Arrays.binarySearch(data, key, Integer::compareTo);
    }

    int search(Integer key) throws ExecutionException, InterruptedException {
        return executor.invokeAny(Arrays.asList(() -> search1(key), () -> search2(key), () -> search3(key)));
    }

}
