package ro.unibuc.fmi.iclp.curs3;

import java.util.Collection;
import java.util.Optional;
import java.util.concurrent.*;
import java.util.function.BinaryOperator;
import java.util.function.Function;
import java.util.stream.Collectors;

public class InvokeAllExample<From,To> {
    ExecutorService executor = Executors.newWorkStealingPool();

    Optional<To> mapReduce(Collection<From> c,
                           Function<From, To> map,
                           BinaryOperator<To> reduce) throws Exception {
        return
          executor.invokeAll(
            c.stream()
              .map( e -> ((Callable<To>) () -> map.apply(e)) )
              .collect(Collectors.toList())
          ).stream()
            .map(InvokeAllExample::get)
            .reduce(reduce);
    }

    private static<To> To get(Future<To> future) {
        To v = null;
        try { v = future.get();
        } catch (InterruptedException | ExecutionException ignored) { }
        return v;
    }
}
