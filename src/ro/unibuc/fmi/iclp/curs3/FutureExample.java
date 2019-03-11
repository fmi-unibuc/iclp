package ro.unibuc.fmi.iclp.curs3;

import java.math.BigInteger;
import java.util.Iterator;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;
import java.util.function.BinaryOperator;

public class FutureExample {
    static ExecutorService executor = Executors.newWorkStealingPool();
    static class AsyncBinaryOperation<T> implements Callable<T> {
        private final BinaryOperator<T> op;
        private final Future<T> left;
        private final Future<T> right;

        public AsyncBinaryOperation(
                Callable<T> left,
                Callable<T> right,
                BinaryOperator<T> op) {
            this.op = op;
            this.left = executor.submit(left);
            this.right = executor.submit(right);
        }

        @Override
        public T call() throws Exception {
            return op.apply(left.get(), right.get());
        }
    }

    interface Expression {
        Callable<BigInteger> evaluate();

    }

    static class Number implements Expression {
        BigInteger value;
        Number(long x) {
            value = BigInteger.valueOf(x);
        }

        @Override
        public Callable<BigInteger> evaluate() {
            return () -> value;
        }
    }

    static class Binary implements Expression {
        private final BinaryOperator<BigInteger> op;
        private final Expression left;
        private final Expression right;

        Binary(BinaryOperator<BigInteger> op, Expression left, Expression right) {
            this.op = op;
            this.left = left;
            this.right = right;
        }

        @Override
        public Callable<BigInteger> evaluate() {
            return new AsyncBinaryOperation<>(left.evaluate(), right.evaluate(), op);
        }
    }

    private static Expression parse(String input) {
        return parse(input.chars().mapToObj(i -> (char) i).iterator());
    }

    private static Expression parse(Iterator<Character> iterator) {
        Character next = iterator.next();
        while (next == ' ') next = iterator.next();
        if (next >= '0' && next <= '9') {
            long x = next - '0';
            while ((next = iterator.next()) != ' ')
                x = 10 * x + (next - '0');
            return new Number(x);
        }
        BinaryOperator<BigInteger> op;
        switch (next) {
            case '+':
                op = BigInteger::add;
                break;
            case '-':
                op = BigInteger::subtract;
                break;
            case '*':
                op = BigInteger::multiply;
            case '/':
                op = BigInteger::divide;
                break;
            default:
                throw new UnsupportedOperationException();
        }
        Expression left = parse(iterator);
        Expression right = parse(iterator);
        return new Binary(op, left, right);
    }

    public static void main(String[] args) throws Exception {
        Expression e = parse(args[0]);
        BigInteger val = e.evaluate().call();
        System.out.println("Value: " + val);
    }
}
