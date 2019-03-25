package ro.unibuc.fmi.iclp.curs4;

import java.math.BigInteger;
import java.util.Iterator;
import java.util.concurrent.RecursiveTask;
import java.util.function.BinaryOperator;

abstract class Expression extends RecursiveTask<BigInteger> {
    static class Number extends Expression {
        long value;
        Number(long x) { value = x; }

        @Override
        public BigInteger compute() {
//            if (value == 10) throw new UnsupportedOperationException("zece!");
            return BigInteger.valueOf(value);
        }
    }

    public static class Binary extends Expression {
        private final BinaryOperator<BigInteger> op;
        private final Expression left;
        private final Expression right;

        Binary(BinaryOperator<BigInteger> op,
               Expression left, Expression right)
        { this.op = op; this.left = left; this.right = right; }

        @Override
        protected BigInteger compute() {
            right.fork();
            return op.apply(left.compute(), right.join());
        }
    }

    public static Expression parse(String input) {
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
                break;
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

    public static void main(String[] args) {
        Expression e = parse(args[0]);
        try {
            System.out.println("Value: " + e.invoke());
        } catch (Exception ex) {
            System.out.println("Exceptie la executie: " + ex.getMessage());
        }
    }
}
