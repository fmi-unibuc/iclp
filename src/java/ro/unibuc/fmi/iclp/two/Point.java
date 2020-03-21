package ro.unibuc.fmi.iclp.two;

public class Point {
    int x;
    int y;
    static int counter;

    public Point(int x, int y) {
        this.x = x;
        this.y = y;
    }

    public synchronized void move(int dx, int dy) {
            x += dx;
            y += dy;
            counter++;
    }

    public synchronized void scale(int f) {
        x *= f; y*= f;
    }

    static synchronized int getCounter() {
        return counter;
    }

    public static void main(String[] args) {
        Point p = new Point(10,7);
        new Thread(() -> p.move(3,4)).start();
        new Thread(() -> p.scale(2)).start();
    }
}
