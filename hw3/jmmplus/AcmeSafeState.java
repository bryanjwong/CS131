import java.util.concurrent.atomic.AtomicLongArray;

public class AcmeSafeState implements State {
    private AtomicLongArray value;

    AcmeSafeState(int length) { value = new AtomicLongArray(length); }

    public int size() { return value.length(); }

    public long[] current() {
        int n = size();
        long[] res = new long[n];
        for (int k = 0; k < n; k++) {
            res[k] = value.get(k);
        }
        return res;
    }

    public void swap(int i, int j) {
        value.getAndIncrement(i);
        value.getAndDecrement(j);
    }
}
