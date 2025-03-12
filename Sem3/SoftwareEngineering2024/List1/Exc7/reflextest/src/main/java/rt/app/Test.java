package rt.app;

import java.util.Random;

public final class Test implements Runnable
{
    private static final Random RANDOM = new Random();
    private static final int WAIT_MIN = 1000;
    private static final int WAIT_MAX = 3000;
    private static final long SUSPICIOUS_SCORE = 5;

    public void run()
    {
        IO.out("\n\n\n");

        try
        {
            Thread.sleep(RANDOM.nextInt(WAIT_MAX - WAIT_MIN) + WAIT_MIN);
        }
        catch(InterruptedException e)
        {
            IO.out("Error: Program has been interrupted, failed to run test\n", IO.Color.RED);
            System.exit(1);
        }

        long start;
        long end;

        IO.out("###", IO.Color.RED);
        start = System.currentTimeMillis();
        IO.in();
        end = System.currentTimeMillis();

        long score = end - start;
        IO.out("\nYour score is ", IO.Color.WHITE);
        IO.out(Long.toString(score), IO.Color.GREEN);
        IO.out(score < SUSPICIOUS_SCORE
            ? "ms, godlike... suspicious...\n\n" : "ms, congratulations!\n\n", IO.Color.WHITE);
    }
}
