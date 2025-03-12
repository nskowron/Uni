public class ThreadLogger
{
    private ThreadLogger() throws InstantiationError
    {
        throw new InstantiationError("Cannot create instance of a static class ThreadController");
    }

    public static void logStart(Thread thread)
    {
        System.out.println("Start: " + thread.threadId());
    }

    public static void logEnd(Thread thread)
    {
        System.out.println("End: " + thread.threadId());
    }
}
