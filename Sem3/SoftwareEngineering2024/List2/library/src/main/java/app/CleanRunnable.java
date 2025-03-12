package app;


// CleanRunnable is a class that resembles a Window but in a CLI

// Pure Fabrication: It provides an additional layer of abstracion for UI windows
// and standarises the way of cleaning the CLI.
// Polymorphism: It creates a common interface for different inheriting runnables.
public class CleanRunnable implements Runnable
{
    public void run()
    {
        IO.out("\033c");
    }
}
