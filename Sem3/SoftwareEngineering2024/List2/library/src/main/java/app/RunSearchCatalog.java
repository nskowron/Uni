package app;

import java.io.IOException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.atomic.AtomicBoolean;

import app.IO.Color;


// Class RunAdd is a sub-UI concerned only by logic of managing certain information.

// Controller: Similarly to UI, it acts as a controller for user input when running.
// Low Coupling: It uses standarised LibraryDataHandler interface without relying on it's internal data.
// High Cohesion: It has a strictly defined set of responsibilities.
public final class RunSearchCatalog extends CleanRunnable
{
    private AtomicBoolean running;
    private Map<String, Runnable> options;

    public RunSearchCatalog(final LibraryDataHandler library)
    {
        running = new AtomicBoolean(true);
        options = new HashMap<>();

        options.put("a", () ->
        {
            try
            {
                List<Book> books = library.getBooks();

                IO.out("\nAll our books\n");
                for(Book book : books)
                {
                    IO.out(
                        "\n" + book.getID() + ". " +
                        book.getTitle() + " by " + book.getAuthor() + " ");

                    if(book.isAvailable())
                    {
                        IO.out("Available", Color.GREEN);
                    }
                    else
                    {
                        IO.out("Not available", Color.RED);
                    }
                }
                IO.out("\n");

                IO.out("\n(enter)\n");
                IO.in();
            }
            catch(IOException e)
            {
                IO.out("\n" + e.getMessage() + "\n", Color.RED);

                IO.out("\n(enter)\n");
                IO.in();
            }
        });
        options.put("A", options.get("a"));

        options.put("t", ()  ->
        {
            IO.out("Title: ");
            String title = IO.in();

            try
            {
                List<Book> books = library.getBooks(title);

                IO.out("\nAll matching books\n");
                for(Book book : books)
                {
                    IO.out(
                        "\n" + book.getID() + ". " +
                        book.getTitle() + " by " + book.getAuthor() + " ");

                    if(book.isAvailable())
                    {
                        IO.out("Available", Color.GREEN);
                    }
                    else
                    {
                        IO.out("Not available", Color.RED);
                    }
                }
                IO.out("\n");

                IO.out("\n(enter)\n");
                IO.in();
            }
            catch(IOException e)
            {
                IO.out("\n" + e.getMessage() + "\n", Color.RED);

                IO.out("\n(enter)\n");
                IO.in();
            }
        });
        options.put("T", options.get("t"));

        options.put("q", () -> { running.set(false); });
        options.put("Q", options.get("q"));
    }

    @Override
    public void run()
    {
        super.run();

        running.set(true);
        while(running.get())
        {
            new ChoiceBox(
                "Show All books [a]" +
                "\nSearch books by Title [t]" +
                "\nQuit catalog [q]" +
                "\n\nWhat do you want to do?: ", options);

            super.run();
        }
    }
}
