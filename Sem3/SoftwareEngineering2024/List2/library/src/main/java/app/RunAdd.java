package app;

import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

import app.IO.Color;


// Class RunAdd is a sub-UI concerned only by logic of adding data.

// Controller: Similarly to UI, it acts as a controller for user input when running.
// Low Coupling: It uses standarised LibraryDataHandler interface without relying on it's internal data.
// High Cohesion: It has a strictly defined set of responsibilities.
public final class RunAdd extends CleanRunnable
{
    private Map<String, Runnable> options;

    public RunAdd(final LibraryDataHandler library)
    {
        this.options = new HashMap<>();

        options.put("c", () -> 
        {
            IO.out("Customer's first name: ");
            String name = IO.in();
            IO.out("Customer's last name: ");
            String lastName = IO.in();
            IO.out("Customer's email address: ");
            String email = IO.in();

            try
            {
                int ID = library.addNewCustomer(new Customer(name, lastName, email));
                IO.out("\nSuccessfully added new customer - new ID = " + ID + "\n", Color.GREEN);

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
        options.put("C", options.get("c"));

        options.put("b", () -> 
        {
            IO.out("Book title: ");
            String title = IO.in();
            IO.out("Book author: ");
            String author = IO.in();

            try
            {
                int ID = library.addNewBook(new Book(title, author));
                IO.out("\nSuccessfully added new book - new ID = " + ID + "\n", Color.GREEN);

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
        options.put("B", options.get("b"));
    }

    @Override
    public void run()
    {
        super.run();
        new ChoiceBox("Add new Customer or Book [cb]: ", options);
    }
}
