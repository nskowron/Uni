package app;

import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

import app.IO.Color;


// Class RunInf is a sub-UI concerned only by logic of managing information.

// Controller: Similarly to UI, it acts as a controller for user input when running.
// Low Coupling: It uses standarised LibraryDataHandler interface without relying on it's internal data.
// High Cohesion: It has a strictly defined set of responsibilities.
public final class RunInf extends CleanRunnable
{
    private Map<String, Runnable> options;

    public RunInf(final LibraryDataHandler library)
    {
        this.options = new HashMap<>();

        options.put("c", () -> 
        {
            IO.out("Customer's ID or email: ");
            String email = IO.in();

            Customer customer;
            try
            {
                try
                {
                    int ID = Integer.parseInt(email);
                    customer = library.getCustomer(ID);
                }
                catch(NumberFormatException e)
                {
                    customer = library.getCustomerByEmail(email);
                }

                IO.out("\nCustomer Info\n", Color.GREEN);
                IO.out(
                    "\nID: " + customer.getID() +
                    "\nName: " + customer.getName() +
                    "\nLast Name: " + customer.getLastName() +
                    "\nemail: " + customer.getEmail() +
                    "\nbooks: "
                );
                for(Book book : customer.getBooks())
                {
                    IO.out(book.getTitle() + " ");
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
        options.put("C", options.get("c"));

        options.put("b", () -> 
        {
            try
            {
                IO.out("Book ID: ");
                int bookID = Integer.parseInt(IO.in());

                Book book = library.getBook(bookID);

                IO.out("\nBook Info\n", Color.GREEN);
                IO.out(
                    "\nBook ID: " + book.getID() +
                    "\nBook title: " + book.getTitle() +
                    "\nBook author: " + book.getAuthor() +
                    "\nAvailability: "
                );
                if(book.isAvailable())
                {
                    IO.out("Available", Color.GREEN);
                }
                else
                {
                    IO.out("Not available", Color.RED);
                    IO.out("\nBorrowed by " + book.getOwnerID());
                }
                IO.out("\n");

                IO.out("\n(enter)\n");
                IO.in();
            }
            catch(NumberFormatException e)
            {
                IO.out("\nInvalid ID - needs to be a number\n", Color.RED);

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

        options.put("s", new RunSearchCatalog(library));
        options.put("S", options.get("s"));
    }

    @Override
    public void run()
    {
        super.run();
        new ChoiceBox("Get info about a Customer, Book or Search the catalog [cbs]: ", options);
    }
}

