package app;

import java.io.IOException;
import java.util.HashMap;
import java.util.Map;

import app.IO.Color;


// Class RunCrs is a sub-UI concerned only by logic of Client Related Data (crs).

// Controller: Similarly to UI, it acts as a controller for user input when running.
// Low Coupling: It uses standarised LibraryDataHandler interface without relying on it's internal data.
// High Cohesion: It has a strictly defined set of responsibilities.
public final class RunCrs extends CleanRunnable
{
    private Map<String, Runnable> options;

    public RunCrs(final LibraryDataHandler library)
    {
        this.options = new HashMap<>();

        options.put("b", () -> 
        {
            try
            {
                IO.out("Customer's ID: ");
                int customerID = Integer.parseInt(IO.in());
                IO.out("Book ID: ");
                int bookID = Integer.parseInt(IO.in());

                Customer customer = library.getCustomer(customerID);
                Book book = library.getBook(bookID);

                library.borrowBook(customer, book);

                IO.out("\nSuccessfully rented " + book.getTitle() + " to " +
                        customer.getName() + " " + customer.getLastName() + "\n", Color.GREEN);

                IO.out("\n(enter)\n");
                IO.in();
            }
            catch(NumberFormatException e)
            {
                IO.out("\nInvalid ID - needs to be a number\n", Color.RED);

                IO.out("\n(enter)\n");
                IO.in();
            }
            catch(IOException | IllegalAccessException e)
            {
                IO.out("\n" + e.getMessage() + "\n", Color.RED);

                IO.out("\n(enter)\n");
                IO.in();
            }
        });
        options.put("B", options.get("b"));

        options.put("r", () -> 
        {
            try
            {
                IO.out("Book ID: ");
                int bookID = Integer.parseInt(IO.in());

                Book book = library.getBook(bookID);
                Customer customer = library.getCustomer(book.getOwnerID());

                library.returnBook(library.getBook(bookID));

                IO.out( "\nSuccessfully picked up " + book.getTitle() + " from " +
                        customer.getName() + " " + customer.getLastName() + "\n", Color.GREEN);

                IO.out("\n(enter)\n");
                IO.in();
            }
            catch(NumberFormatException e)
            {
                IO.out("\nInvalid ID - needs to be a number\n", Color.RED);

                IO.out("\n(enter)\n");
                IO.in();
            }
            catch(IOException | IllegalAccessException e)
            {
                IO.out("\n" + e.getMessage() + "\n", Color.RED);

                IO.out("\n(enter)\n");
                IO.in();
            }
        });
        options.put("R", options.get("R"));

        options.put("e", () -> 
        {
            try
            {
                IO.out("Customer's ID: ");
                int customerID = Integer.parseInt(IO.in());
                IO.out("Customer's new email address: ");
                String email = IO.in();

                Customer customer = library.getCustomer(customerID);
                Customer updated = new Customer(customer.getID(), customer.getName(), customer.getLastName(), email, customer.getBooks());

                library.updateCustomer(customerID, updated);

                IO.out("\nSuccessfully changed " + customer.getName() + " " + customer.getLastName() +
                        "'s email address to " + email + "\n", Color.GREEN);

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
        options.put("E", options.get("e"));
    }

    @Override
    public void run()
    {
        super.run();
        new ChoiceBox("Borrow/Return a book or change customer's Email address [bre]: ", options);
    }
}
