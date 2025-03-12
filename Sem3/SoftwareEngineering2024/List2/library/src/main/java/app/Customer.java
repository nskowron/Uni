package app;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;


// Class Customer is an Information Expert

// Customer and Book are two main "bricks" of data used by the system.
// They help transport data concerning only themselves
// between the UI and the system's DataManager.
public class Customer
{
    private int ID;

    private String name;
    private String lastName;
    private String email;

    private List<Book> books;

    public Customer(final int ID, final String name, final String lastName, final String email, final List<Book> books)
    {
        this.ID = ID;

        this.name = name;
        this.lastName = lastName;
        this.email = email;

        this.books = books;
    }

    public Customer(final String name, final String lastName, final String email)
    {
        this(-1, name, lastName, email, new ArrayList<Book>());
    }

    public final int getID()
    {
        return ID;
    }

    public final String getName()
    {
        return name;
    }

    public final String getLastName()
    {
        return lastName;
    }

    public final String getEmail()
    {
        return email;
    }

    public final List<Book> getBooks()
    {
        return books;
    }

    public final void borrowBook(final Book book) throws IllegalAccessException
    {
        if(books.contains(book))
        {
            throw new IllegalAccessException("Customer has already borrowed " + book.getTitle());
        }

        books.add(book);
    }

    public final void returnBook(final Book book) throws IllegalAccessException
    {
        if(!books.contains(book))
        {
            throw new IllegalAccessException("Customer has not borrowed " + book.getTitle());
        }

        books.remove(book);
    }

    @Override
    public boolean equals(final Object obj)
    {
        if(this == obj)
        {
            return true;
        }

        if(obj == null || getClass() != obj.getClass())
        {
            return false;
        }

        Customer customer = (Customer) obj;
        return ID == customer.ID &&
                name.equals(customer.name) &&
                lastName.equals(customer.lastName) &&
                email.equals(customer.email);
    }

    @Override
    public int hashCode()
    {
        return Objects.hash(ID, name, lastName, email);
    }
}
