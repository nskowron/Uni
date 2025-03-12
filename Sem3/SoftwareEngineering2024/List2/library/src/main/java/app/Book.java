package app;

import java.util.Objects;


// Class Book in an Information Expert

// It contains all and only the information strictly concerning itself
// Which also helps maintain High Cohesion.
public class Book
{
    private int ID;

    private String title;
    private String author;

    private boolean available;
    private int ownerID;

    public Book(final int ID, final String title, final String author, final boolean available, final int ownerID)
    {
        this.ID = ID;

        this.title = title;
        this.author = author;

        this.available = available;
        this.ownerID = ownerID;
    }

    public Book(final String title, final String author)
    {
        this(-1, title, author, true, -1);
    }

    public final int getID()
    {
        return ID;
    }

    public final String getTitle()
    {
        return title;
    }

    public final String getAuthor()
    {
        return author;
    }

    public final boolean isAvailable()
    {
        return available;
    }

    public final int getOwnerID()
    {
        return ownerID;
    }

    public final void borrowBook(final int customerID) throws IllegalAccessException
    {
        if(available == false)
        {
            throw new IllegalAccessException("Book is already borrowed");
        }

        ownerID = customerID;
        available = false;
    }

    public final void returnBook() throws IllegalAccessException
    {
        if(available)
        {
            throw new IllegalAccessException("Book is not borrowed by anyone");
        }

        ownerID = -1;
        available = true;
    }

    @Override
    public boolean equals(final Object obj)
    {
        if (this == obj)
        {
            return true;
        }
        
        if (obj == null || getClass() != obj.getClass())
        {
            return false;
        }

        Book book = (Book) obj;
        return  this.ID == book.getID() &&
                title.equals(book.getTitle()) &&
                author.equals(book.getAuthor());
    }

    @Override
    public int hashCode()
    {
        return Objects.hash(ID, title, author, available);
    }
}
