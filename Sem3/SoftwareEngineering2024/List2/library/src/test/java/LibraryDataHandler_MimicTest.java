import app.Book;
import app.Customer;
import app.LibraryDataHandler_Mimic;
import org.junit.Before;
import org.junit.Test;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import static org.junit.Assert.*;

public class LibraryDataHandler_MimicTest {

    private LibraryDataHandler_Mimic handler;

    @Before
    public void setUp() {
        handler = new LibraryDataHandler_Mimic();
    }

    @Test
    public void testAddNewBook() throws IOException {
        Book book = new Book("Effective Java", "Joshua Bloch");
        int id = handler.addNewBook(book);

        Book addedBook = handler.getBook(id);
        assertEquals("Effective Java", addedBook.getTitle());
        assertEquals("Joshua Bloch", addedBook.getAuthor());
        assertEquals(id, addedBook.getID());
    }

    @Test
    public void testAddNewCustomer() throws IOException {
        Customer customer = new Customer("John", "Doe", "john.doe@example.com");
        int id = handler.addNewCustomer(customer);

        Customer addedCustomer = handler.getCustomer(id);
        assertEquals("John", addedCustomer.getName());
        assertEquals("Doe", addedCustomer.getLastName());
        assertEquals("john.doe@example.com", addedCustomer.getEmail());
        assertEquals(id, addedCustomer.getID());
    }

    @Test
    public void testUpdateBook() throws IOException {
        Book originalBook = new Book("Clean Code", "Robert C. Martin");
        int id = handler.addNewBook(originalBook);

        Book updatedBook = new Book(id, "Clean Coder", "Robert C. Martin", true, -1);
        handler.updateBook(id, updatedBook);

        Book retrievedBook = handler.getBook(id);
        assertEquals("Clean Coder", retrievedBook.getTitle());
        assertEquals("Robert C. Martin", retrievedBook.getAuthor());
    }

    @Test(expected = IOException.class)
    public void testUpdateBookThrowsExceptionForInvalidID() throws IOException {
        Book updatedBook = new Book(999, "Non-existent Book", "Unknown Author", true, -1);
        handler.updateBook(999, updatedBook);
    }

    @Test
    public void testUpdateCustomer() throws IOException {
        Customer originalCustomer = new Customer("Jane", "Doe", "jane.doe@example.com");
        int id = handler.addNewCustomer(originalCustomer);

        Customer updatedCustomer = new Customer(id, "Jane", "Smith", "jane.smith@example.com", originalCustomer.getBooks());
        handler.updateCustomer(id, updatedCustomer);

        Customer retrievedCustomer = handler.getCustomer(id);
        assertEquals("Jane", retrievedCustomer.getName());
        assertEquals("Smith", retrievedCustomer.getLastName());
        assertEquals("jane.smith@example.com", retrievedCustomer.getEmail());
    }

    @Test(expected = IOException.class)
    public void testUpdateCustomerThrowsExceptionForInvalidID() throws IOException {
        Customer updatedCustomer = new Customer(999, "Ghost", "User", "ghost.user@example.com", new ArrayList<>());
        handler.updateCustomer(999, updatedCustomer);
    }

    @Test
    public void testGetBook() throws IOException {
        Book book = new Book("Refactoring", "Martin Fowler");
        int id = handler.addNewBook(book);

        Book retrievedBook = handler.getBook(id);
        assertEquals("Refactoring", retrievedBook.getTitle());
        assertEquals("Martin Fowler", retrievedBook.getAuthor());
    }

    @Test(expected = IOException.class)
    public void testGetBookThrowsExceptionForInvalidID() throws IOException {
        handler.getBook(999); // Non-existent book ID
    }

    @Test
    public void testGetBooksByTitle() throws IOException {
        handler.addNewBook(new Book("Java Concurrency", "Brian Goetz"));
        handler.addNewBook(new Book("Java Performance", "Scott Oaks"));

        List<Book> javaBooks = handler.getBooks("Java");
        assertEquals(2, javaBooks.size());
    }

    @Test
    public void testGetBooks() throws IOException {
        handler.addNewBook(new Book("Effective Java", "Joshua Bloch"));
        handler.addNewBook(new Book("Java Concurrency in Practice", "Brian Goetz"));

        List<Book> allBooks = handler.getBooks();
        assertEquals(2, allBooks.size());
    }

    @Test(expected = IOException.class)
    public void testGetTitlesThrowsIOException() throws IOException {
        handler.getTitles("Some Title");
    }

    @Test(expected = IOException.class)
    public void testGetTitlesWithoutParametersThrowsIOException() throws IOException {
        handler.getTitles();
    }

    @Test
    public void testGetCustomerByID() throws IOException {
        Customer customer = new Customer("Alice", "Johnson", "alice.johnson@example.com");
        int id = handler.addNewCustomer(customer);

        Customer retrievedCustomer = handler.getCustomer(id);
        assertEquals("Alice", retrievedCustomer.getName());
    }

    @Test(expected = IOException.class)
    public void testGetCustomerThrowsExceptionForInvalidID() throws IOException {
        handler.getCustomer(999); // Non-existent customer ID
    }

    @Test
    public void testGetCustomerByEmail() throws IOException {
        Customer customer = new Customer("Bob", "Smith", "bob.smith@example.com");
        handler.addNewCustomer(customer);

        Customer retrievedCustomer = handler.getCustomerByEmail("bob.smith@example.com");
        assertEquals("Bob", retrievedCustomer.getName());
        assertEquals("Smith", retrievedCustomer.getLastName());
    }

    @Test(expected = IOException.class)
    public void testGetCustomerByEmailThrowsExceptionForInvalidEmail() throws IOException {
        handler.getCustomerByEmail("unknown.email@example.com");
    }
}

