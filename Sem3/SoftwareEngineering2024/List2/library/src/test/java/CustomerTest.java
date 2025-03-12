import app.Customer;
import app.Book;
import org.junit.Before;
import org.junit.Test;

import java.util.ArrayList;
import java.util.List;

import static org.junit.Assert.*;

public class CustomerTest {

    private Customer customer;
    private Book book1;
    private Book book2;

    @Before
    public void setUp() {
        book1 = new Book(1, "Book One", "Author A", true, -1);
        book2 = new Book(2, "Book Two", "Author B", true, -1);
        List<Book> initialBooks = new ArrayList<>();
        customer = new Customer(100, "John", "Doe", "johndoe@example.com", initialBooks);
    }

    @Test
    public void testConstructorWithAllParameters() {
        assertEquals(100, customer.getID());
        assertEquals("John", customer.getName());
        assertEquals("Doe", customer.getLastName());
        assertEquals("johndoe@example.com", customer.getEmail());
        assertTrue(customer.getBooks().isEmpty());
    }

    @Test
    public void testConstructorWithBasicDetailsOnly() {
        Customer customerMinimal = new Customer("Jane", "Doe", "janedoe@example.com");
        assertEquals(-1, customerMinimal.getID()); // Default ID
        assertEquals("Jane", customerMinimal.getName());
        assertEquals("Doe", customerMinimal.getLastName());
        assertEquals("janedoe@example.com", customerMinimal.getEmail());
        assertTrue(customerMinimal.getBooks().isEmpty()); // Default empty book list
    }

    @Test
    public void testBorrowBookSuccess() throws IllegalAccessException {
        customer.borrowBook(book1);
        assertTrue(customer.getBooks().contains(book1));
    }

    @Test(expected = IllegalAccessException.class)
    public void testBorrowBookThrowsExceptionWhenAlreadyBorrowed() throws IllegalAccessException {
        customer.borrowBook(book1); // First borrow
        customer.borrowBook(book1); // Attempt to borrow again should throw exception
    }

    @Test
    public void testReturnBookSuccess() throws IllegalAccessException {
        customer.borrowBook(book1); // First borrow
        customer.returnBook(book1); // Now return it
        assertFalse(customer.getBooks().contains(book1));
    }

    @Test(expected = IllegalAccessException.class)
    public void testReturnBookThrowsExceptionWhenNotBorrowed() throws IllegalAccessException {
        customer.returnBook(book1); // Attempt to return a book not borrowed should throw exception
    }

    @Test
    public void testGetBooks() throws IllegalAccessException {
        customer.borrowBook(book1);
        customer.borrowBook(book2);
        List<Book> borrowedBooks = customer.getBooks();
        
        assertEquals(2, borrowedBooks.size());
        assertTrue(borrowedBooks.contains(book1));
        assertTrue(borrowedBooks.contains(book2));
    }
}

