import app.Book;
import org.junit.Before;
import org.junit.Test;

import static org.junit.Assert.*;

public class BookTest {

    private Book book;

    @Before
    public void setUp() {
        book = new Book(1, "Test Title", "Test Author", true, -1);
    }

    @Test
    public void testConstructorWithAllParameters() {
        assertEquals(1, book.getID());
        assertEquals("Test Title", book.getTitle());
        assertEquals("Test Author", book.getAuthor());
        assertTrue(book.isAvailable());
        assertEquals(-1, book.getOwnerID());
    }

    @Test
    public void testConstructorWithTitleAndAuthorOnly() {
        Book bookMinimal = new Book("Minimal Title", "Minimal Author");
        assertEquals(-1, bookMinimal.getID()); // Default ID
        assertEquals("Minimal Title", bookMinimal.getTitle());
        assertEquals("Minimal Author", bookMinimal.getAuthor());
        assertTrue(bookMinimal.isAvailable()); // Default availability
        assertEquals(-1, bookMinimal.getOwnerID()); // Default owner ID
    }

    @Test
    public void testBorrowBookSuccess() throws IllegalAccessException {
        book.borrowBook(101); // Customer ID is 101
        assertFalse(book.isAvailable());
        assertEquals(101, book.getOwnerID());
    }

    @Test(expected = IllegalAccessException.class)
    public void testBorrowBookThrowsExceptionWhenAlreadyBorrowed() throws IllegalAccessException {
        book.borrowBook(101); // First borrow
        book.borrowBook(102); // Attempting to borrow again should throw exception
    }

    @Test
    public void testReturnBookSuccess() throws IllegalAccessException {
        book.borrowBook(101); // Borrow it first
        book.returnBook();
        assertTrue(book.isAvailable());
        assertEquals(-1, book.getOwnerID()); // Owner ID reset to default
    }

    @Test(expected = IllegalAccessException.class)
    public void testReturnBookThrowsExceptionWhenAlreadyAvailable() throws IllegalAccessException {
        book.returnBook(); // Attempting to return an already available book should throw exception
    }

    @Test
    public void testEqualsAndHashCode() {
        Book sameBook = new Book(1, "Test Title", "Test Author", true, -1);
        Book differentBook = new Book(2, "Different Title", "Different Author", true, -1);

        assertEquals(book, sameBook);
        assertEquals(book.hashCode(), sameBook.hashCode());
        assertNotEquals(book, differentBook);
    }
}