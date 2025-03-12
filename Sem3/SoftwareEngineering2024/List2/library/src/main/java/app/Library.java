package app;


// Class Library is the main(literally) Creator

// It's responsible for initialising the main classes
// UI, LibraryDataHandler and possibly in the future some kind of a DatabaseHandler.
// As the creator, it's responsible for assigning the dependencies:
// an instance of LibraryDataHandler for UI
// and possibly an instance of a DatabaseHandler for different future instance of LDH.
public final class Library
{
    private Library() throws InstantiationError
    {
        throw new InstantiationError("Cannot create instance of a static class Library");
    }

    public static void main(final String[] args)
    {
        LibraryDataHandler library = new LibraryDataHandler_Mimic();
        UI ui = new UI(library);
        ui.run();
    }
}
