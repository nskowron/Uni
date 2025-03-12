package app;

import java.util.Scanner;


// The IO class provides centralized handling of input and output operations for the application.

// Pure Fabrication: Provides a utility to handle I/O, enhancing separation of concerns.
// High Cohesion: Focuses solely on I/O responsibilities.
// Low Coupling: Isolates I/O, allowing other classes to be independent of specific I/O mechanisms.
// Protected Variations: Uses the Color enum to encapsulate color choices, making it adaptable to adding new colors.
public final class IO
{
    private static final Scanner SCANNER = new Scanner(System.in);

    private IO() throws InstantiationError
    {
        throw new InstantiationError("Cannot create instance of a static class IO.");
    }

    public enum Color
    {
        WHITE("\033[0m"),
        RED("\033[0;31m"),
        GREEN("\033[0;32m");

        private final String ansiCode;

        Color(final String code)
        {
            ansiCode = code;
        }

        public String getCode()
        {
            return ansiCode;
        }
    }

    public static void out(final String string, final Color color)
    {
        System.out.print(color == null ? Color.WHITE.getCode() : color.getCode() + string + Color.WHITE.getCode());
    }

    public static void out(final String string)
    {
        out(string, Color.WHITE);
    }

    public static String in()
    {
        return SCANNER.nextLine();
    }
}
