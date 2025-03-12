package rt.app;

import java.util.Scanner;

public final class IO
{
    private static final Scanner SCANNER = new Scanner(System.in);

    private IO() throws InstantiationError
    {
        throw new InstantiationError("Cannot create instance of a static class IO.");
    }

    public enum Color
    {
        WHITE("\u001B[37m"),
        RED("\u001B[31m"),
        GREEN("\u001B[32m");

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
        System.out.print(color.getCode() + string + Color.WHITE.getCode());
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
