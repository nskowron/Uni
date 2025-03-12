import java.util.NoSuchElementException;
import java.util.Scanner;

public class IO
{
    private static Scanner scanner = new Scanner(System.in);

    private IO() throws InstantiationError
    {
        throw new InstantiationError("Cannot create instance of static class IO.");
    }

    public static void setOutputLine(String line)
    {
        System.out.println(line);
    }

    public static String getInputLine()
    {
        System.out.print("> ");

        try
        {
            return scanner.nextLine().trim();
        }
        catch(NoSuchElementException e)
        {
            return null;
        }
    }

    public static void close()
    {
        scanner.close();
    }
}
