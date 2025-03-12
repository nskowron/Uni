package rt.app;

import java.util.HashMap;

public final class App
{
    public static void main(final String[] args)
    {
        IO.out("\n\n~ Welcome to ReflexTest! ~\n\n", IO.Color.GREEN);

        IO.out("Rules: Press ", IO.Color.WHITE);
        IO.out("enter", IO.Color.GREEN);
        IO.out(" as soon as you see ", IO.Color.WHITE);
        IO.out("###", IO.Color.RED);
        IO.out(" good luck!\n\n", IO.Color.WHITE);

        HashMap<String, Runnable> choices = new HashMap<>();

        Runnable quit = () -> { System.exit(0); };
        choices.put("q", quit);
        choices.put("Q", quit);
        choices.put("quit", quit);
        choices.put("Quit", quit);

        Runnable test = new Test();
        choices.put("s", test);
        choices.put("S", test);
        choices.put("start", test);
        choices.put("Start", test);

        IO.out("Start new test, or Quit [sq]? : ");
        new ChoiceBox(choices);
    }

    private App() throws InstantiationError
    {
        throw new InstantiationError("Cannot create instance of a static class App.");
    }
}
