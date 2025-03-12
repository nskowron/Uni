package app;

import java.util.Map;


// The ChoiceBox utility class displays a prompt to the user and executes an associated action based on their input.

// Controller: Manages the input-response interaction, directing input to the correct choice.
// Indirection: ChoiceBox communicates with other classes only via a map of chaoices.
// Low Coupling: Uses only the IO utility and a choices map, keeping it independent of any system logic.
// High Cohesion: It's solely responsible for input-response interaction without going into details.
// Protected Variations: ChoiceBox doesn't rely on specific choice logic in any way - behaves the same for any map.
// Pure Fabrication: Solely made to standarise user interaction.
public class ChoiceBox
{
    public ChoiceBox(final String prompt, final Map<String, Runnable> choices)
    {
        IO.out(prompt);
        
        String choice = IO.in();
        if(choices.containsKey(choice))
        {
            choices.get(choice).run();
        }
        else
        {
            IO.out("Error: unknown option: " + choice + "\n", IO.Color.RED);
        }
    }
}
