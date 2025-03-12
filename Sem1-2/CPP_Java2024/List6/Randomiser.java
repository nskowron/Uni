import java.util.Random;

import javafx.scene.paint.Color;

public class Randomiser extends Random
{
    public Color nextColor()
    {
        double bound = 1.0 + Math.ulp(1.0d);
        return new Color(nextDouble(bound), nextDouble(bound), nextDouble(bound), 1.0);
    }
}
