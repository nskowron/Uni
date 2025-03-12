public class Shapes
{
    enum Shapes_OneParam implements IShapes_OneParam
    {
        SQUARE
        {
            @Override
            public String Name()
            {
                return "Square";
            }
    
            @Override
            public double Area(double param) throws IllegalArgumentException
            {
                Shapes_OneParam.CheckParam(param);
                return param * param;
            }
    
            @Override
            public double Circumference(double param) throws IllegalArgumentException
            {
                Shapes_OneParam.CheckParam(param);
                return 4.0 * param;
            }
        },
        HEXAGON
        {
            @Override
            public String Name()
            {
                return "Hexagon";
            }
    
            @Override
            public double Area(double param) throws IllegalArgumentException
            {
                Shapes_OneParam.CheckParam(param);
                return 1.5 * Math.sqrt(3) * param * param;
            }
    
            @Override
            public double Circumference(double param) throws IllegalArgumentException
            {
                Shapes_OneParam.CheckParam(param);
                return 6.0 * param;
            }
        },
        PENTAGON 
        {
            @Override
            public String Name() 
            {
                return "Pentagon";
            }
    
            @Override
            public double Area(double param) throws IllegalArgumentException
            {
                Shapes_OneParam.CheckParam(param);
                return 1.25 * param * param * (1.0 / Math.tan(Math.toRadians(36)));
            }
    
            @Override
            public double Circumference(double param) throws IllegalArgumentException
            {
                Shapes_OneParam.CheckParam(param);
                return 5.0 * param;
            }
        },
        CIRCLE 
        {
            @Override
            public String Name() {
                return "Circle";
            }
    
            @Override
            public double Area(double param) throws IllegalArgumentException
            {
                Shapes_OneParam.CheckParam(param);
                return Math.PI * param * param;
            }
    
            @Override
            public double Circumference(double param) throws IllegalArgumentException
            {
                Shapes_OneParam.CheckParam(param);
                return 2.0 * Math.PI * param;
            }
        };

        static private void CheckParam(double param) throws IllegalArgumentException
        {
            if(param < 0)
            {
                throw new IllegalArgumentException("Parameter cannot be negative, got: " + param);
            }
        }
    }

    enum Shapes_TwoParam implements IShapes_TwoParam
    {
        RECTANGLE
        {
            @Override
            public String Name()
            {
                return "Rectangle";
            }
    
            @Override
            public double Area(double param1, double param2) throws IllegalArgumentException
            {
                Shapes_TwoParam.CheckParams(param1, param2);
                return param1 * param2;
            }
    
            @Override
            public double Circumference(double param1, double param2) throws IllegalArgumentException
            {
                Shapes_TwoParam.CheckParams(param1, param2);
                return 2.0 * param1 + 2.0 * param2;
            }
        },
        RHOMBUS
        {
            @Override
            public String Name()
            {
                return "Rhombus";
            }
    
            @Override
            public double Area(double param1, double param2) throws IllegalArgumentException
            {
                Shapes_TwoParam.CheckParams(param1, param2);
                if(param2 > 180)
                {
                    throw new IllegalArgumentException("Rhombus angle cannot be reflex, got: " + param2);
                }
                return param1 * param1 * Math.sin(Math.toRadians(param2));
            }
    
            @Override
            public double Circumference(double param1, double param2) throws IllegalArgumentException
            {
                Shapes_TwoParam.CheckParams(param1, param2);
                if(param2 > 180)
                {
                    throw new IllegalArgumentException("Rhombus angle cannot be reflex, got: " + param2);
                }
                return 4.0 * param1;
            }
        };

        static private void CheckParams(double param1, double param2) throws IllegalArgumentException
        {
            if(param1 < 0 || param2 < 0)
            {
                throw new IllegalArgumentException("Parameters cannot be negative, got: " + param1 + ", " + param2);
            }
        }
    }
}
