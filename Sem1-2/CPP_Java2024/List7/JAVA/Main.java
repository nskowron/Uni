public class Main
{
    public static void main(String[] args)
    {
        IO.setOutputLine("Enter Tree type:"); 
        IO.setOutputLine("[i]nteger, [d]ouble, [s]tring");
        String input = IO.getInputLine().toLowerCase();
        switch(input)
        {
            case "integer":
            case "int":
            case "i":
                CLI<Integer> integerCLI = new CLI<Integer>(new Parser<Integer>()
                {
                    @Override
                    public Integer parse(String s)
                    {
                        return Integer.parseInt(s);
                    }
                });
                IO.setOutputLine("Integer Tree created successfully, enter commands below:");
                IO.setOutputLine("[p]rint, [i]nsert, [d]elete, [f]ind, [q]uit");
                integerCLI.run();
                break;

            case "float":
            case "double":
            case "d":
                CLI<Double> doubleCLI = new CLI<Double>(new Parser<Double>()
                {
                    @Override
                    public Double parse(String s)
                    {
                        return Double.parseDouble(s);
                    }
                });
                IO.setOutputLine("Double Tree created successfully, enter commands below:");
                IO.setOutputLine("[p]rint, [i]nsert, [d]elete, [f]ind, [q]uit");
                doubleCLI.run();
                break;

            case "text":
            case "string":
            case "s":
                CLI<String> stringCLI = new CLI<String>(new Parser<String>()
                {
                    @Override
                    public String parse(String s)
                    {
                        return s;
                    }
                });
                IO.setOutputLine("String Tree created successfully, enter commands below:");
                IO.setOutputLine("[p]rint, [i]nsert, [d]elete, [f]ind, [q]uit");
                stringCLI.run();
                break;

            default:
                IO.setOutputLine("Invalid Tree type.");
        }
    }
}