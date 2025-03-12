public class CLI<T extends Comparable<T>>
{
    private BST<T> tree;
    private Parser<T> parser;

    public CLI(Parser<T> parser)
    {
        this.parser = parser;
        tree = new BST<>();
    }

    public void run()
    {
        boolean running = true;
        while(running)
        {
            String input = IO.getInputLine();
            if(input.isEmpty() || input == null)
            {
                continue;
            }

            String[] command = input.split("\\s+");
            switch(command[0].toLowerCase())
            {
                case "draw":
                case "print":
                case "p":
                    if(command.length != 1)
                    {
                        IO.setOutputLine("Invalid number of arguments to print - should be 0, got: " + (command.length - 1));
                        break;
                    }

                    for(String line : tree.print())
                    {
                        IO.setOutputLine(line);
                    }
                    break;

                case "add":
                case "insert":
                case "a":
                case "i":
                    if(command.length != 2)
                    {
                        IO.setOutputLine("Invalid number of arguments to insert - should be 1, got: " + (command.length - 1));
                        break;
                    }

                    try
                    {
                        tree.insert(parser.parse(command[1]));
                    }
                    catch(NumberFormatException e)
                    {
                        IO.setOutputLine("Invalid argument to insert: " + command[1]);
                    }
                    
                    break;

                case "delete":
                case "remove":
                case "d":
                case "r":
                    if(command.length != 2)
                    {
                        IO.setOutputLine("Invalid number of arguments to delete - should be 1, got: " + (command.length - 1));
                        break;
                    }

                    try
                    {
                        tree.delete(parser.parse(command[1]));
                    }
                    catch(NumberFormatException e)
                    {
                        IO.setOutputLine("Invalid argument to delete: " + command[1]);
                    }

                    break;

                case "search":
                case "find":
                case "s":
                case "f":
                    if(command.length != 2)
                    {
                        IO.setOutputLine("Invalid number of arguments to find - should be 1, got: " + (command.length - 1));
                        break;
                    }

                    try
                    {
                        T argument = parser.parse(command[1]);
                        IO.setOutputLine(command[1] + (tree.search(argument) ? " is present in the Tree." : " is not present in the Tree."));
                    }
                    catch(NumberFormatException e)
                    {
                        IO.setOutputLine("Invalid argument to insert: " + command[1]);
                    }

                    break;

                case "quit":
                case "q":
                    running = false;
                    break;

                default:
                    IO.setOutputLine("Invalid command: " + command[0]);
            }
        }
    }
}
