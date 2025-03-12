#include <CLI.hpp>
#include <utils.hpp>

#include <string>

template <typename T>
void CLI<T>::run()
{
    bool running = true;
    while(running)
    {
        std::string input = IO::getInputLine();
        if(input.empty())
        {
            break;
        }

        std::vector<std::string> command = splitString(input);
        std::string main_command = command.at(0);
        if(main_command == "insert" || main_command == "add" || main_command == "i" || main_command == "a")
        {
            if(command.size() != 2)
            {
                IO::setOutputLine("Invalid number of arguments to print - should be 1, got: " + std::to_string(command.size() - 1));
                continue;
            }

            try
            {
                tree.insert(parse(command.at(1)), command.at(1));
            }
            catch(const std::exception& e)
            {
                IO::setOutputLine("Invalid argument to insert: " + command.at(1));
            }
        }
        else if(main_command == "delete" || main_command == "remove" || main_command == "d" || main_command == "r")
        {
            if(command.size() != 2)
            {
                IO::setOutputLine("Invalid number of arguments to print - should be 1, got: " + std::to_string(command.size() - 1));
                continue;
            }

            try
            {
                tree.remove(parse(command.at(1)));
            }
            catch(const std::exception& e)
            {
                IO::setOutputLine("Invalid argument to remove: " + command.at(1));
            }
        }
        else if(main_command == "search" || main_command == "find" || main_command == "s" || main_command == "f")
        {
            if(command.size() != 2)
            {
                IO::setOutputLine("Invalid number of arguments to print - should be 1, got: " + std::to_string(command.size() - 1));
                continue;
            }

            try
            {
                T argument = parse(command.at(1));
                IO::setOutputLine(command.at(1) + (tree.search(argument) ? " is present in the Tree." : " is not present in the Tree."));
            }
            catch(const std::exception& e)
            {
                IO::setOutputLine("Invalid argument to find: " + command.at(1));
            }
        }
        else if(main_command == "print" || main_command == "p")
        {
            if(command.size() != 1)
            {
                IO::setOutputLine("Invalid number of arguments to print - should be 0, got: " + std::to_string(command.size() - 1));
                continue;
            }

            for(std::string line : tree.print())
            {
                IO::setOutputLine(line);
            }
        }
        else if(main_command == "quit" || main_command == "q")
        {
            running = false;
        }
        else
        {
            IO::setOutputLine("Invalid command: " + main_command);
        }
    }
}