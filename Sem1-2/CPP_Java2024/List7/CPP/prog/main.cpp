#include <IO.hpp>
#include <CLI.hpp>

#include <string>

int main()
{
    IO::setOutputLine("Enter Tree type:"); 
    IO::setOutputLine("[i]nteger, [d]ouble, [s]tring");
    std::string input = IO::getInputLine();

    if(input == "integer" || input == "int" || input == "i")
    {
        CLI<int> intCLI([](const std::string& s) -> int { return std::stoi(s); });
        IO::setOutputLine("Integer Tree created successfully, enter commands below:");
        IO::setOutputLine("[p]rint, [i]nsert, [d]elete, [f]ind, [q]uit");
        intCLI.run();
    }
    else if(input == "double" || input == "d")
    {
        CLI<double> doubleCLI([](const std::string& s) -> double { return std::stod(s); });
        IO::setOutputLine("Double Tree created successfully, enter commands below:");
        IO::setOutputLine("[p]rint, [i]nsert, [d]elete, [f]ind, [q]uit");
        doubleCLI.run();
    }
    else if(input == "string" || input == "s")
    {
        CLI<std::string> stringCLI([](const std::string& s) -> std::string { return s; });
        IO::setOutputLine("String Tree created successfully, enter commands below:");
        IO::setOutputLine("[p]rint, [i]nsert, [d]elete, [f]ind, [q]uit");
        stringCLI.run();
    }
    else
    {
        IO::setOutputLine("Invalid Tree type.");
    }
}