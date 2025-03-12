#include <utils.hpp>

std::vector<std::string> splitString(std::string input)
{
    std::istringstream stream(input);
    std::vector<std::string> strings(0);
    std::string string;

    while(stream >> string)
    {
        strings.push_back(string);
    }

    return strings;
}