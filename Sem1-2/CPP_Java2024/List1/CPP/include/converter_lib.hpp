#ifndef CONVERTER_LIB_HPP
#define CONVERTER_LIB_HPP

#include <string>

class ConverterLIB
{
public:
    ConverterLIB() = delete;
    ~ConverterLIB() = delete;

    template <typename T>
    static T ConvertStringTo(const std::string& s) noexcept(false);
};

#endif
