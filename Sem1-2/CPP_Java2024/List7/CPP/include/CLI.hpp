#ifndef CLI_HPP
#define CLI_HPP

#include <BST.hpp>
#include <IO.hpp>

#include <functional>
#include <string>

template <typename T>
class CLI
{
private:
    BST<T> tree;
    std::function<T(const std::string&)> parse;

public:
    CLI(std::function<T(const std::string&)> parser) : parse(parser) {}

    void run();
};

#include <CLI.cpp>

#endif