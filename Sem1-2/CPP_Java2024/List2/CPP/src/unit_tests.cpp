#include <unit_tests.hpp>
#include <pascals_triangle.hpp>
#include <converter_lib.hpp>

#include <cassert>

void UnitTests::TestAll()
{
    TestPascalsTriangle();
    TestConverter();
}

void UnitTests::TestPascalsTriangle()
{
    {
        PascalsTriangle PT(5);
        assert(PT.element(1) == 5);
        assert(PT.element(2) == 10);
        assert(PT.element(3) == 10);
        assert(PT.element(4) == 5);
        assert(PT.element(5) == 1);

        std::vector<unsigned long long> row = PT.row();
        assert(row[2] == 10);
        assert(row[4] == 5);
        assert(row[5] == 1);
    }

    try
    {
        PascalsTriangle PT(-12);
        assert(false);
    }
    catch(const std::exception& e)
    {
        assert(true);
    }

    try
    {
        PascalsTriangle PT(12);
        PT.element(13);
        assert(false);
    }
    catch(const std::exception& e)
    {
        assert(true);
    }

    try
    {
        PascalsTriangle PT(30);
        PT.element(-13);
        assert(false);
    }
    catch(const std::exception& e)
    {
        assert(true);
    }
}

void UnitTests::TestConverter()
{
    assert(ConverterLIB::ConvertStringTo<int>("0") == 0);
    assert(ConverterLIB::ConvertStringTo<int>("-11") == -11);
    assert(ConverterLIB::ConvertStringTo<int>("23") == 23);
    assert(ConverterLIB::ConvertStringTo<int>("176915") == 176915);
    assert(ConverterLIB::ConvertStringTo<int>("-987315") == -987315);

    try
    {
        ConverterLIB::ConvertStringTo<int>("ala");
        assert(false);
    }
    catch(const std::exception& e)
    {
        assert(true);
    }

    try
    {
        ConverterLIB::ConvertStringTo<int>("blajha");
        assert(false);
    }
    catch(const std::exception& e)
    {
        assert(true);
    }
}
