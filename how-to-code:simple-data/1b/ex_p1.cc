#include <iostream>
#include <string>

template <typename Type1, typename Type2> int test_function(Type1 func, Type2 expected_result)
{
    // check if return output of func and expected_result is the same
    if (func == expected_result)
        std::cout << "Expected value is reached. Test passed." << std::endl;
    else
    {
        std::cout << "Test 2 failed. Aborting..." << std::endl;
        return (1);
    }

    return 0;
}

// string -> string
// produces a summoning charm by appending the string param to "accio "

// std::string summon(std::string str)       //stub
//{
//    return "a";
//}

// std::string summon(std::string str)       //template
//{
//    ... a;
//}

std::string summon(std::string str)
{
    return ("accio " + str);
}

int main()
{
    test_function(summon("Firebolt"), "accio Firebolt");
    test_function(summon("portkey"), "accio portkey");
    test_function(summon("broom"), "accio broom");

    return 0;
}
