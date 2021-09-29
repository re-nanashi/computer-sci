#include <iostream>

/*
How to design a function:
1. Signature, purpose, stub
2. Examples with debug function
3. Inventory - template & constants
4. Code the body
5. Test and debug

SAMPLE PROBLEM: DESIGN A FUNCTION THAT CONSUMES A NUMBER AND PRODUCES TWICE THAT
NUMBER. CALL YOUR FUNCTION DOUBLE.

STEP 1:
    a. Create a signature: Type(Input)... -> Type(Return)
        // Number -> Number

    b. Create a purpose description (1 line) of what the function produces in
        terms of what it consumes
        // produce 2 times the given number || produces n times 2

    c. Create a stub - a stub is a syntactically complete function definition
        that produces a value of the right type. number -> 0, string -> 'a'

        // int double(int number)
        {
            return 0;
        }

STEP 2:
    a. Create an example input-result list
        // double(3) >> 6
        // double(2) >> 4

    b. It is optional to create a function debug/tester
        NOTE: use the stub to check if tester runs ok
        // int func = return type of the function
        // int result = expected result
        //  bool tester(int func, int result)
            {
                // will return true if func is equal to result otherwise false
                return (func == result);
            }
STEP 3:
    a. Create a template; ... means do something
        //  int double(int number)
            {
                ... number
                return answer;
            }

STEP 4: CODE THE BODY
    int double(number)
    {
        return (2 * number);
    }

STEP 5: RUN THE TEST

sample cpp function
*/

// function double tester
void test_function(int func_return, int expected_result)
{
    char const *result = 0;
    (func_return == expected_result ? result = "true" : result = "false");
    std::cout << *result << std::endl;
}

// Signature: int -> int
// Purpose: return input multiplied by 2
// Stub: int double(int number) {return 0;}
// Sample output:
//      double(1) >> 2
//      double(2) >> 4
//      double(3) >> 6
// Template:
//      int double(int number)
//      {
//          return ...number;
//      }
int doubl(int number)
{
    return (2 * number);
}

int main()
{
    std::cout << "double(2) : " << doubl(2) << std::endl;
    std::cout << "test the function(should return true): ";
    test_function(doubl(2), 4);
    std::cout << std::endl;

    return 0;
}
