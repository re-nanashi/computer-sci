#include <iostream>
#include <string>

// Problem: pluralize a word
// (string) >> string
// pluralizes the input string
// std::string pluralize(std::string) {return 'a';}
// pluralize(give) >> gives
// pluralize(apple) >> apples
// std::string pluralize(std::string input_string)
// {
//      ... input_string    ;add 's' to the end of every string
// }

std::string pluralize(std::string input_string)
{
    return (input_string + 's');
}

int main()
{
    std::string user_input{""}, result_string{""};
    std::cout << "Please input the string/word to pluralize: " << std::endl;
    std::cin >> user_input;

    result_string = pluralize(user_input);

    std::cout << "User input: " << user_input << '\n';
    std::cout << "Pluralized input: " << result_string << '\n';

    return 0;
}
