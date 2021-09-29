#include <iostream>

// unsigned int -> unsigned int
// provided with the length of a side, produces the area of the created square
// unsigned int area(unsigned int length) { return 0; } //stub
// area(2) >> 4
// area(12) >> 144
// unsigned int area (unsigned int length) { ... length }  // template
unsigned int area(unsigned int len)
{
    return (len * len);
}

int main()
{
    unsigned int len{0};
    std::cout << "Please enter the length the side of the square to get the area: " << std::endl;
    std::cin >> len;

    std::cout << "Area of a " << len << "x" << len << " square: " << area(len) << std::endl;
    return 0;
}
