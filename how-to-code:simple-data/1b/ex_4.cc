#include <iostream>

// Test function
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

class Image
{
  public:
    Image(int width, int height) : height_(height), width_(width)
    {
        std::cout << "Image created" << std::endl;
    }

    ~Image()
    {
    }

    void setWidth(int new_width)
    {
        width_ = new_width;
    }

    void setHeight(int new_height)
    {
        height_ = new_height;
    }

    int getWidth() const
    {
        return width_;
    }

    int getHeight() const
    {
        return height_;
    }

  private:
    int height_;
    int width_;
};

// Image -> Boolean
// produces true if image is tall (heigh > width)
bool isImageTall(Image img)
{
    return (img.getHeight() > img.getWidth());
}

// bool isImageTall(Image img)   //stub
//{
//    return false;
//}

// bool isImageTall(Image img)   //template
//{
//    ... img
//}

int main()
{
    Image Image1(10, 15);
    Image Image2(15, 10);
    Image Image3(15, 15);

    test_function(isImageTall(Image1), true);
    test_function(isImageTall(Image2), false);
    test_function(isImageTall(Image3), false);

    return 0;
}
