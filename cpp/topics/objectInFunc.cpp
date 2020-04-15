#include <iostream>
using namespace std;
class Complex
{
private:
  int real;
  int imag;

public:
  Complex() : real(0), imag(0) {}
  void readData()
  {
    cout << "Enter real and imaginary number respectively:" << endl;
    cin >> real >> imag;
  }
  Complex addComplexNumbers(Complex comp2)
  {
    Complex temp;

    // real represents the real data of object c3 because this function is called using code c3.add(c1,c2);
    temp.real = real + comp2.real;

    // imag represents the imag data of object c3 because this function is called using code c3.add(c1,c2);
    temp.imag = imag + comp2.imag;
    return temp;
  }
  void displayData()
  {
    cout << "Sum = " << real << "+" << imag << "i";
  }
};

int main()
{
  Complex c1, c2, c3;

  c1.readData();
  c2.readData();

  c3 = c1.addComplexNumbers(c2);

  c3.displayData();

  return 0;
}