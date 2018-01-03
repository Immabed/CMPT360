using System;

// Used to test the ComplexTriangle class.
class Test
{
    public static void Main(string[] args)
    {
        // These values can be changed to try different triangles.
        var z1 = new ComplexNumber(1, 2);
        var z2 = new ComplexNumber(0, 0);
        var z3 = new ComplexNumber(13.5, -3);

        ComplexTriangle.Triangle(z1, z2, z3);
        Console.ReadLine();
    }
}
