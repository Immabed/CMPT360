using System;

/// <summary>
/// Author: Brady Coles
/// This class allows the use of complex numbers (defined below) in the determining
/// of the sidelengths, area, and angles of a triangle on the Complex Plane.
/// </summary>
static class ComplexTriangle
{
    // For identification

    /// <summary>
    /// Takes three points on the complex plane, and determines the side lengths
    /// and angles of the triangle formed by them.
    /// </summary>
    /// <param name="z1">Point 1</param>
    /// <param name="z2">Point 2</param>
    /// <param name="z3">Point 3</param>
    public static void Triangle (ComplexNumber z1, ComplexNumber z2, ComplexNumber z3)
    {
        Console.WriteLine(string.Format("Triangle \n Z1: {0} \n Z2: {1} \n Z3: {2}",
            z1.ToString(), z2.ToString(), z3.ToString()));
        // Check Validity
        if (z1.Equals(z2) || z1.Equals(z3) || z2.Equals(z3))
            // Check for non-distinct numbers
        {
            Console.WriteLine("Not a valid triangle. Points must be distinct");
            return;
        }
        if (Area(z1, z2, z3) == 0)
            // Check for collinearity
        {
            Console.WriteLine("Not a valid triangle. Points must not be collinear.");
            return;
        }

        var z1z2 = Distance(z1, z2);
        var z2z3 = Distance(z2, z3);
        var z3z1 = Distance(z3, z1);

        var angZ1 = Angle(z2z3, z1z2, z3z1);
        var angZ2 = Angle(z3z1, z1z2, z2z3);
        var angZ3 = Angle(z1z2, z2z3, z3z1);
            
        Console.WriteLine("Side Lengths:");
        Console.WriteLine(string.Format(" Z1 Z2 : {0}", z1z2));
        Console.WriteLine(string.Format(" Z2 Z3 : {0}", z2z3));
        Console.WriteLine(string.Format(" Z3 Z1 : {0}", z3z1));
        Console.WriteLine("Angles (degrees)");
        Console.WriteLine(string.Format(" Z1 : {0}", angZ1));
        Console.WriteLine(string.Format(" Z2 : {0}", angZ2));
        Console.WriteLine(string.Format(" Z3 : {0}", angZ3));
    }

    /// <summary>
    /// Determines the distance between two complex numbers
    /// </summary>
    /// <param name="z1">Number 1</param>
    /// <param name="z2">Number 2</param>
    /// <returns>Distance between numbers</returns>
    private static double Distance(ComplexNumber z1, ComplexNumber z2)
    {
        ComplexNumber z = new ComplexNumber(z1.r - z2.r, z1.i - z2.i);
        return Math.Pow((z.r * z.r) + (z.i * z.i), 0.5);
    }

    /// <summary>
    /// Determines the interior angle opposite a side of a triangle.
    /// </summary>
    /// <param name="opposite">Side opposite the angle</param>
    /// <param name="adjacent1">Adjacent side</param>
    /// <param name="adjacent2">Another adjacent side</param>
    /// <returns>The angle in degrees</returns>
    private static double Angle(double opposite, double adjacent1, double adjacent2)
    {
        return Math.Acos(
                ((adjacent1 * adjacent1) + (adjacent2 * adjacent2) - (opposite * opposite)) 
                / (2 * adjacent1 * adjacent2)
            )
            * (360 / (2 * Math.PI));
    }

    /// <summary>
    /// Determines the area of a triangle in the complex plane.
    /// </summary>
    /// <param name="z1"></param>
    /// <param name="z2"></param>
    /// <param name="z3"></param>
    /// <returns>Area of the triangle</returns>
    private static double Area (ComplexNumber z1, ComplexNumber z2, ComplexNumber z3)
    {
        // Uses shoelace formula to calculate area
        return (z1.r * (z2.i + z3.i) + z2.r * (z1.i + z3.i) + z3.r * (z1.i + z2.i)) / 2;
    }

}

/// <summary>
/// Simple complex number. Does not include operators.
/// </summary>
public struct ComplexNumber
{
    public double r;
    public double i;

    // Default constructor requires real and imaginary parts of the complex number
    public ComplexNumber(double real, double imaginary)
    {
        r = real;
        i = imaginary;
    }

    // Returns a nicely formatted string in the form a+bi
    public override string ToString()
    {
        return string.Format("{0:0.#####}{1:+0.#####;-0.#####}i", r, i);
    }
}

