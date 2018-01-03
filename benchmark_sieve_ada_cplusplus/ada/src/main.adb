--
--Main program file - main.adb
--

with Ada.Text_IO; use Ada.Text_IO;
with Prime_Sieve; use Prime_Sieve;
with Ada.Real_Time; use Ada.Real_Time;

-- Times the execution of the Eratosthenes Sieve benchmark
procedure Main is
   Size : Integer := 10000;
   Primes : PrimeList(1..Size);
   Start_Time, End_Time : Time;
   Elapsed_Milliseconds : Float;
   Trials : Integer := 50000;
begin
   Put_Line("Brady Coles - Lab Assignment #4");

   Start_Time := Ada.Real_Time.Clock;
   -- Actual benchmark
   for i in 1..Trials loop
      Sieve(Primes, Size);
   end loop;


   End_Time := Ada.Real_Time.Clock;
   Elapsed_Milliseconds := Float( To_Duration( End_Time - Start_Time ) ) * 1000.0;

   -- Print primes to check validity
   for i in 2..Size loop
      if not Primes(i) then
         Put(Integer'Image(i));
      end if;
   end loop;

   Put_Line("");
   Put_Line(Float'Image(Elapsed_Milliseconds) & "ms");
   Put_Line(Float'Image(Elapsed_Milliseconds / Float(Trials)) & "ms per trial");

end Main;
