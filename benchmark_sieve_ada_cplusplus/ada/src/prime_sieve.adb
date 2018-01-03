--
-- Body file - prime_sieve.adb
--

package body Prime_Sieve is
   procedure Sieve (Primes : IN OUT PrimeList;
                    Size : Integer) is
      j : Integer;
   begin
      for i in 2..Size loop
         if not Primes(i) then
            j := i + i ;
            while j <= Size loop
               Primes(j) := True;
               j := j + i;
            end loop;
         end if;
      end loop;
   end Sieve;

end Prime_Sieve;
