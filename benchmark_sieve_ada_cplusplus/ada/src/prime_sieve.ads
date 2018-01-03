--
-- Author: Brady Coles
-- Lab Assignment #4
-- Sieve of Eratosthenes Benchmark
--
-- Spec file - prime_sieve.ads
--

package Prime_Sieve is
   type PrimeList is array(Positive range <>) of Boolean;
   procedure Sieve (Primes : IN OUT PrimeList;
                      Size : Integer);
end Prime_Sieve;
