/*
 * Author: Brady Coles
 * Lab Assignment #4
 * Sieve of Eratosthenes Benchmark
 */

#include<iostream>
#include<chrono>
using namespace std;
using namespace chrono;

// Calculate primes up to size, leaving primes as false in primes[]
void sieve(bool primes[], int size) {
    for (int i = 2; i < size; i++) {
        if (!(primes[i])) {
            for (int j = i + i; j < size; j += i) {
                primes[j] = true;
            }
        }
    }
}

// Times the execution of the Eratosthenes Sieve benchmark
int main() {
    
    int const limit = 10000;
    int const trials = 50000;
    
    bool p[limit] = {};
    steady_clock::time_point begin = steady_clock::now();
    for (int i = 0; i < trials; i++) {
        sieve(p, limit);
    }
    steady_clock::time_point end = steady_clock::now();
    
    for (int i = 2; i < limit; i++) {
        if (!(p[i])) cout << i << " ";
    }
    double time = duration_cast<microseconds>(end - begin).count() * 0.001;
    cout << endl << "Time taken = " << time << "ms" << endl;
    cout << "Per trial = " << time / trials << "ms" << endl;
}
