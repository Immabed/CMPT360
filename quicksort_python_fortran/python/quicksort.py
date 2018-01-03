#!/usr/env/bin python3

# Author: Brady Coles
# Quicksort Program
import os.path
import random
import sys

# Reads a file, and takes a float from each line and adds it to a list.
# Returns the list.
def readItemsFromFile(fileName):
    if not os.path.isfile(fileName):
        print("File not found: " + fileName)
        return [];
    f = open(fileName, 'r')
    array = []
    for line in f.readlines(): 
        try:
            array.append(float(line))
        except ValueError:
            print("Input was unreadable on a line. Skipped")
    f.close()
    print("Array read from file")
    return array

# Quicksort using Lomuto partitioning (take last item as pivot).
def quicksort(a):
    # Base case, 1 or 0 elements to sort
    if len(a) <= 1:
        return a;
    pivot = a[len(a) - 1]
    split = 0
    for i in range(len(a) - 1):
        if a[i] <= pivot:
            a[i], a[split] = a[split], a[i]
            split += 1
    a[split], a[len(a) - 1] = pivot, a[split]
    # Recursive call
    a[:split] = quicksort(a[:split])
    a[split+1:] = quicksort(a[split+1:])
    return a
    
# Checks to see if the elements in an array are sorted in
# increasing order.    
def checkSorted(a):
    for i in range(1, len(a)):
        if a[i] < a[i-1]:
            return False
    return True
    
# Creates a file with name filename and puts each element
# of array on a line.
def printArrayToFile(a, filename):
    f = open(filename, 'w')
    for i in a:
        f.write(str(i) + '\n')
    print("Write finished.")
    
# Create a file with random, uniformly distributed floats between
# -10000 and 10000.
# length is number of items, name is file name.
def createRandomFile(length, name):
    f = open(name, 'w')
    for i in range(length):
        f.write('' + str(random.uniform(-10000, 10000)) + '\n')
    f.close()
    print("Done create file")

# Run a sort on an input file and save to an output file.
def main():
    if (len(sys.argv) == 3):
        inFile = sys.argv[1]
        outFile = sys.argv[2]
        array = quicksort(readItemsFromFile(inFile))
        if checkSorted(array):
            print("Array is sorted.")
        else:
            print("Array not sorted.")
        printArrayToFile(array, outFile)
    else:
        print("Invalid arguments. Enter an input and output file:\nHW2.py <inFile> <outFile>") 

# Only run main if this is the main program
# If this module is imported, this does not run. 
if __name__ == "__main__":
    main()
