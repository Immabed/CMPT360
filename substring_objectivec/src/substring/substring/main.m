/*
 * Author: Brady Coles
 * Substring finding program using Knuth-Morris-Pratt algorithm.
 */

#import <Foundation/Foundation.h>

/* A class for finding substrings within a string. Uses the Knuth-Morris-Pratt
 algorithm, which preprocesses the substring to enable faster searching of the
 string. */
@interface KMPSubstring : NSObject
/* Returns the first index (starting from zero) in string where a full
 match of patter was found. If no match found, returns -1 */
- (int) firstMatchOf:(NSString *) pattern inString:(NSString *) string;
/* Simple wrapper of firstMatchOf that checks if a match was found */
- (BOOL) contains:(NSString *) pattern inString:(NSString *) string;

@end

/* Extends KMPSubstring, but without exposing the additional members */
@interface KMPSubstring()
/* Creates the partial match table of the 'pattern'required to find a substring 
 with firstMatchOf. This is what allows the KMP algorithm to be efficient. */
- (int *) partialTable:(NSString *) pattern ;
@end

/* Implements the methods in KMPSubstring and the extension. */
@implementation KMPSubstring

/* Finds the first match of 'pattern' in 'string'. Gets a partial match table from
 partialTable. Running time, excluding the call to partialTable is O(n) where n is
 the length of the string being searched. */
- (int) firstMatchOf:(NSString *)pattern inString:(NSString *)string {
    /* If the pattern is longer than the string or the pattern is empty, there
     can be no matches. */
    if (pattern.length > string.length || pattern.length == 0)
        return -1;
    
    /* The location (in string) of the start of the currently tested match */
    int m = 0;
    /* The location (in string) of the current character being tested against */
    int i = 0;
    /* Partial match table for pattern */
    int *t = [self partialTable:pattern];
    
    /* Begin algorithm */
    while (i < string.length && m <= string.length - pattern.length) {
        if ([string characterAtIndex:i] == [pattern characterAtIndex:(i - m)]) {
            /* Current character matches character in pattern, go to next character
             or return position of match if whole pattern checked */
            if ((i - m + 1) == pattern.length) {
                return m;
            }
            i++;
        }
        else if (m == i) {
            /* Current character not a match, and first character in pattern being 
             tested, simply try starting at next character */
            m++;
            i++;
        }
        else {
            /* Had found partial match, use partial match table to find next possible 
             location for a match to start. Testing can stay at current index, since 
             all prior characters in pattern must be true according to the partial 
             match table. */
            m = i - t[i - m];
        }
    }
    /* Match never found, whole string searched. Return -1 to indicate no match. */
    return -1;
}

/* Simply checks if firstMatchOf returns a valid index, returning true, otherwise
 no match was found, and so returns false. Note that no information is stored 
 between runs, so call firstMatchOf if you want the index of a match, and check 
 that the return value is a valid index (ie, -1 is invalid and indicates no match). 
 Calling contains before firstMatchOf runs algorithm twice. */
- (BOOL) contains:(NSString *)pattern inString:(NSString *)string {
    return ([self firstMatchOf:pattern inString: string] >= 0);
}

/* Creates a partial match table of the pattern. Indicates how many previous 
 characters are a prefix of the pattern, allowing the search algorithm to know 
 exactly where the next possible match could begin, based on how much of pattern 
 had already been matched. */
- (int *) partialTable:(NSString *) pattern {
    /* An empty pattern can't be matched. */
    if (pattern.length == 0) {
        return NULL;
    }
    /* A single character is a special case, since only one of two
     specially initialized entries can exists */
    int *table = malloc(pattern.length * sizeof(int));
    if (pattern.length == 1) {
        table[0] = -1;
    }
    /* in order to maintain the correctness of the algorithm when checking 
     partial matches at the beginning of 'pattern', the first and second entries 
     are always -1 and 0. */
    else {
        table[0] = -1;
        table[1] = 0;
    }
    /* The current index in 'pattern' being calculated for the table */
    int i = 2;
    /* The index of the next character to check to continue a partial match */
    int m = 0;
    
    while (i < pattern.length) {
        if ([pattern characterAtIndex:(i-1)] == [pattern characterAtIndex:m]) {
            /* previous character continues a partial match */
            /* Show number of characters in partial match ending at previous character */
            table[i] = m + 1;
            /* Set next character for partial match to next character in 'pattern' */
            m++;
            /* Move to next entry in table and next character in 'pattern'*/
            i++;
        }
        else if (m > 0) {
            /* Match not found, but partial match exists, so change to next 
             possible partial match containing that character and try again.
             Next attempt may have m = 0 */
            m = table[m];
        }
        else {
            /* Match not found, and m = 0, so no more possible partial matches 
             including that character, move on. */
            table[i] = 0;
            i++;
        }
    }
    return table;
}

@end


// Main program. Used to test KMPSubstring.
int main(int argc, const char * argv[]) {
    @autoreleasepool {
        NSLog(@"Substring finder using Knuth-Morris-Pratt Algorithm");
        char str[100]; // Temporary container for input.
        KMPSubstring *s = [[KMPSubstring alloc] init];
        
        /* Repeat until stopped by user */
        while (true) {
            /* Get input */
            NSLog(@"Enter a string to find a substring in:");
            gets(str); // Unsafe
            NSString *string = [NSString stringWithUTF8String:str];
            NSLog(@"Enter a pattern to search for");
            gets(str); // Unsafe
            NSString *pattern = [NSString stringWithUTF8String:str];
            /* Result */
            int index = [s firstMatchOf:pattern inString:string];
            NSLog(index >= 0 ?
                  [NSString stringWithFormat:@"pattern '%@' found in '%@' at index %d",
                    pattern, string, index]
                  :[NSString stringWithFormat:@"pattern '%@' not found in '%@'",
                    pattern, string]);
            /* Check is user is done */
            NSLog(@"Would you like to search another string? (y/n)");
            gets(str); // Unsafe
            NSString *answer = [NSString stringWithUTF8String:str];
            if ([s firstMatchOf:@"y" inString: answer] != 0) {
                NSLog(@"Goodbye");
                break;
            }
        }
    }
    return 0;
}