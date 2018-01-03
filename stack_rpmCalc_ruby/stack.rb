#!/usr/bin/ruby -w

# Author: Brady Coles
# Lab Assignment # 3
# ADT: Reverse Polish Notation Calculator implemented with a Stack

# Generic stack. Utilizes associative array to allow any type elements in stack
class Stack
    
    # Initialize array to hold elements in stack
    def initialize
        @stack = Array.new
    end
    
    # Return number of items in stack
    def length
        return @stack.length
    end
    
    # True if stack has no elements, false otherwise.
    def is_empty?
        return @stack.length == 0
    end
    
    # Push an item onto the stack.
    def push(item)
        @stack[@stack.length]=item
    end
    
    # Get top item on stack without removing it.
    def peek
        if @stack.length <= 0
            #ERROR
        else
            return @stack[@stack.length - 1]
        end
    end
    
    # Remove top item on stack, returns top item.
    def pop
        if @stack.length <= 0
            #ERROR
        else
            return @stack.delete_at(@stack.length - 1)
        end
    end
end

# Module for parsing strings in RPN and for calculating value of RPN statements
module RPNCalc

    # CONSTANTS
    ADD_OP = "+"
    MUL_OP = "*"
    SUB_OP = "-"
    DIV_OP = "/"
    BIN_OPS = [ADD_OP, MUL_OP, SUB_OP, DIV_OP]
    SEP = " "
    DEC_SEP = "."
    DIGITS = ["0", "1", "2", "3", "4", "5", "6", "7", "8", "9"]
    NUMERIC = DIGITS + [DEC_SEP]
    
    # Parses a string in RPN into lexemes, puts lexemes into a stack, which is returned
    # Parser is not strict on whitespace. If two characters can never be together
    # in a lexeme, then it assumes they are in separate lexemes.
    # eg. '3-5/' is the same as '3 -5 /', or in standard notation '3 / (-5)'
    # Interpets decimal points as numeric, but checks that no more than one decimal point
    # is in each numeric lexeme. 
    # Converts numeric lexemes into floating point type, regardless of whether the lexeme
    # had a decimal point or not. 
    def parse(str)
        lexeme = ""
        st = Stack.new
        isnumeric = false
        # Iterate over each character from the left
        str.each_char do |symbol|
            # If the current lexeme is not empty and the current symbol is not numeric
            # then the current lexeme must be complete.
            if !(NUMERIC.include? symbol) && lexeme != ""
                # If the current lexeme is numeric, convert it to a float
                if isnumeric
                    # Ensure lexeme has no more than one decimal point, else stop parsing 
                    # and return nil
                    if lexeme.count(DEC_SEP) == 1 || !lexeme.include?(DEC_SEP)
                        lexeme = lexeme.to_f
                    else
                        puts "Invalid numeric literal: " + lexeme
                        return
                    end
                end
                # Push lexeme onto stack and reset for next lexeme
                st.push(lexeme)
                isnumeric = false
                lexeme = "" 
            end
            # Check what next symbol is
            case symbol
                # Separators are used in the above selection, so are skipped
                when SEP
                    next
                # Plus and minus symbols can be operators or sign a numeric, so add to lexeme
                when ADD_OP, SUB_OP
                    lexeme = symbol
                # Mul and div ops are always there own lexeme, push to stack.
                when MUL_OP, DIV_OP
                    st.push(symbol)
                # Digits and decimal points get added to current lexeme and set flag 
                # isnumeric for the above selection
                when *NUMERIC
                    isnumeric = true
                    lexeme += symbol
                # If symbol is not recognized, stop parsing and return nil
                else
                    puts "Unknown symbol: " + symbol 
                    return
            end
        end
        # Push last lexeme. Must be an operator unless only a numeric was passed.
        if lexeme != ""
            if isnumeric
                # Ensure lexeme has no more than one decimal point, else stop parsing 
                # and return nil
                if lexeme.count(DEC_SEP) == 1 || !lexeme.include?(DEC_SEP)
                    lexeme = lexeme.to_f
                else
                    puts "Invalid numeric literal: " + lexeme
                    return
                end
            end
            st.push(lexeme)
        end
        
        # Return the stack
        return st
    end
    
    # Calculate an RPN expression from a stack
    def calculate(st)
        # if the stack is nil, do nothing
        if st != nil
            val = statement(st)
            # If the stack is not empty after running statement, then the stack
            # is not valid a valid RPN statement.
            return val if st.is_empty?
            puts "Error in input"
            return
        end
    end
    
    #Remaining functions are only accessible indirectly from calculate
    private
    # Return value of an RPN statement from a stack
    def statement(st)
        # If the stack is empty, then the stack is invalid, even if this occurs
        # in a recursive call.
        if st.respond_to?(:is_empty?) && st.is_empty?
            puts "Error in input"
            return
        end
        
        # If the next lexeme is an operator, decode as a binary operator statement
        if BIN_OPS.include? st.peek
            val =  bin_statement(st)
        # If the next lexeme is not an operator, it is a numeric, so return it as is.
        else
            return st.pop
        end
    end

    # Return value of a binary operator RPN statement, where there are two statements and 
    # a binary operator.
    def bin_statement(st)
        # If the stack is empty, then the stack is invalid, even if this occurs
        # in a recursive call.
        if st.respond_to?(:is_empty?) && st.is_empty?
            puts "Error in input"
            return
        end
        
        # Since bin_statement was called, the next lexeme should be an operator
        if BIN_OPS.include? st.peek
            case st.pop
                # Multiplication
                when MUL_OP
                    val2 = statement(st)
                    val1 = statement(st)
                    return val1 * val2 if !([val1, val2].include? nil)
                    return
                # Division. Is not commutative, so values ordered properly.
                when DIV_OP
                    val2 = statement(st)
                    val1 = statement(st)
                    return val1 / val2 if !([val1, val2].include? nil)
                    return
                # Addition
                when ADD_OP
                    val2 = statement(st)
                    val1 = statement(st)
                    return val1 + val2 if !([val1, val2].include? nil)
                    return
                # Subtraction. Is not commutative, so values ordered properly.
                when SUB_OP
                    val2 = statement(st)
                    val1= statement(st)
                    return val1 - val2 if !([val1, val2].include? nil)
                    return
                # If definition of BIN_OPS is changed, this error may occur
                else
                    puts "ERROR - Unkown binary operator in stack"
                    return
            end
        else
            # If function called when it shouldn't have been, this error may occur
            puts "ERROR - bin_statement called without a binary operator in stack"
            return
        end
    end
end

# Program itself. Uses RPNCalc to return answers.
if __FILE__ == $0
    include RPNCalc
    puts "Enter calculations in Reverse Polish Notation"
    puts "Valid operators: + - / *"
    puts "Enter 'exit' to end program"
    
    while true do
        print ">> "
        input = gets.chomp
        if input == "exit"
            puts "Goodbye"
            break
        end
        val = calculate(parse(input))
        puts val if val != nil
    end
end
    
