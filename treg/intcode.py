#!/usr/bin/python3

class Error(Exception):
    """Base class for exceptions in this module"""
    pass

class EndOfComputation(Error):
    """Raised when computation completes. Result in result attribute"""
    def __init__(self, result):
        self.result = result

def add(ins, i):
    ins[ins[i+3]] = ins[ins[i+1]] + ins[ins[i+2]]
    return 4

def mul(ins, i):
    ins[ins[i+3]] = ins[ins[i+1]] * ins[ins[i+2]]
    return 4

def end(ins, i):
    raise EndOfComputation(ins[0])

ops = { 1: add,
        2: mul,
       99: end }

def runComputer(ins, ip=0):
    try:
        while True:
            oper = ins[ip]
            ip += ops[oper](ins, ip)
    except EndOfComputation as eoc:
        return eoc.result
