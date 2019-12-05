#!/usr/bin/python
import itertools

class Error(Exception):
    """Base class for exceptions in this module"""
    pass

class EndOfComputation(Error):
    """Raised when computation completes. Result in result attribute"""
    pass

class CPU():

    MAX_OPERANDS = 4
    
    def getParam(self, p):
        if self.modes[p] == '0':
            return self.memory[self.memory[self.ip + (p+1)]]
        else:
            return self.memory[self.ip + (p+1)]

    def setParam(self, p, val):
        self.memory[self.memory[self.ip + (p+1)]] = val
        
    def _add(self):
        in_a = self.getParam(0)
        in_b = self.getParam(1)
        self.setParam(2, in_a + in_b)
        self.ip += 4

    def _mul(self):
        in_a = self.getParam(0)
        in_b = self.getParam(1)
        self.setParam(2, in_a * in_b)
        self.ip += 4

    def _inp(self):
        self.setParam(0, next(self.getInput))
        self.ip += 2

    def _out(self):
        output = self.getParam(0)
        self.sendOutput(output)
        self.ip += 2

    def _jit(self):
        if self.getParam(0):
            self.ip = self.getParam(1)
        else:
            self.ip += 3

    def _jif(self):
        if self.getParam(0) == 0:
            self.ip = self.getParam(1)
        else:
            self.ip += 3

    def _lt(self):
        if self.getParam(0) < self.getParam(1):
            self.setParam(2, 1)
        else:
            self.setParam(2, 0)
        self.ip += 4

    def _eq(self):
        if self.getParam(0) == self.getParam(1):
            self.setParam(2,1)
        else:
            self.setParam(2,0)
        self.ip += 4
            
    def _end(self):
        raise EndOfComputation()
    
    def _decodeOp(self):
        complexOp = self.memory[self.ip]
        self.op    = self.ops[complexOp % 100]
        self.modes = (str(complexOp // 100))[::-1]
        self.modes = self.modes + '0' * (CPU.MAX_OPERANDS - len(self.modes))
        
    def __init__(self, instructions, inputter=None, outputter=None):
        self.origMemory = list(instructions)
        self.memory = list(instructions)
        self.ops = { 1: self._add,
                     2: self._mul,
                     3: self._inp,
                     4: self._out,
                     5: self._jit,
                     6: self._jif,
                     7: self._lt,
                     8: self._eq,
                     99: self._end}

        self.ip = 0
        self.getInput  = itertools.repeat(0)
        self.sendOutput = lambda x : None
        self.modes = '0' * CPU.MAX_OPERANDS
        self.op = self._end

        if inputter != None:
            self.getInput = inputter()
        if outputter != None:
            self.sendOutput = outputter

    def reset(self):
        self.memory = list(self.origMemory)
        self.ip = 0
        
    def run(self):
        try:
            while True:
                self._decodeOp()
                self.op()
        except EndOfComputation:
            return
