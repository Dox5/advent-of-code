#!/usr/bin/python
import itertools
from collections import defaultdict

class Error(Exception):
    """Base class for exceptions in this module"""
    pass

class EndOfComputation(Error):
    """Raised when computation completes. Result in result attribute"""
    pass

class AwaitingInput(Error):
    """Raised when Computation needs more data but it isn't there yet"""
    pass

class UnexpectedMode(Error):
    """Thrown when the mode confuses us"""
    pass

class CPU():

    MAX_OPERANDS = 4
    
    def getParam(self, p):
        if self.modes[p] == '0':
            return self.memory[self.memory[self.ip + (p+1)]]
        elif self.modes[p] == '1':
            return self.memory[self.ip + (p+1)]
        else:
            return self.memory[self.memory[self.ip + (p+1)] + self.relbase]

    def setParam(self, p, val):
        if self.modes[p] == '0':
            self.memory[self.memory[self.ip + (p+1)]] = val
        elif self.modes[p] == '2':
            self.memory[self.memory[self.ip + (p+1)] + self.relbase] = val
        else:
            raise UnexpectedMode()
        
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
        value = next(self.getInput)
        if value is None:
            raise AwaitingInput()
        
        self.setParam(0, value)
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

    def _base(self):
        self.relbase += self.getParam(0)
        self.ip += 2
        
    def _end(self):
        raise EndOfComputation()
    
    def _decodeOp(self):
        complexOp = self.memory[self.ip]
        self.op    = self.ops[complexOp % 100]
        self.modes = (str(complexOp // 100))[::-1]
        self.modes = self.modes + '0' * (CPU.MAX_OPERANDS - len(self.modes))

    def _loadProgram(self):
        self.memory = defaultdict(int, {pos : instruction for (pos,instruction) in enumerate(self.origMemory)})
         
    def __init__(self, instructions, inputter=None, outputter=None):
        self.origMemory = list(instructions)
        self._loadProgram()
        
        self.ops = { 1: self._add,
                     2: self._mul,
                     3: self._inp,
                     4: self._out,
                     5: self._jit,
                     6: self._jif,
                     7: self._lt,
                     8: self._eq,
                     9: self._base,
                     99: self._end}

        self.ip = 0
        self.relbase = 0
        self.getInput  = itertools.repeat(0)
        self.sendOutput = lambda x : None
        self.modes = '0' * CPU.MAX_OPERANDS
        self.op = self._end
        self.isFinished = False
        
        if inputter != None:
            self.getInput = inputter()
        if outputter != None:
            self.sendOutput = outputter

    def reset(self):
        self._loadProgram()
        self.ip = 0
        self.isFinished = False
        
    def finished(self):
        return self.isFinished
        
    def run(self):
        try:
            while True:
                self._decodeOp()
                self.op()
        except EndOfComputation:
            self.isFinished = True
            return
        except AwaitingInput:
            return
